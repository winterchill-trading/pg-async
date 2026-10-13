open! Core
open! Async

module Stats = struct
  type t =
    { size : int
    ; idle : int
    ; waiting : int
    ; opened : int
    ; discarded : int
    }
  [@@deriving sexp_of]
end

type idle =
  { conn : Connection.t
  ; since : Time_ns.t
  }

type t =
  { config : Config.t
  ; metrics : Metrics.t option
  ; max_size : int
  ; acquire_timeout : Time_ns.Span.t
  ; health_check_after : Time_ns.Span.t
  ; max_idle_time : Time_ns.Span.t
  ; mutable idle : idle list (* most recently returned first *)
  ; mutable size : int
  ; waiters : (Connection.t, Pg_error.t) Result.t Ivar.t Queue.t
  ; mutable opened : int
  ; mutable discarded : int
  ; mutable closed : bool
  ; reaper_stop : unit Ivar.t
  }

let config t = t.config

let stats t =
  { Stats.size = t.size
  ; idle = List.length t.idle
  ; waiting = Queue.count t.waiters ~f:Ivar.is_empty
  ; opened = t.opened
  ; discarded = t.discarded
  }
;;

let discard t conn =
  t.size <- t.size - 1;
  t.discarded <- t.discarded + 1;
  don't_wait_for (Connection.close conn)
;;

let rec next_waiter t =
  match Queue.dequeue t.waiters with
  | None -> None
  | Some w when Ivar.is_full w -> next_waiter t (* timed out *)
  | Some w -> Some w
;;

let open_connection t =
  t.size <- t.size + 1;
  match%map Connection.connect ?metrics:t.metrics t.config with
  | Ok conn ->
    t.opened <- t.opened + 1;
    Ok conn
  | Error _ as e ->
    t.size <- t.size - 1;
    e
;;

(* Give [conn] to the first live waiter, or park it. *)
let rec hand_over t conn =
  if t.closed || Connection.is_closed conn
  then (
    discard t conn;
    refill t)
  else (
    match next_waiter t with
    | Some w -> Ivar.fill_exn w (Ok conn)
    | None -> t.idle <- { conn; since = Time_ns.now () } :: t.idle)

(* After a connection goes away, open a replacement if someone is waiting. *)
and refill t =
  if (not t.closed) && t.size < t.max_size
  then (
    match next_waiter t with
    | None -> ()
    | Some w ->
      don't_wait_for
        (match%map open_connection t with
         | Ok conn ->
           if Ivar.is_empty w then Ivar.fill_exn w (Ok conn) else hand_over t conn
         | Error e -> Ivar.fill_if_empty w (Error e)))
;;

let release t conn =
  if Connection.in_transaction conn && not (Connection.is_closed conn)
  then
    don't_wait_for
      (match%map Tx.rollback conn with
       | Ok () -> hand_over t conn
       | Error _ ->
         discard t conn;
         refill t)
  else hand_over t conn
;;

let healthy t { conn; since } =
  if Connection.is_closed conn
  then return false
  else if Time_ns.Span.( < ) (Time_ns.diff (Time_ns.now ()) since) t.health_check_after
  then return true
  else (
    match%map
      Query.execute
        ~label:"pool_health_check"
        ~timeout:(Time_ns.Span.of_int_sec 5)
        conn
        "SELECT 1"
    with
    | Ok (_ : int) -> true
    | Error _ -> false)
;;

let rec acquire t =
  if t.closed
  then return (Error Pg_error.Closed)
  else (
    match t.idle with
    | candidate :: rest ->
      t.idle <- rest;
      if%bind healthy t candidate
      then return (Ok candidate.conn)
      else (
        discard t candidate.conn;
        acquire t)
    | [] when t.size < t.max_size -> open_connection t
    | [] ->
      let w = Ivar.create () in
      Queue.enqueue t.waiters w;
      upon (Clock_ns.after t.acquire_timeout) (fun () ->
        Ivar.fill_if_empty w (Error (Pg_error.Timeout t.acquire_timeout)));
      Ivar.read w)
;;

let with_connection t ~f =
  match%bind acquire t with
  | Error _ as e -> return e
  | Ok conn ->
    Monitor.protect
      (fun () -> f conn)
      ~finally:(fun () ->
        release t conn;
        return ())
;;

let reap t =
  let now = Time_ns.now () in
  let keep, drop =
    List.partition_tf t.idle ~f:(fun { since; _ } ->
      Time_ns.Span.( < ) (Time_ns.diff now since) t.max_idle_time)
  in
  t.idle <- keep;
  List.iter drop ~f:(fun { conn; _ } -> discard t conn)
;;

let create
  ?metrics
  ?(max_size = 10)
  ?(acquire_timeout = Time_ns.Span.of_int_sec 30)
  ?(health_check_after = Time_ns.Span.of_int_sec 30)
  ?(max_idle_time = Time_ns.Span.of_int_sec 300)
  config
  =
  if max_size < 1
  then raise_s [%message "Pool.create: max_size must be positive" (max_size : int)];
  let t =
    { config
    ; metrics
    ; max_size
    ; acquire_timeout
    ; health_check_after
    ; max_idle_time
    ; idle = []
    ; size = 0
    ; waiters = Queue.create ()
    ; opened = 0
    ; discarded = 0
    ; closed = false
    ; reaper_stop = Ivar.create ()
    }
  in
  Clock_ns.every
    ~stop:(Ivar.read t.reaper_stop)
    (Time_ns.Span.max (Time_ns.Span.of_int_ms 100) (Time_ns.Span.( / ) max_idle_time 2.))
    (fun () -> reap t);
  t
;;

let close t =
  if not t.closed
  then (
    t.closed <- true;
    Ivar.fill_if_empty t.reaper_stop ();
    Queue.iter t.waiters ~f:(fun w -> Ivar.fill_if_empty w (Error Pg_error.Closed));
    Queue.clear t.waiters);
  let idle = t.idle in
  t.idle <- [];
  t.size <- t.size - List.length idle;
  Deferred.List.iter ~how:`Parallel idle ~f:(fun { conn; _ } -> Connection.close conn)
;;

let with_transaction ?isolation ?read_only ?max_attempts ?backoff ?commit_if t ~f =
  with_connection t ~f:(fun conn ->
    Tx.with_transaction ?isolation ?read_only ?max_attempts ?backoff ?commit_if conn ~f)
;;

let execute ?label ?timeout ?params t sql =
  with_connection t ~f:(fun conn -> Query.execute ?label ?timeout ?params conn sql)
;;

let fetch_all ?label ?timeout ?params t sql ~row =
  with_connection t ~f:(fun conn -> Query.fetch_all ?label ?timeout ?params conn sql ~row)
;;

let fetch_one ?label ?timeout ?params t sql ~row =
  with_connection t ~f:(fun conn -> Query.fetch_one ?label ?timeout ?params conn sql ~row)
;;

let fetch_opt ?label ?timeout ?params t sql ~row =
  with_connection t ~f:(fun conn -> Query.fetch_opt ?label ?timeout ?params conn sql ~row)
;;
