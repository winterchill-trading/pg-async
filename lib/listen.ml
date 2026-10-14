open! Core
open! Async

type t =
  { config : Config.t
  ; metrics : Metrics.t option
  ; reconnect : bool
  ; backoff : attempt:int -> Time_ns.Span.t
  ; on_reconnect : unit -> unit
  ; channels : String.Hash_set.t
  ; reader : Connection.Notification.t Pipe.Reader.t
  ; writer : Connection.Notification.t Pipe.Writer.t
  ; mutable conn : Connection.t option (* [None] while reconnecting *)
  ; mutable closed : bool
  ; mutable reconnects : int
  }

let quote_identifier s =
  "\"" ^ String.substr_replace_all s ~pattern:"\"" ~with_:"\"\"" ^ "\""
;;

let default_backoff ~attempt =
  Time_ns.Span.min
    (Time_ns.Span.of_int_sec 10)
    (Time_ns.Span.scale_int (Time_ns.Span.of_int_ms 100) (1 lsl Int.min 10 (attempt - 1)))
;;

(* LISTEN takes an identifier, which cannot be a bind parameter; [quote_identifier] makes
   any channel name a single quoted identifier. *)
let listen_sql channels =
  List.map channels ~f:(fun c -> "LISTEN " ^ quote_identifier c)
  |> String.concat ~sep:"; "
;;

let rec attach t conn =
  t.conn <- Some conn;
  Connection.Private.set_notification_handler conn (fun n ->
    Pipe.write_without_pushback_if_open t.writer n);
  upon (Connection.closed conn) (fun () -> lost t conn);
  Connection.Private.start_idle_watch conn

and lost t conn =
  match t.conn with
  | Some c when phys_equal c conn ->
    t.conn <- None;
    if t.closed || not t.reconnect
    then Pipe.close t.writer
    else don't_wait_for (reconnect t ~attempt:1)
  | Some _ | None -> ()

and reconnect t ~attempt =
  let%bind () = Clock_ns.after (t.backoff ~attempt) in
  if t.closed
  then return ()
  else (
    match%bind Connection.connect ?metrics:t.metrics t.config with
    | Error _ -> reconnect t ~attempt:(attempt + 1)
    | Ok conn ->
      let channels = Hash_set.to_list t.channels in
      let%bind listened =
        if List.is_empty channels
        then return (Ok ())
        else Connection.execute_script ~label:"listen" conn (listen_sql channels)
      in
      (match listened with
       | Error _ ->
         let%bind () = Connection.close conn in
         reconnect t ~attempt:(attempt + 1)
       | Ok () when t.closed -> Connection.close conn
       | Ok () ->
         t.reconnects <- t.reconnects + 1;
         attach t conn;
         t.on_reconnect ();
         return ()))
;;

let create
  ?metrics
  ?(reconnect = false)
  ?(backoff = default_backoff)
  ?(on_reconnect = ignore)
  config
  =
  match%map Connection.connect ?metrics config with
  | Error _ as e -> e
  | Ok conn ->
    let reader, writer = Pipe.create () in
    let t =
      { config
      ; metrics
      ; reconnect
      ; backoff
      ; on_reconnect
      ; channels = String.Hash_set.create ()
      ; reader
      ; writer
      ; conn = None
      ; closed = false
      ; reconnects = 0
      }
    in
    attach t conn;
    Ok t
;;

(* While reconnecting, a channel change is recorded and applied by the next session. *)
let apply t sql =
  match t.conn with
  | None -> return (if t.closed then Error Pg_error.Closed else Ok ())
  | Some conn ->
    (match%map Connection.execute_script ~label:"listen" conn sql with
     | Error (Pg_error.Closed | Connection_lost _) when t.reconnect && not t.closed ->
       Ok ()
     | r -> r)
;;

let listen t channel =
  if t.closed
  then return (Error Pg_error.Closed)
  else (
    let was_listening = Hash_set.mem t.channels channel in
    Hash_set.add t.channels channel;
    match%map apply t (listen_sql [ channel ]) with
    | Ok () -> Ok ()
    | Error _ as e ->
      if not was_listening then Hash_set.remove t.channels channel;
      e)
;;

let unlisten t channel =
  if t.closed
  then return (Error Pg_error.Closed)
  else (
    Hash_set.remove t.channels channel;
    apply t ("UNLISTEN " ^ quote_identifier channel))
;;

let channels t = Hash_set.to_list t.channels |> List.sort ~compare:String.compare
let reconnects t = t.reconnects
let notifications t = t.reader

let close t =
  t.closed <- true;
  Pipe.close t.writer;
  match t.conn with
  | None -> return ()
  | Some conn -> Connection.close conn
;;

let notify conn ~channel ~payload =
  Query.fetch_all
    ~label:"notify"
    conn
    "SELECT pg_notify($1, $2)"
    ~params:[ Param.string channel; Param.string payload ]
    ~row:(Row.return ())
  |> Deferred.Result.ignore_m
;;
