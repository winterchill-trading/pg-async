open! Core
open! Async

module Notification = struct
  type t =
    { channel : string
    ; payload : string
    ; pid : int
    }
  [@@deriving sexp_of, compare, equal]
end

type state =
  | Open
  | Closed

type t =
  { conn : Postgresql.connection
  ; fd : Fd.t
  ; config : Config.t
  ; metrics : Metrics.t option
  ; sequencer : unit Throttle.Sequencer.t
  ; backend_pid : int
  ; server_version : int * int * int
  ; mutable state : state
  ; closed : unit Ivar.t
  ; mutable in_transaction : bool
  ; mutable on_notification : Notification.t -> unit
  ; mutable idle_interrupt : unit Ivar.t option
  }

let config t = t.config
let metrics t = t.metrics
let backend_pid t = t.backend_pid
let server_version t = t.server_version
let in_transaction t = t.in_transaction
let closed t = Ivar.read t.closed

let is_closed t =
  match t.state with
  | Closed -> true
  | Open -> false
;;

(* libpq reports failures as [Postgresql.Error]; a call on a finished handle raises
   [Failure]. Both mean the session is unusable. *)
let describe_exn = function
  | Postgresql.Error e -> Postgresql.string_of_error e
  | exn -> Exn.to_string exn
;;

let nonempty s = if String.is_empty s then None else Some s

let server_error_of_result (r : Postgresql.result) : Pg_error.Server_error.t =
  let field f = nonempty (String.strip (r#error_field f)) in
  { severity = Option.value (field SEVERITY_NONLOCALIZED) ~default:"ERROR"
  ; sqlstate = Option.value (field SQLSTATE) ~default:""
  ; message = Option.value (field MESSAGE_PRIMARY) ~default:(String.strip r#error)
  ; detail = field MESSAGE_DETAIL
  ; hint = field MESSAGE_HINT
  ; schema = field SCHEMA_NAME
  ; table = field TABLE_NAME
  ; column = field COLUMN_NAME
  ; constraint_ = field CONSTRAINT_NAME
  }
;;

(* Wait until the socket is readable or writable (or both, [`Read_write]), the deadline
   passes, or the descriptor goes away. Both directions are watched with one interrupt so
   the losing watch is always torn down before returning; Async allows only one watch per
   direction at a time. *)
let wait_fd fd what ~timeout =
  let stop = Ivar.create () in
  let interrupt = Deferred.any_unit [ Ivar.read stop; timeout ] in
  let watch dir = Fd.interruptible_ready_to fd dir ~interrupt in
  let summarize = function
    | `Ready -> `Ready
    | `Interrupted -> `Interrupted
    | `Bad_fd | `Closed -> `Gone
  in
  match what with
  | (`Read | `Write) as dir ->
    let%map r = watch dir in
    (match summarize r with
     | `Interrupted -> `Timeout
     | (`Ready | `Gone) as r -> r)
  | `Read_write ->
    let r = watch `Read
    and w = watch `Write in
    let%bind () = Deferred.any_unit [ Deferred.ignore_m r; Deferred.ignore_m w ] in
    Ivar.fill_if_empty stop ();
    let%map r and w in
    (match summarize r, summarize w with
     | `Gone, _ | _, `Gone -> `Gone
     | `Ready, _ | _, `Ready -> `Ready
     | `Interrupted, `Interrupted -> `Timeout)
;;

let connect_handshake (config : Config.t) =
  let deadline = Time_ns.add (Time_ns.now ()) config.connect_timeout in
  let timeout = Clock_ns.at deadline in
  match
    Result.try_with (fun () ->
      new Postgresql.connection ~conninfo:(Config.to_conninfo config) ~startonly:true ())
  with
  | Error exn -> return (Error (Pg_error.Connect (describe_exn exn)))
  | Ok conn ->
    let fail msg =
      (try conn#finish with
       | _ -> ());
      return (Error (Pg_error.Connect msg))
    in
    (* The socket can change between polls (libpq tries each address, and retries without
       TLS under sslmode=prefer), so each wait gets a fresh Async [Fd] that is released,
       without closing the descriptor libpq owns, as soon as it fires. *)
    let wait dir =
      match Result.try_with (fun () -> conn#socket_descr) with
      | Error exn -> return (`Error (describe_exn exn))
      | Ok descr ->
        let fd = Fd.create (Socket `Active) descr (Info.of_string "pg-async connect") in
        let%bind r = wait_fd fd dir ~timeout in
        let%map () = Fd.close ~file_descriptor_handling:Do_not_close_file_descriptor fd in
        (match r with
         | `Ready -> `Ready
         | `Timeout -> `Timeout
         | `Gone -> `Error "socket closed during connect")
    in
    let rec poll (status : Postgresql.polling_status) =
      match status with
      | Polling_ok -> return (Ok conn)
      | Polling_failed -> fail (String.strip conn#error_message)
      | Polling_reading | Polling_writing ->
        let dir =
          match status with
          | Polling_reading -> `Read
          | _ -> `Write
        in
        (match%bind wait dir with
         | `Ready ->
           (match Result.try_with (fun () -> conn#connect_poll) with
            | Ok status -> poll status
            | Error exn -> fail (describe_exn exn))
         | `Timeout ->
           fail
             (sprintf
                "connect timed out after %s"
                (Time_ns.Span.to_string_hum config.connect_timeout))
         | `Error msg -> fail msg)
    in
    (* libpq's documented start state: behave as if the last poll asked to write. *)
    poll Polling_writing
;;

let drain_notifications t =
  let rec loop () =
    match t.conn#notifies with
    | None -> ()
    | Some { Postgresql.Notification.name; pid; extra } ->
      t.on_notification { channel = name; payload = extra; pid };
      loop ()
  in
  loop ()
;;

let shut_down t =
  match t.state with
  | Closed -> ()
  | Open ->
    t.state <- Closed;
    Option.iter t.idle_interrupt ~f:(fun i -> Ivar.fill_if_empty i ());
    (* Async must forget the descriptor before [PQfinish] closes it, or a new socket
       reusing the number would collide with the stale registration. *)
    don't_wait_for
      (let%map () =
         Fd.close ~file_descriptor_handling:Do_not_close_file_descriptor t.fd
       in
       (try t.conn#finish with
        | _ -> ());
       Ivar.fill_if_empty t.closed ())
;;

let close t =
  shut_down t;
  closed t
;;

let lost t exn =
  match t.state with
  | Closed -> Error Pg_error.Closed
  | Open ->
    let msg = describe_exn exn in
    shut_down t;
    Error (Pg_error.Connection_lost msg)
;;

let track_transaction t (r : Postgresql.result) =
  match r#cmd_status with
  | "BEGIN" | "START TRANSACTION" | "SAVEPOINT" -> t.in_transaction <- true
  | "COMMIT" | "ROLLBACK" | "PREPARE TRANSACTION" -> t.in_transaction <- false
  | _ -> ()
;;

(* Flush the output buffer. While libpq cannot write, the server may be waiting for us to
   read, so wait for either direction and consume input when readable. *)
let rec flush_all t ~timeout =
  match t.conn#flush with
  | Successful -> return `Done
  | Data_left_to_send ->
    (match%bind wait_fd t.fd `Read_write ~timeout with
     | `Ready ->
       t.conn#consume_input;
       drain_notifications t;
       flush_all t ~timeout
     | (`Timeout | `Gone) as r -> return r)
;;

(* Read results until [PQgetResult] returns NULL, or until a COPY IN result when
   [stop_at_copy_in]. *)
let collect t ~timeout ~stop_at_copy_in =
  let rec loop acc =
    t.conn#consume_input;
    drain_notifications t;
    if t.conn#is_busy
    then (
      match%bind wait_fd t.fd `Read ~timeout with
      | `Ready -> loop acc
      | `Timeout -> return (`Collect_timeout, List.rev acc)
      | `Gone -> return (`Gone, List.rev acc))
    else (
      match t.conn#get_result with
      | None -> return (`Done, List.rev acc)
      | Some r ->
        (match r#status with
         | Copy_in when stop_at_copy_in -> return (`Done, List.rev (r :: acc))
         | _ -> loop (r :: acc)))
  in
  loop []
;;

let interpret t (results : Postgresql.result list) =
  List.iter results ~f:(fun r ->
    match r#status with
    | Command_ok | Tuples_ok -> track_transaction t r
    | _ -> ());
  match
    List.find results ~f:(fun r ->
      match r#status with
      | Fatal_error | Nonfatal_error | Bad_response -> true
      | _ -> false)
  with
  | Some r ->
    (match r#status with
     | Bad_response -> Error (Pg_error.Protocol (String.strip r#error))
     | _ -> Error (Pg_error.Server (server_error_of_result r)))
  | None ->
    (match List.last results with
     | None -> Error (Pg_error.Protocol "no result")
     | Some r ->
       (match r#status with
        | Command_ok | Tuples_ok | Empty_query | Copy_in -> Ok r
        | status ->
          Error
            (Pg_error.Protocol
               (sprintf "unexpected result status %s" (Postgresql.result_status status)))))
;;

let cancel_grace = Time_ns.Span.of_int_sec 5

let rec connect ?metrics (config : Config.t) =
  match%bind connect_handshake config with
  | Error _ as e -> return e
  | Ok conn ->
    (match
       Result.try_with (fun () ->
         conn#set_nonblocking true;
         conn#set_notice_processor (fun (_ : string) -> ());
         let descr = conn#socket_descr in
         ( Fd.create (Socket `Active) descr (Info.of_string "pg-async")
         , conn#backend_pid
         , conn#server_version ))
     with
     | Error exn ->
       (try conn#finish with
        | _ -> ());
       return (Error (Pg_error.Connect (describe_exn exn)))
     | Ok (fd, backend_pid, server_version) ->
       let t =
         { conn
         ; fd
         ; config
         ; metrics
         ; sequencer = Throttle.Sequencer.create ()
         ; backend_pid
         ; server_version
         ; state = Open
         ; closed = Ivar.create ()
         ; in_transaction = false
         ; on_notification = ignore
         ; idle_interrupt = None
         }
       in
       let statement_timeout =
         match config.statement_timeout with
         | None -> ""
         | Some span ->
           sprintf
             "; SET statement_timeout = %d"
             (Int.max 1 (Float.iround_up_exn (Time_ns.Span.to_ms span)))
       in
       let setup =
         "SET TimeZone = 'UTC'; SET DateStyle = 'ISO, YMD'; SET bytea_output = 'hex'"
         ^ statement_timeout
       in
       (match%bind
          run_script_unmetered t setup ~timeout:(Some config.connect_timeout)
        with
        | Ok () -> return (Ok t)
        | Error e ->
          let%map () = close t in
          Error (Pg_error.Connect (Error.to_string_hum (Pg_error.to_error e)))))

(* Ask the server to cancel [t]'s running statement from a second session. The bindings
   take libpq's cancel handle when the connection starts, before the backend key is known,
   so [PQcancel] cannot be used with [PQconnectStart]. *)
and cancel_backend t =
  let config = { t.config with query_timeout = None; statement_timeout = None } in
  don't_wait_for
    (match%bind connect config with
     | Error _ -> return ()
     | Ok side ->
       let%bind (_ : _ Result.t) =
         run_exec
           side
           ~sql:"SELECT pg_cancel_backend($1)"
           ~param_types:[| 23 |]
           ~params:[| Some (Int.to_string t.backend_pid) |]
           ~deadline:(Some (Time_ns.add (Time_ns.now ()) cancel_grace))
           ~label:None
       in
       close side)

and with_deadline t ~deadline ~f =
  let timeout_of = function
    | None -> Deferred.never ()
    | Some d -> Clock_ns.at d
  in
  match t.state with
  | Closed -> return (Error Pg_error.Closed)
  | Open ->
    let expired =
      match deadline with
      | Some d -> Time_ns.( <= ) d (Time_ns.now ())
      | None -> false
    in
    if expired
    then return (Error (Pg_error.Timeout Time_ns.Span.zero))
    else (
      (* A deadline passing while results are awaited cancels the statement and gives the
         server [cancel_grace] to answer; if it does not, or if the deadline passes while
         still sending, the session is abandoned. *)
      let finish ~cancelled = function
        | Error exn -> return (lost t exn)
        | Ok (`Gone, _) ->
          (match t.state with
           | Closed -> return (Error Pg_error.Closed)
           | Open -> return (lost t (Failure "socket closed")))
        | Ok (`Timeout, _) ->
          shut_down t;
          return (Error (Pg_error.Timeout Time_ns.Span.zero))
        | Ok (`Done, results) ->
          (match interpret t results with
           | Error (Pg_error.Server { sqlstate = "57014"; _ }) when cancelled ->
             return (Error (Pg_error.Timeout Time_ns.Span.zero))
           | r -> return r)
      in
      match%bind
        Monitor.try_with ~extract_exn:true (fun () -> f ~timeout:(timeout_of deadline))
      with
      | Ok (`Collect_timeout, earlier) ->
        cancel_backend t;
        let grace = Clock_ns.after cancel_grace in
        let%bind r =
          Monitor.try_with ~extract_exn:true (fun () ->
            match%map collect t ~timeout:grace ~stop_at_copy_in:false with
            | `Collect_timeout, _ -> `Timeout, []
            | ((`Done | `Gone) as r), later -> r, earlier @ later)
        in
        finish ~cancelled:true r
      | Ok (((`Done | `Gone | `Timeout) as r), results) ->
        finish ~cancelled:false (Ok (r, results))
      | Error _ as e -> finish ~cancelled:false e)

and run_exec t ~sql ~param_types ~params ~deadline ~label:_ =
  let params = Array.map params ~f:(Option.value ~default:Postgresql.null) in
  with_deadline t ~deadline ~f:(fun ~timeout ->
    t.conn#send_query ~param_types ~params sql;
    match%bind flush_all t ~timeout with
    | (`Timeout | `Gone) as r -> return (r, [])
    | `Done -> collect t ~timeout ~stop_at_copy_in:false)

and run_script_unmetered t sql ~timeout =
  let deadline = Option.map timeout ~f:(Time_ns.add (Time_ns.now ())) in
  let%map r =
    Throttle.enqueue t.sequencer (fun () ->
      run_exec t ~sql ~param_types:[||] ~params:[||] ~deadline ~label:None)
  in
  Result.map r ~f:(ignore : Postgresql.result -> unit)
;;

let interrupt_idle t = Option.iter t.idle_interrupt ~f:(fun i -> Ivar.fill_if_empty i ())

(* Every public entry point: compute the deadline at call time (queueing counts), take the
   connection's turn, record metrics. *)
let submit ?(label = "query") ?timeout t ~f =
  let timeout = Option.first_some timeout t.config.query_timeout in
  let start = Time_ns.now () in
  let deadline = Option.map timeout ~f:(Time_ns.add start) in
  interrupt_idle t;
  let%map r =
    match t.state with
    | Closed -> return (Error Pg_error.Closed)
    | Open -> Throttle.enqueue t.sequencer (fun () -> f ~deadline)
  in
  let r =
    match r, timeout with
    | Error (Pg_error.Timeout _), Some span -> Error (Pg_error.Timeout span)
    | r, _ -> r
  in
  Option.iter t.metrics ~f:(fun m ->
    Metrics.record
      m
      ~label
      ~latency:(Time_ns.diff (Time_ns.now ()) start)
      ~ok:(Result.is_ok r));
  r
;;

let execute_script ?label ?timeout t sql =
  let%map r =
    submit ?label ?timeout t ~f:(fun ~deadline ->
      run_exec t ~sql ~param_types:[||] ~params:[||] ~deadline ~label:None)
  in
  Result.map r ~f:(ignore : Postgresql.result -> unit)
;;

let rec put_all t chunk ~pos ~timeout =
  if pos >= String.length chunk
  then return `Done
  else (
    match t.conn#put_copy_data chunk ~pos with
    | Put_copy_queued -> return `Done
    | Put_copy_error -> return (`Error (String.strip t.conn#error_message))
    | Put_copy_not_queued ->
      (match%bind flush_all t ~timeout with
       | `Done -> put_all t chunk ~pos ~timeout
       | (`Timeout | `Gone) as r -> return r))
;;

module Private = struct
  let exec ?label ?timeout t ~sql ~param_types ~params =
    submit ?label ?timeout t ~f:(fun ~deadline ->
      run_exec t ~sql ~param_types ~params ~deadline ~label:None)
  ;;

  let copy_in ?label ?timeout t ~sql ~data =
    let%map r =
      submit ?label ?timeout t ~f:(fun ~deadline ->
        let error = ref None in
        let%bind r =
          with_deadline t ~deadline ~f:(fun ~timeout ->
            t.conn#send_query sql;
            match%bind flush_all t ~timeout with
            | (`Timeout | `Gone) as r -> return (r, [])
            | `Done ->
              (match%bind collect t ~timeout ~stop_at_copy_in:true with
               | ((`Collect_timeout | `Gone), _) as r -> return r
               | `Done, results ->
                 (match List.last results with
                  | Some r when Poly.equal r#status Postgresql.Copy_in ->
                    let rec send () =
                      match%bind Pipe.read data with
                      | `Eof -> return `Done
                      | `Ok chunk ->
                        (match%bind put_all t chunk ~pos:0 ~timeout with
                         | `Done -> send ()
                         | `Error msg ->
                           error := Some msg;
                           return `Done
                         | (`Timeout | `Gone) as r -> return r)
                    in
                    (match%bind send () with
                     | (`Timeout | `Gone) as r -> return (r, [])
                     | `Done ->
                       let rec finish () =
                         match t.conn#put_copy_end ?error_msg:!error () with
                         | Put_copy_queued -> flush_all t ~timeout
                         | Put_copy_error -> return `Done
                         | Put_copy_not_queued ->
                           (match%bind flush_all t ~timeout with
                            | `Done -> finish ()
                            | (`Timeout | `Gone) as r -> return r)
                       in
                       (match%bind finish () with
                        | (`Timeout | `Gone) as r -> return (r, [])
                        | `Done -> collect t ~timeout ~stop_at_copy_in:false))
                  | Some _ | None -> return (`Done, results))))
        in
        Pipe.close_read data;
        return r)
    in
    Result.bind r ~f:(fun r ->
      match r#status with
      | Copy_in -> Error (Pg_error.Protocol "COPY did not finish")
      | _ ->
        (match Int.of_string_opt r#cmd_tuples with
         | Some n -> Ok n
         | None -> Ok 0))
  ;;

  let set_notification_handler t f = t.on_notification <- f

  let start_idle_watch t =
    let rec loop () =
      match t.state with
      | Closed -> return ()
      | Open ->
        let%bind () =
          Throttle.enqueue t.sequencer (fun () ->
            match t.state with
            | Closed -> return ()
            | Open when Throttle.num_jobs_waiting_to_start t.sequencer > 0 ->
              (* A query queued before this job started; let it run first. *)
              return ()
            | Open ->
              let interrupt = Ivar.create () in
              t.idle_interrupt <- Some interrupt;
              let%map r =
                Fd.interruptible_ready_to t.fd `Read ~interrupt:(Ivar.read interrupt)
              in
              t.idle_interrupt <- None;
              (match r with
               | `Interrupted -> ()
               | `Bad_fd | `Closed -> shut_down t
               | `Ready ->
                 (match
                    Result.try_with (fun () ->
                      t.conn#consume_input;
                      drain_notifications t)
                  with
                  | Ok () -> ()
                  | Error exn -> ignore (lost t exn : (unit, Pg_error.t) Result.t))))
        in
        loop ()
    in
    don't_wait_for (loop ())
  ;;

  let server_error_of_result = server_error_of_result
end
