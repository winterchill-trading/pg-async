open! Core
open! Async

module Isolation = struct
  type t =
    | Read_committed
    | Repeatable_read
    | Serializable
  [@@deriving sexp, compare, equal, enumerate]

  let to_sql = function
    | Read_committed -> "READ COMMITTED"
    | Repeatable_read -> "REPEATABLE READ"
    | Serializable -> "SERIALIZABLE"
  ;;
end

let begin_ ?(isolation = Isolation.Read_committed) ?(read_only = false) conn =
  (* Fixed keywords chosen from a closed set; nothing from the caller reaches the SQL. *)
  Connection.execute_script
    ~label:"begin"
    conn
    (sprintf
       "BEGIN ISOLATION LEVEL %s %s"
       (Isolation.to_sql isolation)
       (if read_only then "READ ONLY" else "READ WRITE"))
;;

(* COMMIT of a transaction in which a statement failed succeeds at the protocol level with
   the command tag ROLLBACK. Report it as the error it is. *)
let commit conn =
  match%map
    Connection.Private.exec
      ~label:"commit"
      conn
      ~sql:"COMMIT"
      ~param_types:[||]
      ~params:[||]
  with
  | Error _ as e -> e
  | Ok r when String.equal r#cmd_status "ROLLBACK" ->
    Error
      (Pg_error.Server
         { severity = "ERROR"
         ; sqlstate = "25P02"
         ; message = "COMMIT rolled back a transaction in which a statement had failed"
         ; detail = None
         ; hint = None
         ; schema = None
         ; table = None
         ; column = None
         ; constraint_ = None
         })
  | Ok (_ : Postgresql.result) -> Ok ()
;;

let rollback conn = Connection.execute_script ~label:"rollback" conn "ROLLBACK"

let default_backoff ~attempt =
  let base_ms = Float.min 1000. (10. *. (2. ** Float.of_int (Int.max 0 (attempt - 1)))) in
  Time_ns.Span.of_ms (base_ms *. (0.5 +. Random.float 1.))
;;

let with_transaction
  ?isolation
  ?read_only
  ?(max_attempts = 5)
  ?(backoff = default_backoff)
  ?(commit_if = fun _ -> true)
  conn
  ~f
  =
  let rollback_quietly () =
    let%map (_ : (unit, Pg_error.t) Result.t) = rollback conn in
    ()
  in
  let rec attempt n =
    let retry_or_fail e =
      if Pg_error.is_retryable_transaction_failure e && n < max_attempts
      then (
        let%bind () = Clock_ns.after (backoff ~attempt:n) in
        attempt (n + 1))
      else return (Error e)
    in
    match%bind begin_ ?isolation ?read_only conn with
    | Error e -> return (Error e)
    | Ok () ->
      (match%bind Monitor.try_with ~extract_exn:true (fun () -> f conn) with
       | Error exn ->
         let%bind () = rollback_quietly () in
         raise exn
       | Ok (Error e) ->
         let%bind () = rollback_quietly () in
         retry_or_fail e
       | Ok (Ok v) when not (commit_if v) ->
         let%map () = rollback_quietly () in
         Ok v
       | Ok (Ok v) ->
         (match%bind commit conn with
          | Ok () -> return (Ok v)
          | Error e ->
            (* A failed COMMIT has already ended the transaction; ROLLBACK is a no-op then
               but keeps [in_transaction] honest if the server did not. *)
            let%bind () =
              if Connection.in_transaction conn then rollback_quietly () else return ()
            in
            retry_or_fail e))
  in
  attempt 1
;;
