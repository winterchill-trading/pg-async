open! Core
open! Async

type t =
  { conn : Connection.t
  ; reader : Connection.Notification.t Pipe.Reader.t
  }

let quote_identifier s =
  "\"" ^ String.substr_replace_all s ~pattern:"\"" ~with_:"\"\"" ^ "\""
;;

let create ?metrics config =
  match%map Connection.connect ?metrics config with
  | Error _ as e -> e
  | Ok conn ->
    let reader, writer = Pipe.create () in
    Connection.Private.set_notification_handler conn (fun n ->
      Pipe.write_without_pushback_if_open writer n);
    upon (Connection.closed conn) (fun () -> Pipe.close writer);
    Connection.Private.start_idle_watch conn;
    Ok { conn; reader }
;;

(* LISTEN takes an identifier, which cannot be a bind parameter; [quote_identifier] makes
   any channel name a single quoted identifier. *)
let listen t channel =
  Connection.execute_script ~label:"listen" t.conn ("LISTEN " ^ quote_identifier channel)
;;

let unlisten t channel =
  Connection.execute_script ~label:"listen" t.conn ("UNLISTEN " ^ quote_identifier channel)
;;

let notifications t = t.reader
let close t = Connection.close t.conn

let notify conn ~channel ~payload =
  Query.fetch_all
    ~label:"notify"
    conn
    "SELECT pg_notify($1, $2)"
    ~params:[ Param.string channel; Param.string payload ]
    ~row:(Row.return ())
  |> Deferred.Result.ignore_m
;;
