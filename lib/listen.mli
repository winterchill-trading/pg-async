(** LISTEN/NOTIFY on a dedicated connection.

    The connection keeps reading its socket while idle, so notifications arrive as soon as
    the server sends them, in commit order.

    If the connection is lost, by default the pipe closes. With [~reconnect:true] the
    listener instead opens a new session (waiting [backoff ~attempt] before each try),
    listens to the same channels again, and calls [on_reconnect]. Notifications sent while
    no session was listening are lost: re-read whatever state they describe in
    [on_reconnect]. *)

open! Core
open! Async

type t

(** The first connection is not retried: an error is returned at once.

    @param reconnect default [false].
    @param backoff default 100 ms doubling per attempt, at most 10 s. *)
val create
  :  ?metrics:Metrics.t
  -> ?reconnect:bool
  -> ?backoff:(attempt:int -> Time_ns.Span.t)
  -> ?on_reconnect:(unit -> unit)
  -> Config.t
  -> (t, Pg_error.t) Result.t Deferred.t

(** Channel names are identifiers; they are quoted, so any string is safe and the name is
    case-sensitive. While a reconnecting listener has no session, the channel is recorded
    and [Ok ()] returned. *)
val listen : t -> string -> (unit, Pg_error.t) Result.t Deferred.t

(** Stop listening to a channel. *)
val unlisten : t -> string -> (unit, Pg_error.t) Result.t Deferred.t

(** The channels listened to, sorted. *)
val channels : t -> string list

(** Sessions opened after the first one. *)
val reconnects : t -> int

(** All notifications for the channels listened to. One reader. *)
val notifications : t -> Connection.Notification.t Pipe.Reader.t

(** Stops listening and reconnecting, and closes the pipe. *)
val close : t -> unit Deferred.t

(** [pg_notify(channel, payload)] on any connection. Inside a transaction it is sent at
    commit. *)
val notify
  :  Connection.t
  -> channel:string
  -> payload:string
  -> (unit, Pg_error.t) Result.t Deferred.t

(** Double-quote an identifier for SQL. *)
val quote_identifier : string -> string
