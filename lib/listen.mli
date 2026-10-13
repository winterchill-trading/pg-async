(** LISTEN/NOTIFY on a dedicated connection.

    The connection keeps reading its socket while idle, so notifications arrive as soon as
    the server sends them, in commit order. If the connection is lost the pipe closes;
    create a new listener (and re-read whatever state the notifications describe, since
    some may have been missed). *)

open! Core
open! Async

type t

val create : ?metrics:Metrics.t -> Config.t -> (t, Pg_error.t) Result.t Deferred.t

(** Channel names are identifiers; they are quoted, so any string is safe and the name is
    case-sensitive. *)
val listen : t -> string -> (unit, Pg_error.t) Result.t Deferred.t

val unlisten : t -> string -> (unit, Pg_error.t) Result.t Deferred.t

(** All notifications for the channels listened to. One reader. *)
val notifications : t -> Connection.Notification.t Pipe.Reader.t

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
