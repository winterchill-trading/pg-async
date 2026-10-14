(** One PostgreSQL session driven without blocking the Async scheduler.

    libpq runs in non-blocking mode. A query is sent with [PQsendQueryParams], flushed
    while the socket is writable, and its results are read with [PQconsumeInput],
    [PQisBusy] and [PQgetResult] whenever [Fd.ready_to] reports the socket readable. The
    connection handshake (TCP, TLS, authentication) goes through [PQconnectStart] and
    [PQconnectPoll] the same way. No libpq call here waits on the network.

    Calls on one connection are serialized; concurrent callers queue in order. Use {!Pool}
    to run queries in parallel. A connection whose socket fails, or whose query overruns
    its deadline and cannot be cancelled, closes itself; later calls return [Closed]. *)

open! Core
open! Async

type t

(** Connect, then set the session to [TimeZone=UTC], [DateStyle=ISO], [bytea_output=hex]
    and the configured [statement_timeout]. [connect_timeout] bounds the whole handshake
    (libpq ignores it in non-blocking mode). *)
val connect : ?metrics:Metrics.t -> Config.t -> (t, Pg_error.t) Result.t Deferred.t

(** Idempotent. Pending calls return [Closed]. *)
val close : t -> unit Deferred.t

val is_closed : t -> bool

(** Becomes determined once the connection is closed, by [close] or by a failure. *)
val closed : t -> unit Deferred.t

val config : t -> Config.t
val metrics : t -> Metrics.t option
val backend_pid : t -> int

(** [PQserverVersion], e.g. [180003] for 18.3 and [90624] for 9.6.24. *)
val server_version_num : t -> int

(** Human form, e.g. ["18.3"]. *)
val server_version : t -> string

(** Whether the last statement left the session inside a transaction block, tracked from
    command tags ([BEGIN], [COMMIT], [ROLLBACK]). The pool rolls back a connection
    returned in this state. *)
val in_transaction : t -> bool

(** Run [sql] with the simple query protocol: any number of statements, no parameters,
    results discarded. Meant for migrations and fixed scripts; use {!Query} for anything
    that takes values. *)
val execute_script
  :  ?label:string
  -> ?timeout:Time_ns.Span.t
  -> t
  -> string
  -> (unit, Pg_error.t) Result.t Deferred.t

(** Statements held in this connection's prepared-statement cache. *)
val prepared_statements : t -> int

module Notification : sig
  type t =
    { channel : string
    ; payload : string
    ; pid : int
    }
  [@@deriving sexp_of, compare, equal]
end

(** Building blocks for {!Query}, {!Copy} and {!Listen}. Not a stable interface. *)
module Private : sig
  (** Run one parameterized statement. [params.(i) = None] is SQL NULL. A [param_types]
      entry of [0] lets the server infer the type. Returns the statement's result, whose
      status is [Command_ok] or [Tuples_ok].

      With parameters and a non-zero [statement_cache_size], the statement is prepared
      once per connection and reused. [binary_result] is asked, given the statement's
      result columns from an earlier execution, whether the caller can read them all in
      binary; if so the results come back in the binary format. Without parameters the
      simple query protocol is used and results are text. *)
  val exec
    :  ?label:string
    -> ?timeout:Time_ns.Span.t
    -> ?binary_result:(names:string array -> oids:int array -> bool)
    -> t
    -> sql:string
    -> param_types:int array
    -> params:string option array
    -> (Postgresql.result, Pg_error.t) Result.t Deferred.t

  (** Send [sql] (a [COPY ... FROM STDIN]), stream every chunk of [data] as copy data, end
      the copy and return the number of rows the server reports. If [abort ()] returns a
      message once [data] is exhausted, the copy ends with that error and the server
      discards every row. *)
  val copy_in
    :  ?label:string
    -> ?timeout:Time_ns.Span.t
    -> ?abort:(unit -> string option)
    -> t
    -> sql:string
    -> data:string Pipe.Reader.t
    -> (int, Pg_error.t) Result.t Deferred.t

  (** Notifications are delivered to [f] whenever the connection reads from the socket. *)
  val set_notification_handler : t -> (Notification.t -> unit) -> unit

  (** Keep reading the socket while no query runs, so notifications arrive without
      traffic. Stops when the connection closes. *)
  val start_idle_watch : t -> unit

  val server_error_of_result : Postgresql.result -> Pg_error.Server_error.t
end
