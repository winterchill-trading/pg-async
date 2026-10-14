(** A bounded pool of connections.

    At most [max_size] sessions exist at once; callers beyond that wait in order for up to
    [acquire_timeout]. Idle connections are reused most-recently-used first, so a quiet
    pool shrinks: connections idle longer than [max_idle_time] are closed. A connection
    idle longer than [health_check_after] runs [SELECT 1] before it is handed out, and
    broken connections are replaced. A connection given back inside a transaction block is
    rolled back first.

    With [min_size], that many connections are opened in the background at [create] and
    kept open through idle periods (a failed open is retried every few seconds). With
    [max_lifetime], a connection older than that (less up to 10% per connection, so
    connections opened together do not expire together) is closed when it is next returned
    or found idle, and replaced if needed; this bounds how long a session keeps
    server-side state and lets a pool follow DNS or failover changes. *)

open! Core
open! Async

type t

val create
  :  ?metrics:Metrics.t
  -> ?max_size:int (** Default 10. *)
  -> ?min_size:int (** Default 0. At most [max_size]. *)
  -> ?max_lifetime:Time_ns.Span.t (** Default: no limit. *)
  -> ?acquire_timeout:Time_ns.Span.t (** Default 30 s. *)
  -> ?health_check_after:Time_ns.Span.t (** Default 30 s. *)
  -> ?max_idle_time:Time_ns.Span.t (** Default 5 min. *)
  -> Config.t
  -> t

val config : t -> Config.t

(** Borrow a connection for the duration of [f]. *)
val with_connection
  :  t
  -> f:(Connection.t -> ('a, Pg_error.t) Result.t Deferred.t)
  -> ('a, Pg_error.t) Result.t Deferred.t

(** {!Tx.with_transaction} on one borrowed connection (retries reuse it). *)
val with_transaction
  :  ?isolation:Tx.Isolation.t
  -> ?read_only:bool
  -> ?max_attempts:int
  -> ?backoff:(attempt:int -> Time_ns.Span.t)
  -> ?commit_if:('a -> bool)
  -> t
  -> f:(Connection.t -> ('a, Pg_error.t) Result.t Deferred.t)
  -> ('a, Pg_error.t) Result.t Deferred.t

(** {!Query} functions, each on a borrowed connection. *)

val execute
  :  ?label:string
  -> ?timeout:Time_ns.Span.t
  -> ?params:Param.t list
  -> t
  -> string
  -> (int, Pg_error.t) Result.t Deferred.t

val fetch_all
  :  ?label:string
  -> ?timeout:Time_ns.Span.t
  -> ?params:Param.t list
  -> t
  -> string
  -> row:'a Row.t
  -> ('a list, Pg_error.t) Result.t Deferred.t

val fetch_one
  :  ?label:string
  -> ?timeout:Time_ns.Span.t
  -> ?params:Param.t list
  -> t
  -> string
  -> row:'a Row.t
  -> ('a, Pg_error.t) Result.t Deferred.t

val fetch_opt
  :  ?label:string
  -> ?timeout:Time_ns.Span.t
  -> ?params:Param.t list
  -> t
  -> string
  -> row:'a Row.t
  -> ('a option, Pg_error.t) Result.t Deferred.t

module Stats : sig
  type t =
    { size : int (** Open or opening connections. *)
    ; idle : int
    ; waiting : int
    ; opened : int (** Connections created since [create]. *)
    ; discarded : int (** Connections closed because broken, unhealthy or idle. *)
    }
  [@@deriving sexp_of]
end

val stats : t -> Stats.t

(** Close idle connections now and the rest as they are returned; waiting and later
    callers get [Closed]. *)
val close : t -> unit Deferred.t
