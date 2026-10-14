(** Transactions on one connection.

    [with_transaction] runs [f] between [BEGIN] and [COMMIT]. If [f] returns an error,
    raises, or [commit_if] rejects its value, the transaction rolls back. A serialization
    failure (40001) or deadlock (40P01), from [f] or from [COMMIT], rolls back and runs
    [f] again after a jittered exponential backoff, up to [max_attempts] times in all. [f]
    must therefore be safe to repeat: no side effects outside the database. *)

open! Core
open! Async

module Isolation : sig
  type t =
    | Read_committed
    | Repeatable_read
    | Serializable
  [@@deriving sexp, compare, equal, enumerate]
end

(** [BEGIN], with an isolation level ([Read_committed] by default) and access mode. Prefer
    {!with_transaction}. *)
val begin_
  :  ?isolation:Isolation.t
  -> ?read_only:bool
  -> Connection.t
  -> (unit, Pg_error.t) Result.t Deferred.t

(** [ROLLBACK]. *)
val rollback : Connection.t -> (unit, Pg_error.t) Result.t Deferred.t

(** [COMMIT]. A transaction in which a statement failed is rolled back by the server and
    reported here as SQLSTATE 25P02. *)
val commit : Connection.t -> (unit, Pg_error.t) Result.t Deferred.t

(** 10 ms doubling per attempt, capped at 1 s, each scaled by a random factor from
    0.5 (inclusive) to 1.5 (exclusive). *)
val default_backoff : attempt:int -> Time_ns.Span.t

(** Run [f] in a transaction, retrying serialization failures and deadlocks (see above). *)
val with_transaction
  :  ?isolation:Isolation.t
  -> ?read_only:bool
  -> ?max_attempts:int (** Default 5. *)
  -> ?backoff:(attempt:int -> Time_ns.Span.t)
  -> ?commit_if:('a -> bool)
       (** Roll back (and still return [Ok]) when this is false, e.g.
           [~commit_if:Result.is_ok] for a domain result. Default: always commit. *)
  -> Connection.t
  -> f:(Connection.t -> ('a, Pg_error.t) Result.t Deferred.t)
  -> ('a, Pg_error.t) Result.t Deferred.t
