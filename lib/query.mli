(** Parameterized statements on one connection.

    SQL refers to parameters as [$1], [$2], ...; values travel separately as typed
    {!Param.t}s. Every function takes [?label] (the {!Metrics} label, default ["query"])
    and [?timeout] (overrides [Config.query_timeout]). *)

open! Core
open! Async

type 'a result := ('a, Pg_error.t) Result.t Deferred.t

(** Run a statement that returns no rows; the number of rows it affected
    ([INSERT]/[UPDATE]/[DELETE]), or 0. *)
val execute
  :  ?label:string
  -> ?timeout:Time_ns.Span.t
  -> ?params:Param.t list
  -> Connection.t
  -> string
  -> int result

val fetch_all
  :  ?label:string
  -> ?timeout:Time_ns.Span.t
  -> ?params:Param.t list
  -> Connection.t
  -> string
  -> row:'a Row.t
  -> 'a list result

(** Exactly one row, or [Row_count]. *)
val fetch_one
  :  ?label:string
  -> ?timeout:Time_ns.Span.t
  -> ?params:Param.t list
  -> Connection.t
  -> string
  -> row:'a Row.t
  -> 'a result

(** Zero or one row; more is [Row_count]. *)
val fetch_opt
  :  ?label:string
  -> ?timeout:Time_ns.Span.t
  -> ?params:Param.t list
  -> Connection.t
  -> string
  -> row:'a Row.t
  -> 'a option result

val fold
  :  ?label:string
  -> ?timeout:Time_ns.Span.t
  -> ?params:Param.t list
  -> Connection.t
  -> string
  -> row:'a Row.t
  -> init:'acc
  -> f:('acc -> 'a -> 'acc)
  -> 'acc result
