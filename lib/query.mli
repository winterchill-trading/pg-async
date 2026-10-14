(** Parameterized statements on one connection.

    SQL refers to parameters as [$1], [$2], ...; values travel separately as typed
    {!Param.t}s. Every function takes [?label] (the {!Metrics} label, default ["query"])
    and [?timeout] (overrides [Config.query_timeout]).

    {2 Prepared statements and binary results}

    A statement with parameters is prepared on its connection the first time it runs and
    reused afterwards (up to [Config.statement_cache_size] statements per connection,
    least recently used evicted). Its first execution returns text; from then on, if every
    column the {!Row.t} decodes accepts the binary format ({!Type.accepts_binary}),
    results come back in binary, which skips parsing numbers, timestamps and hex [bytea]
    on both ends. A cached statement that the server no longer knows, or whose result
    shape changed after DDL, is re-prepared and retried once outside a transaction.

    A statement without parameters uses the simple query protocol (text results, never
    cached), so such a string may hold several statements. Set [statement_cache_size] to
    [0] behind a pooler that does not support prepared statements (PgBouncer before 1.21
    in transaction mode). *)

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

(** Every row, in order. *)
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

(** Fold over the rows without building a list. *)
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
