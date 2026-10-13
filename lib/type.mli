(** PostgreSQL value codecs in the text format.

    A ['a t] encodes OCaml values as query parameters and decodes result columns. NULL is
    only accepted or produced by {!nullable}; any other codec reports a NULL column as an
    error. *)

open! Core

type 'a t

(** SQL type name, e.g. ["int8"] or ["text[]"]. *)
val name : _ t -> string

(** Parameter type OID sent with the query; [0] lets the server infer it. *)
val oid : _ t -> int

(** [None] is SQL NULL. *)
val encode : 'a t -> 'a -> string option

(** [None] is SQL NULL. *)
val decode : 'a t -> string option -> ('a, string) Result.t

(** [int8]; also reads [int2] and [int4] columns. *)
val int : int t

val int32 : int32 t
val int64 : int64 t
val string : string t
val bool : bool t

(** [float8]; NaN and infinities included. *)
val float : float t

(** [timestamptz]. PostgreSQL keeps microseconds; finer digits are rounded by the server.
    Reads any UTC offset. *)
val time_ns : Time_ns.t t

val date : Date.t t

(** [bytea], hex format. *)
val bytea : string t

(** [jsonb] as text. The server normalises it (key order, spacing). *)
val json : string t

(** [jsonb] as [Yojson.Safe.t]. *)
val yojson : Yojson.Safe.t t

(** [uuid] as its text form. *)
val uuid : string t

(** [numeric] as its exact text form. *)
val numeric : string t

val nullable : 'a t -> 'a option t

(** One-dimensional arrays. Raises for an element codec without an array type (an array of
    arrays, or a custom codec created without [array_oid]). *)
val array : 'a t -> 'a list t

(** A codec for another type. [oid] [0] lets the server infer the parameter type. *)
val custom
  :  name:string
  -> oid:int
  -> ?array_oid:int
  -> encode:('a -> string)
  -> decode:(string -> ('a, string) Result.t)
  -> unit
  -> 'a t

(** Reuse a codec through a conversion, e.g. an identifier type over [string]. *)
val map
  :  'a t
  -> name:string
  -> f:('a -> ('b, string) Result.t)
  -> f_inv:('b -> 'a)
  -> 'b t

(** Array literal syntax, exposed for tests and {!Copy}. *)
module Array_literal : sig
  val to_string : string option list -> string
  val of_string : string -> (string option list, string) Result.t
end

module Timestamptz : sig
  val to_string : Time_ns.t -> string
  val of_string : string -> (Time_ns.t, string) Result.t
end
