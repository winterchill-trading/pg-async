(** A typed query parameter: a value together with its codec. Parameters are always sent
    separately from the SQL text ([$1], [$2], ...); this library has no API that splices
    values into SQL. *)

open! Core

type t

val create : 'a Type.t -> 'a -> t

(** A NULL whose type the server infers from context. *)
val null : t

val int : int -> t
val int32 : int32 -> t
val int64 : int64 -> t
val string : string -> t
val bool : bool -> t
val float : float -> t
val time_ns : Time_ns.t -> t
val date : Date.t -> t
val bytea : string -> t
val json : string -> t
val yojson : Yojson.Safe.t -> t
val uuid : string -> t
val numeric : string -> t
val option : 'a Type.t -> 'a option -> t
val array : 'a Type.t -> 'a list -> t

(** [(oid, text)] for libpq; [None] is NULL. *)
val encode : t -> int * string option

val sexp_of_t : t -> Sexp.t
