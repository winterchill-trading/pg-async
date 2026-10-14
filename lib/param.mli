(** A typed query parameter: a value together with its codec. Parameters are always sent
    separately from the SQL text ([$1], [$2], ...); this library has no API that splices
    values into SQL. *)

open! Core

type t

(** [value] with its codec, e.g. [create Type.int 42]. *)
val create : 'a Type.t -> 'a -> t

(** A NULL whose type the server infers from context. *)
val null : t

(** {2 Shortcuts}

    Each is [create] with the {!Type} codec of the same name. *)

(** [int8]. *)
val int : int -> t

(** [int4]. *)
val int32 : int32 -> t

(** [int8]. *)
val int64 : int64 -> t

(** [text]. *)
val string : string -> t

(** [bool]. *)
val bool : bool -> t

(** [float8]. *)
val float : float -> t

(** [timestamptz]. *)
val time_ns : Time_ns.t -> t

(** [date]. *)
val date : Date.t -> t

(** [bytea]. *)
val bytea : string -> t

(** [jsonb], as JSON text. *)
val json : string -> t

(** [jsonb]. *)
val yojson : Yojson.Safe.t -> t

(** [uuid], as its text form. *)
val uuid : string -> t

(** [numeric], as its exact text form. *)
val numeric : string -> t

(** [interval]. *)
val interval : Type.Interval.t -> t

(** [time]. *)
val ofday : Time_ns.Ofday.t -> t

(** [inet]. *)
val inet : Type.Inet.t -> t

(** [cidr]. *)
val cidr : Type.Inet.t -> t

(** The range type over the element codec ({!Type.range}). *)
val range : 'a Type.t -> 'a Type.Range.t -> t

(** [None] is NULL, typed by the codec. *)
val option : 'a Type.t -> 'a option -> t

(** A one-dimensional array ({!Type.array}). *)
val array : 'a Type.t -> 'a list -> t

(** [(oid, text)] for libpq; [None] is NULL. *)
val encode : t -> int * string option

(** The type name and encoded value, for logs. *)
val sexp_of_t : t -> Sexp.t
