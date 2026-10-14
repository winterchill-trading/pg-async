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

(** Whether a result column of type [oid] can be read in the binary format. The built-in
    codecs read their own types in binary (plus [int2]/[int4] for the integer codecs,
    [varchar]/[bpchar]/[name] for {!string} and [json] for {!json}/{!yojson}), arrays and
    ranges of those, and nullable or {!map}ped versions of those; {!numeric} and {!custom}
    codecs read text only. A query asks for binary results only when every column it
    decodes accepts binary (see {!Query}). *)
val accepts_binary : _ t -> oid:int -> bool

(** Decode a binary-format value of a column of type [oid]. It gives the same value as
    {!decode} on the column's text form. [None] is SQL NULL. *)
val decode_binary : 'a t -> oid:int -> string option -> ('a, string) Result.t

(** [int8]; also reads [int2] and [int4] columns. *)
val int : int t

(** [int4]; also reads [int2]. *)
val int32 : int32 t

(** [int8]; also reads [int2] and [int4]. *)
val int64 : int64 t

(** [text]; also reads [varchar], [bpchar] (padded) and [name]. *)
val string : string t

(** [bool]. *)
val bool : bool t

(** [float8]; NaN and infinities included. *)
val float : float t

(** [timestamptz]. PostgreSQL keeps microseconds; finer digits are rounded by the server.
    Reads any UTC offset. *)
val time_ns : Time_ns.t t

(** [date], years 1 to 9999. *)
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

(** [interval] as months, days and microseconds, the three fields PostgreSQL keeps
    separately (a month is not a fixed number of days, nor a day of seconds). Reads both
    the [postgres] and the [iso_8601] interval styles. *)
module Interval : sig
  type t =
    { months : int
    ; days : int
    ; microseconds : int
    }
  [@@deriving sexp, compare, equal, hash]

  (** No time at all. *)
  val zero : t

  (** Microseconds only, truncated toward zero. *)
  val of_span : Time_ns.Span.t -> t

  (** [None] when [months] or [days] is not zero. *)
  val to_span : t -> Time_ns.Span.t option

  (** The form sent to the server: [N mons N days [-]H:MM:SS.ffffff]. *)
  val to_string : t -> string

  (** Reads the [postgres] and [iso_8601] interval styles. *)
  val of_string : string -> (t, string) Result.t
end

(** [interval]. *)
val interval : Interval.t t

(** [time] (without time zone), microsecond precision; [24:00:00] reads as
    [Time_ns.Ofday.start_of_next_day]. *)
val ofday : Time_ns.Ofday.t t

(** An address with a prefix length, for [inet] and [cidr]. *)
module Inet : sig
  type t =
    { address : Core_unix.Inet_addr.t
    ; prefix_length : int (** 0 to 32 for IPv4, 0 to 128 for IPv6. *)
    }
  [@@deriving compare, equal, sexp_of]

  (** [prefix_length] defaults to the full width of the address. *)
  val create : ?prefix_length:int -> Core_unix.Inet_addr.t -> t Or_error.t

  (** [address/prefix_length]. *)
  val to_string : t -> string

  (** An address with an optional [/prefix_length]. *)
  val of_string : string -> (t, string) Result.t
end

(** [inet]: a host address with an optional netmask. *)
val inet : Inet.t t

(** [cidr]: a network. The server rejects a value with bits set right of the prefix. *)
val cidr : Inet.t t

(** Range values. Discrete ranges ([int4range], [int8range], [daterange]) come back from
    the server in canonical form, [\[lower, upper)]. *)
module Range : sig
  type 'a bound =
    | Inclusive of 'a
    | Exclusive of 'a
    | Unbounded
  [@@deriving sexp, compare, equal]

  type 'a t =
    | Empty
    | Non_empty of
        { lower : 'a bound
        ; upper : 'a bound
        }
  [@@deriving sexp, compare, equal]

  (** Map the bound values. *)
  val map : 'a t -> f:('a -> 'b) -> 'b t

  (** The range literal syntax over already encoded elements. *)
  val to_literal : string t -> string

  (** Parse a range literal; the elements are left encoded. *)
  val of_literal : string -> (string t, string) Result.t
end

(** The range type over an element codec: [int4range] over {!int32}, [int8range] over
    {!int} and {!int64}, [numrange] over {!numeric}, [tstzrange] over {!time_ns} and
    [daterange] over {!date}.

    Raises if the element codec has no range type. *)
val range : 'a t -> 'a Range.t t

(** A value or one of PostgreSQL's [infinity] / [-infinity]. *)
module Infinite : sig
  type 'a t =
    | Neg_infinity
    | Finite of 'a
    | Infinity
  [@@deriving sexp, compare, equal]
end

(** Accept [infinity] and [-infinity] as well, for [timestamptz] ({!time_ns}) and [date]
    columns, e.g. [with_infinity time_ns] for an open-ended validity column or a
    [tstzrange] bound ([range (with_infinity time_ns)]). *)
val with_infinity : 'a t -> 'a Infinite.t t

(** Also accepts and produces SQL NULL, as [None]. *)
val nullable : 'a t -> 'a option t

(** One-dimensional arrays. Raises for an element codec without an array type (an array of
    arrays, or a custom codec created without [array_oid]). *)
val array : 'a t -> 'a list t

(** A codec for another type, in the text format. [oid] [0] lets the server infer the
    parameter type. *)
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
  (** [{"a",NULL}]: every element quoted. *)
  val to_string : string option list -> string

  (** Parse a one-dimensional array literal with default bounds. *)
  val of_string : string -> (string option list, string) Result.t
end

module Timestamptz : sig
  (** ISO 8601 in UTC, e.g. [2026-10-13T12:00:00.5Z]. *)
  val to_string : Time_ns.t -> string

  (** The server's output form or ISO 8601, any UTC offset. *)
  val of_string : string -> (Time_ns.t, string) Result.t
end
