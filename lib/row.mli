(** Row decoders, combined applicatively:

    {[
      let user =
        let open Row.Let_syntax in
        let%map_open id = column "id" int
        and email = column "email" string
        and deleted_at = column "deleted_at" (nullable time_ns) in
        { id; email; deleted_at }
      ;;
    ]}

    Column names are resolved once per result. A failure names the row and the column. *)

open! Core

type 'a t

(** The column with this name; it must appear in the result. *)
val column : string -> 'a Type.t -> 'a t

(** The column at this zero-based position. *)
val nth : int -> 'a Type.t -> 'a t

(** [return], [map], [both], [map2], ...: decoders combine applicatively. *)
include Applicative.S with type 'a t := 'a t

(** For [let%map_open] / [let%map] (ppx_let). *)
module Let_syntax : sig
  (** A decoder that reads nothing. *)
  val return : 'a -> 'a t

  module Let_syntax : sig
    (** A decoder that reads nothing. *)
    val return : 'a -> 'a t

    (** Transform a decoded value. *)
    val map : 'a t -> f:('a -> 'b) -> 'b t

    (** Decode two columns. *)
    val both : 'a t -> 'b t -> ('a * 'b) t

    (** In scope on the right of [let%map_open]: {!column}, {!nth} and the {!Type} codecs. *)
    module Open_on_rhs : sig
      (** {!Row.column}. *)
      val column : string -> 'a Type.t -> 'a t

      (** {!Row.nth}. *)
      val nth : int -> 'a Type.t -> 'a t

      (** {!Type.int}. *)
      val int : int Type.t

      (** {!Type.int32}. *)
      val int32 : int32 Type.t

      (** {!Type.int64}. *)
      val int64 : int64 Type.t

      (** {!Type.string}. *)
      val string : string Type.t

      (** {!Type.bool}. *)
      val bool : bool Type.t

      (** {!Type.float}. *)
      val float : float Type.t

      (** {!Type.time_ns}. *)
      val time_ns : Time_ns.t Type.t

      (** {!Type.date}. *)
      val date : Date.t Type.t

      (** {!Type.bytea}. *)
      val bytea : string Type.t

      (** {!Type.json}. *)
      val json : string Type.t

      (** {!Type.yojson}. *)
      val yojson : Yojson.Safe.t Type.t

      (** {!Type.uuid}. *)
      val uuid : string Type.t

      (** {!Type.numeric}. *)
      val numeric : string Type.t

      (** {!Type.interval}. *)
      val interval : Type.Interval.t Type.t

      (** {!Type.ofday}. *)
      val ofday : Time_ns.Ofday.t Type.t

      (** {!Type.inet}. *)
      val inet : Type.Inet.t Type.t

      (** {!Type.cidr}. *)
      val cidr : Type.Inet.t Type.t

      (** {!Type.range}. *)
      val range : 'a Type.t -> 'a Type.Range.t Type.t

      (** {!Type.with_infinity}. *)
      val with_infinity : 'a Type.t -> 'a Type.Infinite.t Type.t

      (** {!Type.nullable}. *)
      val nullable : 'a Type.t -> 'a option Type.t

      (** {!Type.array}. *)
      val array : 'a Type.t -> 'a list Type.t
    end
  end
end

(** Whether every column this decoder reads can be decoded from the binary format, given
    the column names and type OIDs of the statement's result (known from an earlier
    execution of the same prepared statement). Column names are matched as libpq's
    [PQfnumber] does. *)
val accepts_binary : _ t -> names:string array -> oids:int array -> bool

(** Decode every row of a [Tuples_ok] result. Each column is read in the format the result
    carries it in. *)
val decode_all : 'a t -> Postgresql.result -> ('a list, Pg_error.t) Result.t

(** Decode one row, by index. *)
val decode_row : 'a t -> Postgresql.result -> int -> ('a, Pg_error.t) Result.t

(** Fold over the rows without building a list. *)
val fold
  :  'a t
  -> Postgresql.result
  -> init:'acc
  -> f:('acc -> 'a -> 'acc)
  -> ('acc, Pg_error.t) Result.t
