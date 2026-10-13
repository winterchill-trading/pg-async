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

include Applicative.S with type 'a t := 'a t

module Let_syntax : sig
  val return : 'a -> 'a t

  module Let_syntax : sig
    val return : 'a -> 'a t
    val map : 'a t -> f:('a -> 'b) -> 'b t
    val both : 'a t -> 'b t -> ('a * 'b) t

    module Open_on_rhs : sig
      val column : string -> 'a Type.t -> 'a t
      val nth : int -> 'a Type.t -> 'a t
      val int : int Type.t
      val int32 : int32 Type.t
      val int64 : int64 Type.t
      val string : string Type.t
      val bool : bool Type.t
      val float : float Type.t
      val time_ns : Time_ns.t Type.t
      val date : Date.t Type.t
      val bytea : string Type.t
      val json : string Type.t
      val yojson : Yojson.Safe.t Type.t
      val uuid : string Type.t
      val numeric : string Type.t
      val nullable : 'a Type.t -> 'a option Type.t
      val array : 'a Type.t -> 'a list Type.t
    end
  end
end

(** Decode every row of a [Tuples_ok] result. *)
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
