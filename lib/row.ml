open! Core

exception Decode_failed of Pg_error.t

type column_ref =
  | Name of string
  | Index of int

(* [prepare]: given a result, resolve column positions once and return a per-row reader.
   [columns]: what the decoder reads and whether each column could come back in binary, so
   a query can ask for binary results before it has any. *)
type 'a t =
  { prepare : Postgresql.result -> int -> 'a
  ; columns : (column_ref * (oid:int -> bool)) list
  }

let fail ~row ~column message =
  raise (Decode_failed (Pg_error.Decode { row; column; message }))
;;

let read ty (r : Postgresql.result) ~index ~column row =
  let raw = if r#getisnull row index then None else Some (r#getvalue row index) in
  let decoded =
    match r#fformat index with
    | TEXT -> Type.decode ty raw
    | BINARY -> Type.decode_binary ty ~oid:(r#ftype_oid index) raw
  in
  match decoded with
  | Ok v -> v
  | Error message -> fail ~row ~column (sprintf "%s: %s" (Type.name ty) message)
;;

let accepts ty ~oid = Type.accepts_binary ty ~oid

let column name ty =
  { columns = [ Name name, accepts ty ]
  ; prepare =
      (fun r ->
        let index =
          match r#fnumber name with
          | i when i >= 0 -> i
          | _ | (exception _) ->
            fail ~row:(-1) ~column:name "no such column in the result"
        in
        fun row -> read ty r ~index ~column:name row)
  }
;;

let nth index ty =
  { columns = [ Index index, accepts ty ]
  ; prepare =
      (fun r ->
        if index < 0 || index >= r#nfields
        then
          fail
            ~row:(-1)
            ~column:(sprintf "#%d" index)
            (sprintf "the result has %d columns" r#nfields);
        let column = r#fname index in
        fun row -> read ty r ~index ~column row)
  }
;;

include Applicative.Make_using_map2 (struct
    type nonrec 'a t = 'a t

    let return x = { prepare = (fun _ _ -> x); columns = [] }

    let map2 a b ~f =
      { columns = a.columns @ b.columns
      ; prepare =
          (fun r ->
            let a = a.prepare r
            and b = b.prepare r in
            fun row -> f (a row) (b row))
      }
    ;;

    let map =
      `Custom (fun a ~f -> { a with prepare = (fun r -> Fn.compose f (a.prepare r)) })
    ;;
  end)

module Let_syntax = struct
  let return = return

  module Let_syntax = struct
    let return = return
    let map = map
    let both = both

    module Open_on_rhs = struct
      let column = column
      let nth = nth
      let int = Type.int
      let int32 = Type.int32
      let int64 = Type.int64
      let string = Type.string
      let bool = Type.bool
      let float = Type.float
      let time_ns = Type.time_ns
      let date = Type.date
      let bytea = Type.bytea
      let json = Type.json
      let yojson = Type.yojson
      let uuid = Type.uuid
      let numeric = Type.numeric
      let interval = Type.interval
      let ofday = Type.ofday
      let inet = Type.inet
      let cidr = Type.cidr
      let range = Type.range
      let with_infinity = Type.with_infinity
      let nullable = Type.nullable
      let array = Type.array
    end
  end
end

let guard f =
  match f () with
  | v -> Ok v
  | exception Decode_failed e -> Error e
;;

let fold t r ~init ~f =
  guard (fun () ->
    let read = t.prepare r in
    let n = r#ntuples in
    let rec go i acc = if i = n then acc else go (i + 1) (f acc (read i)) in
    go 0 init)
;;

let decode_all t r =
  (* In row order, so the first bad row is the one reported. *)
  Result.map (fold t r ~init:[] ~f:(fun acc x -> x :: acc)) ~f:List.rev
;;

let decode_row t r i = guard (fun () -> t.prepare r i)

(* [PQfnumber]'s rules: an exact match first, then the name case-folded unless it is
   double-quoted. *)
let field_number names name =
  match Array.findi names ~f:(fun _ n -> String.equal n name) with
  | Some (i, _) -> Some i
  | None ->
    let folded =
      let b = Buffer.create (String.length name) in
      let in_quotes = ref false in
      let n = String.length name in
      let i = ref 0 in
      while !i < n do
        let c = name.[!i] in
        if Char.equal c '"'
        then
          if !in_quotes && !i + 1 < n && Char.equal name.[!i + 1] '"'
          then (
            Buffer.add_char b '"';
            incr i)
          else in_quotes := not !in_quotes
        else Buffer.add_char b (if !in_quotes then c else Char.lowercase c);
        incr i
      done;
      Buffer.contents b
    in
    Array.findi names ~f:(fun _ n -> String.equal n folded) |> Option.map ~f:fst
;;

let accepts_binary t ~names ~oids =
  List.for_all t.columns ~f:(fun (column, accepts) ->
    let index =
      match column with
      | Name name -> field_number names name
      | Index i -> if i >= 0 && i < Array.length oids then Some i else None
    in
    match index with
    | Some i when i < Array.length oids -> accepts ~oid:oids.(i)
    | Some _ | None -> false)
;;
