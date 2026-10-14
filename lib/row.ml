open! Core

exception Decode_failed of Pg_error.t

(* Given a result, resolve column positions once and return a per-row reader. *)
type 'a t = { prepare : Postgresql.result -> int -> 'a }

let fail ~row ~column message =
  raise (Decode_failed (Pg_error.Decode { row; column; message }))
;;

let read ty (r : Postgresql.result) ~index ~column row =
  let raw = if r#getisnull row index then None else Some (r#getvalue row index) in
  match Type.decode ty raw with
  | Ok v -> v
  | Error message -> fail ~row ~column (sprintf "%s: %s" (Type.name ty) message)
;;

let column name ty =
  { prepare =
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
  { prepare =
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

    let return x = { prepare = (fun _ _ -> x) }

    let map2 a b ~f =
      { prepare =
          (fun r ->
            let a = a.prepare r
            and b = b.prepare r in
            fun row -> f (a row) (b row))
      }
    ;;

    let map = `Custom (fun a ~f -> { prepare = (fun r -> Fn.compose f (a.prepare r)) })
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
