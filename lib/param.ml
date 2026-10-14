open! Core

type t =
  | T : 'a Type.t * 'a -> t
  | Null

let create ty v = T (ty, v)
let null = Null
let int = create Type.int
let int32 = create Type.int32
let int64 = create Type.int64
let string = create Type.string
let bool = create Type.bool
let float = create Type.float
let time_ns = create Type.time_ns
let date = create Type.date
let bytea = create Type.bytea
let json = create Type.json
let yojson = create Type.yojson
let uuid = create Type.uuid
let numeric = create Type.numeric
let interval = create Type.interval
let ofday = create Type.ofday
let inet = create Type.inet
let cidr = create Type.cidr
let range ty v = create (Type.range ty) v
let option ty v = create (Type.nullable ty) v
let array ty v = create (Type.array ty) v

let encode = function
  | Null -> 0, None
  | T (ty, v) -> Type.oid ty, Type.encode ty v
;;

let sexp_of_t = function
  | Null -> [%sexp "NULL"]
  | T (ty, v) -> [%sexp (Type.name ty : string), (Type.encode ty v : string option)]
;;
