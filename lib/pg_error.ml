open! Core

module Server_error = struct
  type t =
    { severity : string
    ; sqlstate : string
    ; message : string
    ; detail : string option [@sexp.option]
    ; hint : string option [@sexp.option]
    ; schema : string option [@sexp.option]
    ; table : string option [@sexp.option]
    ; column : string option [@sexp.option]
    ; constraint_ : string option [@sexp.option]
    }
  [@@deriving sexp_of, compare, equal]
end

type t =
  | Connect of string
  | Server of Server_error.t
  | Decode of
      { row : int
      ; column : string
      ; message : string
      }
  | Row_count of
      { expected : string
      ; got : int
      }
  | Timeout of Time_ns.Span.t
  | Connection_lost of string
  | Closed
  | Protocol of string
[@@deriving sexp_of, compare, equal]

let sqlstate = function
  | Server { sqlstate; _ } -> Some sqlstate
  | Connect _
  | Decode _
  | Row_count _
  | Timeout _
  | Connection_lost _
  | Closed
  | Protocol _ -> None
;;

let is_retryable_transaction_failure t =
  match sqlstate t with
  | Some ("40001" | "40P01") -> true
  | Some _ | None -> false
;;

let is_unique_violation t = Option.equal String.equal (sqlstate t) (Some "23505")
let to_error t = Error.create_s [%message "postgres" ~_:(t : t)]
let to_or_error r = Result.map_error r ~f:to_error
let ok_exn r = Or_error.ok_exn (to_or_error r)
