(** Errors returned by every operation in this library.

    Server errors keep the fields PostgreSQL sends (SQLSTATE, detail, constraint, ...) so
    callers can branch on them, e.g. retry on a serialization failure or map a unique
    violation to a domain error. *)

open! Core

module Server_error : sig
  type t =
    { severity : string
    ; sqlstate : string
    ; message : string
    ; detail : string option
    ; hint : string option
    ; schema : string option
    ; table : string option
    ; column : string option
    ; constraint_ : string option
    }
  [@@deriving sexp_of, compare, equal]
end

type t =
  | Connect of string (** Establishing the connection failed. *)
  | Server of Server_error.t (** The server rejected the statement. *)
  | Decode of
      { row : int
      ; column : string
      ; message : string
      } (** A row decoder could not read a column. *)
  | Row_count of
      { expected : string
      ; got : int
      } (** [fetch_one] got zero or several rows. *)
  | Timeout of Time_ns.Span.t (** The client-side deadline passed. *)
  | Connection_lost of string (** The socket failed mid-operation. *)
  | Closed (** The connection or pool was closed before the call. *)
  | Protocol of string (** libpq returned something this library does not expect. *)
[@@deriving sexp_of, compare, equal]

val sqlstate : t -> string option

(** SQLSTATE 40001 (serialization_failure) or 40P01 (deadlock_detected): the transaction
    can be retried as a whole. *)
val is_retryable_transaction_failure : t -> bool

(** SQLSTATE 23505. *)
val is_unique_violation : t -> bool

val to_error : t -> Error.t
val ok_exn : ('a, t) Result.t -> 'a

(** Turn a [('a, t) Result.t] into an [Or_error.t]. *)
val to_or_error : ('a, t) Result.t -> 'a Or_error.t
