(** Bulk loads with [COPY ... FROM STDIN] (text format).

    Rows are encoded with the same codecs as query parameters, escaped for the COPY text
    format and streamed in chunks of about 64 KiB as the pipe yields them. The whole load
    is one statement: it lands completely or not at all. *)

open! Core
open! Async

(** [table] and [columns] are identifiers and are quoted; pass [schema] for a table
    outside the search path. Each row must have one value per column. Returns the number
    of rows loaded. *)
val from_pipe
  :  ?label:string
  -> ?timeout:Time_ns.Span.t
  -> ?schema:string
  -> Connection.t
  -> table:string
  -> columns:string list
  -> Param.t list Pipe.Reader.t
  -> (int, Pg_error.t) Result.t Deferred.t

(** {!from_pipe} over a list of rows. *)
val from_list
  :  ?label:string
  -> ?timeout:Time_ns.Span.t
  -> ?schema:string
  -> Connection.t
  -> table:string
  -> columns:string list
  -> Param.t list list
  -> (int, Pg_error.t) Result.t Deferred.t

(** One row in COPY text format, newline included. *)
val encode_row : Param.t list -> string
