(** Connection settings.

    TLS is required by default: [ssl_mode] defaults to [Verify_full], which checks the
    server certificate chain and host name. Local development and the test clusters pass
    [~ssl_mode:Disable] explicitly; nothing downgrades silently.

    Fields left as [None] fall back to libpq's own defaults and its [PG*] environment
    variables. The password never appears in sexps or [to_string_hum]. *)

open! Core

module Ssl_mode : sig
  type t =
    | Disable
    | Allow
    | Prefer
    | Require
    | Verify_ca
    | Verify_full
  [@@deriving sexp, compare, equal, enumerate]

  (** The libpq keyword value, e.g. ["verify-full"]. *)
  val to_string : t -> string

  val of_string : string -> t Or_error.t
end

type t =
  { host : string option
  ; port : int option
  ; user : string option
  ; password : string option
  ; dbname : string option
  ; ssl_mode : Ssl_mode.t
  ; ssl_root_cert : string option (** CA file for [Verify_ca]/[Verify_full]. *)
  ; application_name : string
  ; connect_timeout : Time_ns.Span.t
  ; statement_timeout : Time_ns.Span.t option
  (** Server-side [statement_timeout], set for the session after connecting. *)
  ; query_timeout : Time_ns.Span.t option
  (** Default client-side deadline for each query; [None] waits forever. *)
  ; extra : (string * string) list (** Other libpq keywords, passed through. *)
  }
[@@deriving sexp_of]

val default_application_name : string

val create
  :  ?host:string
  -> ?port:int
  -> ?user:string
  -> ?password:string
  -> ?dbname:string
  -> ?ssl_mode:Ssl_mode.t
  -> ?ssl_root_cert:string
  -> ?application_name:string
  -> ?connect_timeout:Time_ns.Span.t
  -> ?statement_timeout:Time_ns.Span.t
  -> ?query_timeout:Time_ns.Span.t
  -> ?extra:(string * string) list
  -> unit
  -> t

(** Parse [postgres://user:password@host:port/dbname?sslmode=...]. Recognised query
    parameters: [sslmode], [sslrootcert], [application_name], [connect_timeout] (seconds),
    [statement_timeout] and [query_timeout] (milliseconds); the rest go to [extra]. A host
    starting with [/] (percent-encoded as [%2F]) is a socket directory. Without [sslmode]
    the URL gets the [Verify_full] default. *)
val of_url : string -> t Or_error.t

(** [DATABASE_URL] when set, otherwise [PGHOST], [PGPORT], [PGUSER], [PGPASSWORD],
    [PGDATABASE], [PGSSLMODE], [PGSSLROOTCERT], [PGAPPNAME] and [PGCONNECT_TIMEOUT].
    [getenv] defaults to [Sys.getenv]. *)
val of_env : ?getenv:(string -> string option) -> unit -> t Or_error.t

(** The libpq connection string, values quoted. Contains the password. *)
val to_conninfo : t -> string

(** A URL-like rendering for logs, with the password replaced by [***]. *)
val to_string_hum : t -> string
