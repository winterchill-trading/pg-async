(** Ordered schema migrations.

    A migration is a SQL script with an integer version. [apply] runs every pending
    migration, in version order, inside one transaction together with the bookkeeping rows
    in [schema_migrations] (version, name, MD5 checksum, applied_at): either all of them
    land or none. A transaction-scoped advisory lock serializes concurrent migrators, e.g.
    several replicas starting at once.

    Already-applied migrations are checked first: a changed file (checksum mismatch), a
    missing file, or a new migration numbered below the latest applied one stops the run
    before anything executes. Scripts must not contain their own [BEGIN]/[COMMIT] or
    statements that cannot run in a transaction ([CREATE INDEX CONCURRENTLY], [VACUUM]). *)

open! Core
open! Async

module Migration : sig
  type t =
    { version : int
    ; name : string
    ; sql : string
    }
  [@@deriving sexp_of]

  (** Hex MD5 of [sql]. *)
  val checksum : t -> string
end

module Error : sig
  type t =
    | Pg of Pg_error.t
    | Invalid of string (** Duplicate versions or badly named files. *)
    | Checksum_mismatch of
        { version : int
        ; name : string
        ; applied : string
        ; file : string
        }
    | Missing of
        { version : int
        ; name : string
        } (** Applied in the database, absent from the list. *)
    | Out_of_order of
        { version : int
        ; latest_applied : int
        }
  [@@deriving sexp_of]

  val to_error : t -> Core.Error.t
end

(** Read [<version>_<name>.sql] files (e.g. [0001_create_users.sql]) from [dir]; other
    files are ignored. *)
val load_dir : string -> (Migration.t list, Error.t) Result.t Deferred.t

(** Apply pending migrations; returns the versions applied, in order. *)
val apply : Connection.t -> Migration.t list -> (int list, Error.t) Result.t Deferred.t

module Status : sig
  type state =
    | Applied of Time_ns.Alternate_sexp.t
    | Pending
    | Checksum_mismatch
  [@@deriving sexp_of]

  type t =
    { version : int
    ; name : string
    ; state : state
    }
  [@@deriving sexp_of]
end

(** Every migration in the list with its state; does not create the table. *)
val status
  :  Connection.t
  -> Migration.t list
  -> (Status.t list, Error.t) Result.t Deferred.t
