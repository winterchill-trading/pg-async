(** Throwaway PostgreSQL clusters for tests.

    [with_temp_cluster] runs [initdb] into a fresh temporary directory, starts the server
    with [pg_ctl] on a free local port (TCP on 127.0.0.1 plus a Unix socket in the same
    directory), runs the callback and removes everything afterwards. The cluster trusts
    local connections as user [postgres] and runs with [fsync=off]: fast, and never for
    real data.

    Server binaries are found through [PG_BINDIR], then [pg_config --bindir], then the
    usual install locations (Homebrew [postgresql@18], Debian/Ubuntu
    [/usr/lib/postgresql/<version>/bin]). [initdb] refuses to run as root. *)

open! Core
open! Async

type t

val port : t -> int
val data_dir : t -> string

(** Settings for a database in the cluster ([postgres] by default), TLS disabled because
    the test cluster has no certificate. *)
val config : ?dbname:string -> ?application_name:string -> t -> Pg_async.Config.t

val find_bindir : unit -> string Or_error.t Deferred.t

(** [server_options] are extra [-c name=value] settings, e.g. [("ssl", "on")]. *)
val start
  :  ?bindir:string
  -> ?server_options:(string * string) list
  -> ?setup_data_dir:(string -> unit Deferred.t)
  -> unit
  -> t Or_error.t Deferred.t

val stop : t -> unit Deferred.t

val with_temp_cluster
  :  ?server_options:(string * string) list
  -> ?setup_data_dir:(string -> unit Deferred.t)
  -> (t -> 'a Deferred.t)
  -> 'a Deferred.t

(** [CREATE DATABASE] with a fresh name; returns its settings. *)
val create_database : t -> Pg_async.Config.t Deferred.t

(** One cluster per process, started on first use and stopped at exit. Test files in one
    inline-test library share it; each test should work in its own database
    ({!create_database}). *)
val shared : unit -> t Deferred.t
