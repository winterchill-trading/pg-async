(* Small operational CLI over the library: connection check, migrations, ad-hoc
   parameterized queries. Settings come from -url, DATABASE_URL or PG*. *)

open! Core
open! Async
open Pg_async

let config_param =
  let%map_open.Command url =
    flag "-url" (optional string) ~doc:"URL postgres://... (default: DATABASE_URL or PG*)"
  and ssl_mode =
    flag
      "-sslmode"
      (optional
         (Arg_type.create (fun s -> Or_error.ok_exn (Config.Ssl_mode.of_string s))))
      ~doc:"MODE override sslmode (disable, require, verify-ca, verify-full)"
  in
  fun () ->
    let open Or_error.Let_syntax in
    let%map config =
      match url with
      | Some url -> Config.of_url url
      | None -> Config.of_env ()
    in
    { config with
      ssl_mode = Option.value ssl_mode ~default:config.ssl_mode
    ; application_name = "pg-async-cli"
    }
;;

let with_connection config ~f =
  let open Deferred.Or_error.Let_syntax in
  let%bind config = Deferred.return (config ()) in
  let%bind conn = Connection.connect config |> Deferred.map ~f:Pg_error.to_or_error in
  Monitor.protect (fun () -> f conn) ~finally:(fun () -> Connection.close conn)
;;

let check =
  Command.async_or_error
    ~summary:"Connect and print the server version and session details"
    (let%map_open.Command config = config_param in
     fun () ->
       with_connection config ~f:(fun conn ->
         let open Deferred.Or_error.Let_syntax in
         let%map user, db, ssl =
           Query.fetch_one
             conn
             "SELECT current_user, current_database(), coalesce((SELECT ssl FROM \
              pg_stat_ssl WHERE pid = pg_backend_pid()), false)"
             ~row:
               Row.Let_syntax.(
                 let%map_open u = nth 0 string
                 and d = nth 1 string
                 and s = nth 2 bool in
                 u, d, s)
           |> Deferred.map ~f:Pg_error.to_or_error
         in
         print_s
           [%message
             ""
               ~server_version:(Connection.server_version conn : string)
               (user : string)
               ~database:(db : string)
               ~tls:(ssl : bool)
               ~backend_pid:(Connection.backend_pid conn : int)]))
;;

let migrations_dir =
  Command.Param.(flag "-dir" (required string) ~doc:"DIR migration files")
;;

let migrate =
  Command.async_or_error
    ~summary:"Apply pending migrations from a directory of <version>_<name>.sql files"
    (let%map_open.Command config = config_param
     and dir = migrations_dir in
     fun () ->
       with_connection config ~f:(fun conn ->
         let open Deferred.Or_error.Let_syntax in
         let lift r = Result.map_error r ~f:Migrate.Error.to_error in
         let%bind migrations = Migrate.load_dir dir |> Deferred.map ~f:lift in
         let%map applied = Migrate.apply conn migrations |> Deferred.map ~f:lift in
         print_s [%message "applied" ~versions:(applied : int list)]))
;;

let status =
  Command.async_or_error
    ~summary:"Show which migrations are applied"
    (let%map_open.Command config = config_param
     and dir = migrations_dir in
     fun () ->
       with_connection config ~f:(fun conn ->
         let open Deferred.Or_error.Let_syntax in
         let lift r = Result.map_error r ~f:Migrate.Error.to_error in
         let%bind migrations = Migrate.load_dir dir |> Deferred.map ~f:lift in
         let%map status = Migrate.status conn migrations |> Deferred.map ~f:lift in
         List.iter status ~f:(fun s -> print_s [%sexp (s : Migrate.Status.t)])))
;;

(* Parameters are sent as untyped text ($1, $2, ...); the server infers their types from
   the statement, as psql's \bind does. *)
let text_param = Type.custom ~name:"unknown" ~oid:0 ~encode:Fn.id ~decode:Result.return ()

let query =
  Command.async_or_error
    ~summary:"Run one parameterized statement and print the rows, tab-separated"
    (let%map_open.Command config = config_param
     and sql = anon ("SQL" %: string)
     and params = flag "-p" (listed string) ~doc:"VALUE parameter, in order ($1, $2, ...)"
     and null =
       flag "-null" (optional_with_default "NULL" string) ~doc:"TEXT how to print NULL"
     in
     fun () ->
       with_connection config ~f:(fun conn ->
         let open Deferred.Or_error.Let_syntax in
         let%map r =
           Connection.Private.exec
             ~label:"cli"
             conn
             ~sql
             ~param_types:(Array.of_list_map params ~f:(fun _ -> Type.oid text_param))
             ~params:(Array.of_list_map params ~f:Option.some)
           |> Deferred.map ~f:Pg_error.to_or_error
         in
         match r#status with
         | Tuples_ok ->
           print_endline (String.concat ~sep:"\t" r#get_fnames_lst);
           for row = 0 to r#ntuples - 1 do
             List.init r#nfields ~f:(fun col ->
               if r#getisnull row col then null else r#getvalue row col)
             |> String.concat ~sep:"\t"
             |> print_endline
           done
         | _ -> print_endline r#cmd_status))
;;

let () =
  Command_unix.run
    (Command.group
       ~summary:"PostgreSQL utilities over pg-async"
       [ "check", check; "migrate", migrate; "status", status; "query", query ])
;;
