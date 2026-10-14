(* The README's quick start, built in CI so it keeps compiling. Run it against a database
   with a [users] table and a [migrations] directory:

   DATABASE_URL=postgres:///app?sslmode=disable dune exec examples/quick_start.exe *)

open! Core
open! Async
open Pg_async

type user =
  { id : int
  ; email : string
  ; created_at : Time_ns.t
  }

let user_row =
  let open Row.Let_syntax in
  let%map_open id = column "id" int
  and email = column "email" string
  and created_at = column "created_at" time_ns in
  { id; email; created_at }
;;

let migrate config =
  let%bind conn = Connection.connect config >>| Pg_error.ok_exn in
  let%bind applied =
    match%bind Migrate.load_dir "migrations" with
    | Error _ as e -> return e
    | Ok migrations -> Migrate.apply conn migrations
  in
  let%map () = Connection.close conn in
  Result.map_error applied ~f:Migrate.Error.to_error |> ok_exn
;;

let register pool email =
  Pool.with_transaction pool ~isolation:Serializable ~f:(fun conn ->
    Query.fetch_one
      conn
      "INSERT INTO users (email) VALUES ($1) RETURNING id, email, created_at"
      ~params:[ Param.string email ]
      ~row:user_row)
;;

let main () =
  let config = Config.of_env () |> ok_exn (* DATABASE_URL or PG* *) in
  let%bind (_ : int list) = migrate config in
  let pool = Pool.create ~max_size:8 config in
  let%bind () =
    match%map register pool "a@example.com" with
    | Ok user ->
      printf
        "created %d for %s at %s\n"
        user.id
        user.email
        (Time_ns.to_string_utc user.created_at)
    | Error e when Pg_error.is_unique_violation e -> print_endline "already registered"
    | Error e -> Error.raise (Pg_error.to_error e)
  in
  Pool.close pool
;;

let () =
  Command_unix.run
    (Command.async
       ~summary:"Apply migrations and register a user"
       (Command.Param.return main))
;;
