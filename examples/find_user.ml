(* The example in the [Pg_async] module documentation, built in CI. *)

open! Core
open! Async
open Pg_async

let user_row =
  let open Row.Let_syntax in
  let%map_open id = column "id" int
  and email = column "email" string in
  id, email
;;

let find_user pool ~id =
  Pool.fetch_opt
    pool
    "SELECT id, email FROM users WHERE id = $1"
    ~params:[ Param.int id ]
    ~row:user_row
;;

let main () =
  let config = Config.of_env () |> ok_exn in
  let pool = Pool.create ~max_size:8 config in
  let%bind user = find_user pool ~id:42 in
  print_s [%sexp (user : ((int * string) option, Pg_error.t) Result.t)];
  Pool.close pool
;;

let () =
  Command_unix.run (Command.async ~summary:"Look up user 42" (Command.Param.return main))
;;
