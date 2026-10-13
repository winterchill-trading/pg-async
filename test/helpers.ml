open! Core
open! Async
open Pg_async

(* A connection to a fresh database in the per-process test cluster. *)
let with_conn ?metrics ?(tweak = Fn.id) f =
  let%bind cluster = Pg_test.shared () in
  let%bind config = Pg_test.create_database cluster in
  let%bind conn = Connection.connect ?metrics (tweak config) >>| Pg_error.ok_exn in
  Monitor.protect (fun () -> f conn) ~finally:(fun () -> Connection.close conn)
;;

let print_result sexp_of_a r = print_s [%sexp (r : (a, Pg_error.t) Result.t)]
let ok r = Pg_error.ok_exn r
