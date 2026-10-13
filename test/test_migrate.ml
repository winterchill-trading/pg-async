open! Core
open! Async
open Pg_async
open Helpers

let m version name sql = { Migrate.Migration.version; name; sql }

let base =
  [ m
      1
      "create_users"
      "CREATE TABLE users (id bigserial PRIMARY KEY, email text NOT NULL)"
  ; m 2 "add_index" "CREATE UNIQUE INDEX users_email ON users (email)"
  ]
;;

let unwrap r = Result.map_error r ~f:Migrate.Error.to_error |> Or_error.ok_exn
let print_apply r = print_s [%sexp (r : (int list, Migrate.Error.t) Result.t)]

let print_status conn migrations =
  let%map r = Migrate.status conn migrations in
  (* Hide applied_at, it is wall-clock time. *)
  Result.map
    r
    ~f:
      (List.map ~f:(fun (s : Migrate.Status.t) ->
         let state =
           match s.state with
           | Applied _ -> "applied"
           | Pending -> "pending"
           | Checksum_mismatch -> "checksum mismatch"
         in
         s.version, s.name, state))
  |> [%sexp_of: ((int * string * string) list, Migrate.Error.t) Result.t]
  |> print_s
;;

let%expect_test "apply, re-apply, add one" =
  with_conn (fun conn ->
    let%bind () = print_status conn base in
    [%expect {| (Ok ((1 create_users pending) (2 add_index pending))) |}];
    let%bind r = Migrate.apply conn base in
    print_apply r;
    [%expect {| (Ok (1 2)) |}];
    let%bind r = Migrate.apply conn base in
    print_apply r;
    [%expect {| (Ok ()) |}];
    let next = base @ [ m 3 "add_name" "ALTER TABLE users ADD COLUMN name text" ] in
    let%bind r = Migrate.apply conn next in
    print_apply r;
    [%expect {| (Ok (3)) |}];
    let%bind () = print_status conn next in
    [%expect
      {| (Ok ((1 create_users applied) (2 add_index applied) (3 add_name applied))) |}];
    return ())
;;

let%expect_test "verification failures stop the run" =
  with_conn (fun conn ->
    let%bind (_ : int list) = Migrate.apply conn base >>| unwrap in
    let edited =
      [ List.hd_exn base; m 2 "add_index" "CREATE INDEX users_email ON users (email)" ]
    in
    let%bind r = Migrate.apply conn edited in
    print_apply r;
    [%expect
      {|
      (Error
       (Checksum_mismatch (version 2) (name add_index)
        (applied ba881d27639286d6a1a160900b630bde)
        (file b46624dff24ac00289f92c39155fd17d)))
      |}];
    let%bind r = Migrate.apply conn [ List.hd_exn base ] in
    print_apply r;
    [%expect {| (Error (Missing (version 2) (name add_index))) |}];
    let%bind r = Migrate.apply conn (base @ [ m 0 "early" "SELECT 1" ]) in
    print_apply r;
    [%expect {| (Error (Out_of_order (version 0) (latest_applied 2))) |}];
    let%bind r = Migrate.apply conn (base @ [ m 2 "again" "SELECT 1" ]) in
    print_apply r;
    [%expect {| (Error (Invalid "version 2 used twice (add_index, again)")) |}];
    return ())
;;

let%expect_test "a failing migration applies nothing" =
  with_conn (fun conn ->
    let%bind r =
      Migrate.apply conn (base @ [ m 3 "broken" "ALTER TABLE nope ADD COLUMN x int" ])
    in
    (match r with
     | Error (Pg (Server { sqlstate; _ })) -> print_s [%message (sqlstate : string)]
     | r -> print_apply r);
    [%expect {| (sqlstate 42P01) |}];
    let%bind () = print_status conn base in
    [%expect {| (Ok ((1 create_users pending) (2 add_index pending))) |}];
    return ())
;;

let%expect_test "concurrent migrators apply once" =
  let%bind cluster = Pg_test.shared () in
  let%bind config = Pg_test.create_database cluster in
  let%bind a = Connection.connect config >>| ok in
  let%bind b = Connection.connect config >>| ok in
  let%bind ra = Migrate.apply a base
  and rb = Migrate.apply b base in
  let applied = List.sort ~compare:[%compare: int list] [ unwrap ra; unwrap rb ] in
  print_s [%sexp (applied : int list list)];
  [%expect {| (() (1 2)) |}];
  let%bind () = Connection.close a in
  Connection.close b
;;

let%expect_test "load_dir" =
  let dir = Filename_unix.temp_dir "pg-async-migrations" "" in
  let write file contents = Writer.save (dir ^/ file) ~contents in
  let%bind () = write "0002_second.sql" "SELECT 2" in
  let%bind () = write "0001_first.sql" "SELECT 1" in
  let%bind () = write "README.md" "ignored" in
  let%bind () = write "10.sql" "SELECT 10" in
  let%bind r = Migrate.load_dir dir in
  print_s [%sexp (r : (Migrate.Migration.t list, Migrate.Error.t) Result.t)];
  [%expect
    {|
    (Ok
     (((version 1) (name first)) ((version 2) (name second))
      ((version 10) (name ""))))
    |}];
  let%bind () = write "0001_dup.sql" "SELECT 1" in
  let%bind r = Migrate.load_dir dir in
  (match r with
   | Error (Invalid msg) -> print_endline msg
   | _ -> print_endline "expected a duplicate error");
  [%expect {| version 1 used twice (dup, first) |}];
  let%map (_ : string) = Process.run_exn ~prog:"rm" ~args:[ "-rf"; dir ] () in
  ()
;;
