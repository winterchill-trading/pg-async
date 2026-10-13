open! Core
open! Async
open Pg_async
open Helpers

let%expect_test "select with typed params" =
  with_conn (fun conn ->
    let%bind r =
      Query.fetch_one
        conn
        "SELECT $1::int8 + 1 AS n, upper($2) AS s, $3::bool AS b"
        ~params:[ Param.int 41; Param.string "abc"; Param.bool true ]
        ~row:
          (let open Row.Let_syntax in
           let%map_open n = column "n" int
           and s = column "s" string
           and b = column "b" bool in
           n, s, b)
    in
    print_result [%sexp_of: int * string * bool] r;
    [%expect {| (Ok (42 ABC true)) |}];
    return ())
;;

let%expect_test "insert, update and fetch rows" =
  with_conn (fun conn ->
    let%bind () =
      Connection.execute_script
        conn
        "CREATE TABLE item (id bigint PRIMARY KEY, name text NOT NULL, note text)"
      >>| ok
    in
    let%bind inserted =
      Deferred.List.map
        ~how:`Sequential
        [ 1, "a", None; 2, "b", Some "x" ]
        ~f:(fun (id, name, note) ->
          Query.execute
            conn
            "INSERT INTO item (id, name, note) VALUES ($1, $2, $3)"
            ~params:[ Param.int id; Param.string name; Param.option Type.string note ]
          >>| ok)
    in
    let%bind updated =
      Query.execute conn "UPDATE item SET note = $1" ~params:[ Param.string "y" ] >>| ok
    in
    print_s [%message (inserted : int list) (updated : int)];
    [%expect {| ((inserted (1 1)) (updated 2)) |}];
    let%bind rows =
      Query.fetch_all
        conn
        "SELECT id, name, note FROM item ORDER BY id"
        ~row:
          Row.Let_syntax.(
            let%map_open id = column "id" int
            and name = column "name" string
            and note = column "note" (nullable string) in
            id, name, note)
      >>| ok
    in
    print_s [%sexp (rows : (int * string * string option) list)];
    [%expect {| ((1 a (y)) (2 b (y))) |}];
    let%bind none =
      Query.fetch_opt
        conn
        "SELECT name FROM item WHERE id = $1"
        ~params:[ Param.int 99 ]
        ~row:(Row.column "name" Type.string)
    in
    print_result [%sexp_of: string option] none;
    [%expect {| (Ok ()) |}];
    let%bind two =
      Query.fetch_one conn "SELECT name FROM item" ~row:(Row.column "name" Type.string)
    in
    print_result [%sexp_of: string] two;
    [%expect {| (Error (Row_count (expected "exactly one row") (got 2))) |}];
    return ())
;;

let%expect_test "server errors keep their fields" =
  with_conn (fun conn ->
    let%bind () =
      Connection.execute_script
        conn
        "CREATE TABLE u (email text CONSTRAINT u_email UNIQUE)"
      >>| ok
    in
    let insert () =
      Query.execute conn "INSERT INTO u VALUES ($1)" ~params:[ Param.string "a@b" ]
    in
    let%bind (_ : int) = insert () >>| ok in
    let%bind r = insert () in
    print_result [%sexp_of: int] r;
    [%expect
      {|
      (Error
       (Server
        ((severity ERROR) (sqlstate 23505)
         (message "duplicate key value violates unique constraint \"u_email\"")
         (detail "Key (email)=(a@b) already exists.") (schema public) (table u)
         (constraint_ u_email))))
      |}];
    print_s
      [%sexp (Result.error r |> Option.map ~f:Pg_error.is_unique_violation : bool option)];
    [%expect {| (true) |}];
    (* The connection stays usable after an error. *)
    let%bind n = Query.fetch_one conn "SELECT 1 AS one" ~row:(Row.nth 0 Type.int) in
    print_result [%sexp_of: int] n;
    [%expect {| (Ok 1) |}];
    return ())
;;

let%expect_test "decode errors name the column" =
  with_conn (fun conn ->
    let%bind r =
      Query.fetch_all
        conn
        "SELECT x AS amount FROM (VALUES ('1'), ('1.5')) v(x)"
        ~row:(Row.column "amount" Type.int)
    in
    print_result [%sexp_of: int list] r;
    [%expect
      {|
      (Error
       (Decode (row 1) (column amount)
        (message "int8: cannot read \"1.5\" as a 63-bit int")))
      |}];
    let%bind r =
      Query.fetch_all conn "SELECT NULL::int AS n" ~row:(Row.column "n" Type.int)
    in
    print_result [%sexp_of: int list] r;
    [%expect
      {|
      (Error
       (Decode (row 0) (column n)
        (message "int8: unexpected NULL (wrap the codec in Type.nullable)")))
      |}];
    let%bind r = Query.fetch_all conn "SELECT 1 AS n" ~row:(Row.column "m" Type.int) in
    print_result [%sexp_of: int list] r;
    [%expect
      {| (Error (Decode (row -1) (column m) (message "no such column in the result"))) |}];
    return ())
;;

let%expect_test "client timeout cancels the statement and keeps the connection" =
  with_conn (fun conn ->
    let%bind r =
      Query.execute conn "SELECT pg_sleep(5)" ~timeout:(Time_ns.Span.of_int_ms 200)
    in
    print_result [%sexp_of: int] r;
    [%expect {| (Error (Timeout 200ms)) |}];
    let%bind n = Query.fetch_one conn "SELECT 2" ~row:(Row.nth 0 Type.int) in
    print_s
      [%message
        (n : (int, Pg_error.t) Result.t) ~closed:(Connection.is_closed conn : bool)];
    [%expect {| ((n (Ok 2)) (closed false)) |}];
    return ())
;;

let%expect_test "server statement_timeout" =
  with_conn
    ~tweak:(fun c -> { c with statement_timeout = Some (Time_ns.Span.of_int_ms 100) })
    (fun conn ->
      let%bind r = Query.execute conn "SELECT pg_sleep(2)" in
      print_s [%sexp (Result.error r |> Option.bind ~f:Pg_error.sqlstate : string option)];
      [%expect {| (57014) |}];
      return ())
;;

let%expect_test "queries on one connection run in order" =
  with_conn (fun conn ->
    let%bind results =
      Deferred.List.init ~how:`Parallel 20 ~f:(fun i ->
        Query.fetch_one
          conn
          "SELECT $1::int8 * 2"
          ~params:[ Param.int i ]
          ~row:(Row.nth 0 Type.int)
        >>| ok)
    in
    print_s
      [%sexp (List.equal Int.equal results (List.init 20 ~f:(fun i -> i * 2)) : bool)];
    [%expect {| true |}];
    return ())
;;

let%expect_test "closed connection" =
  with_conn (fun conn ->
    let%bind () = Connection.close conn in
    let%bind r = Query.execute conn "SELECT 1" in
    print_result [%sexp_of: int] r;
    [%expect {| (Error Closed) |}];
    return ())
;;

let%expect_test "connect errors" =
  let%bind cluster = Pg_test.shared () in
  let config = { (Pg_test.config cluster) with dbname = Some "missing_db" } in
  let%bind r = Connection.connect config in
  (match r with
   | Ok _ -> print_endline "connected?"
   | Error (Connect msg) ->
     print_s
       [%sexp
         (String.is_substring msg ~substring:"database \"missing_db\" does not exist"
          : bool)]
   | Error e -> print_s [%sexp (e : Pg_error.t)]);
  [%expect {| true |}];
  (* verify-full is the default and the test cluster has no TLS. *)
  let config =
    Config.create ~host:"127.0.0.1" ~port:(Pg_test.port cluster) ~user:"postgres" ()
  in
  let%bind r = Connection.connect config in
  (match r with
   | Ok _ -> print_endline "connected?"
   | Error (Connect msg) ->
     print_s
       [%sexp (String.is_substring msg ~substring:"server does not support SSL" : bool)]
   | Error e -> print_s [%sexp (e : Pg_error.t)]);
  [%expect {| true |}];
  return ()
;;

let%expect_test "server notices do not stall the connection" =
  with_conn (fun conn ->
    let%bind r =
      Query.execute
        conn
        "DO $$ BEGIN RAISE NOTICE 'a notice'; RAISE WARNING 'a warning'; END $$"
    in
    print_result [%sexp_of: int] r;
    [%expect {| (Ok 0) |}];
    return ())
;;
