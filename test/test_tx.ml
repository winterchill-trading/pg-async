open! Core
open! Async
open Pg_async
open Helpers

let count conn =
  Query.fetch_one conn "SELECT count(*) FROM t" ~row:(Row.nth 0 Type.int) >>| ok
;;

let insert conn v = Query.execute conn "INSERT INTO t VALUES ($1)" ~params:[ Param.int v ]

let%expect_test "commit, rollback on error, commit_if" =
  with_conn (fun conn ->
    let%bind () = Connection.execute_script conn "CREATE TABLE t (v int8)" >>| ok in
    let%bind r = Tx.with_transaction conn ~f:(fun conn -> insert conn 1) in
    print_result [%sexp_of: int] r;
    [%expect {| (Ok 1) |}];
    let%bind r =
      Tx.with_transaction conn ~f:(fun conn ->
        let%bind (_ : int) = insert conn 2 >>| ok in
        Query.execute conn "INSERT INTO t VALUES ('not a number')")
    in
    print_s [%sexp (Result.error r |> Option.bind ~f:Pg_error.sqlstate : string option)];
    [%expect {| (22P02) |}];
    let%bind r =
      Tx.with_transaction conn ~commit_if:Result.is_ok ~f:(fun conn ->
        let%map (_ : int) = insert conn 3 >>| ok in
        Ok (Error "domain says no"))
    in
    print_s [%sexp (r : ((unit, string) Result.t, Pg_error.t) Result.t)];
    [%expect {| (Ok (Error "domain says no")) |}];
    let%bind n = count conn in
    print_s [%message (n : int) ~in_transaction:(Connection.in_transaction conn : bool)];
    [%expect {| ((n 1) (in_transaction false)) |}];
    return ())
;;

let%expect_test "an ignored failure inside the transaction fails the COMMIT" =
  with_conn (fun conn ->
    let%bind () = Connection.execute_script conn "CREATE TABLE t (v int8)" >>| ok in
    let%bind r =
      Tx.with_transaction conn ~f:(fun conn ->
        let%bind (_ : int) = insert conn 1 >>| ok in
        let%map (_ : (int, Pg_error.t) Result.t) = Query.execute conn "SELECT 1/0" in
        Ok ())
    in
    print_s [%sexp (Result.error r |> Option.bind ~f:Pg_error.sqlstate : string option)];
    [%expect {| (25P02) |}];
    let%bind n = count conn in
    print_s [%message (n : int) ~in_transaction:(Connection.in_transaction conn : bool)];
    [%expect {| ((n 0) (in_transaction false)) |}];
    return ())
;;

let%expect_test "an exception rolls back and propagates" =
  with_conn (fun conn ->
    let%bind () = Connection.execute_script conn "CREATE TABLE t (v int8)" >>| ok in
    let%bind r =
      Monitor.try_with ~extract_exn:true (fun () ->
        Tx.with_transaction conn ~f:(fun conn ->
          let%bind (_ : int) = insert conn 1 >>| ok in
          failwith "boom"))
    in
    print_s [%sexp (Result.is_error r : bool)];
    [%expect {| true |}];
    let%bind n = count conn in
    print_s [%message (n : int) ~in_transaction:(Connection.in_transaction conn : bool)];
    [%expect {| ((n 0) (in_transaction false)) |}];
    return ())
;;

let%expect_test "serialization failures are retried" =
  let%bind cluster = Pg_test.shared () in
  let%bind config = Pg_test.create_database cluster in
  let connect () = Connection.connect config >>| ok in
  let%bind a = connect () in
  let%bind b = connect () in
  let%bind () =
    Connection.execute_script
      a
      "CREATE TABLE counter (id int PRIMARY KEY, n int8); INSERT INTO counter VALUES (1, \
       0)"
    >>| ok
  in
  (* Both first attempts read the counter before either writes, so the second writer must
     fail with 40001 and succeed on its retry. *)
  let both_read = Ivar.create () in
  let reads = ref 0 in
  let attempts = ref 0 in
  let increment conn =
    Tx.with_transaction
      conn
      ~isolation:Serializable
      ~backoff:(fun ~attempt:_ -> Time_ns.Span.of_int_ms 1)
      ~f:(fun conn ->
        incr attempts;
        let%bind n =
          Query.fetch_one
            conn
            "SELECT n FROM counter WHERE id = 1"
            ~row:(Row.nth 0 Type.int)
          >>| ok
        in
        let%bind () =
          if Ivar.is_full both_read
          then return ()
          else (
            incr reads;
            if !reads = 2 then Ivar.fill_exn both_read ();
            Ivar.read both_read)
        in
        Query.execute
          conn
          "UPDATE counter SET n = $1 WHERE id = 1"
          ~params:[ Param.int (n + 1) ])
  in
  let%bind ra = increment a
  and rb = increment b in
  let%bind n =
    Query.fetch_one a "SELECT n FROM counter WHERE id = 1" ~row:(Row.nth 0 Type.int)
    >>| ok
  in
  print_s
    [%message
      (ra : (int, Pg_error.t) Result.t)
        (rb : (int, Pg_error.t) Result.t)
        (n : int)
        (attempts : int ref)];
  [%expect {| ((ra (Ok 1)) (rb (Ok 1)) (n 2) (attempts 3)) |}];
  let%bind () = Connection.close a in
  Connection.close b
;;

let%expect_test "retries stop at max_attempts" =
  with_conn (fun conn ->
    let attempts = ref 0 in
    let%bind r =
      Tx.with_transaction
        conn
        ~max_attempts:3
        ~backoff:(fun ~attempt:_ -> Time_ns.Span.zero)
        ~f:(fun conn ->
          incr attempts;
          (* RAISE with an explicit SQLSTATE stands in for a real conflict. *)
          Query.execute
            conn
            "DO $$ BEGIN RAISE EXCEPTION 'conflict' USING ERRCODE = \
             'serialization_failure'; END $$")
    in
    print_s
      [%message
        ""
          ~sqlstate:(Result.error r |> Option.bind ~f:Pg_error.sqlstate : string option)
          (attempts : int ref)];
    [%expect {| ((sqlstate (40001)) (attempts 3)) |}];
    return ())
;;
