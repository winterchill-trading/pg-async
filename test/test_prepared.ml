(* The per-connection prepared-statement cache and binary results. *)

open! Core
open! Async
open Pg_async
open Helpers

let server_statements conn =
  Query.fetch_one
    conn
    "SELECT count(*) AS n FROM pg_prepared_statements"
    ~row:(Row.column "n" Type.int)
  >>| ok
;;

(* The raw result format for [sql], through the same path [Query] uses. *)
let formats conn sql ~params =
  let encoded = List.map params ~f:Param.encode in
  let%map r =
    Connection.Private.exec
      conn
      ~sql
      ~param_types:(Array.of_list_map encoded ~f:fst)
      ~params:(Array.of_list_map encoded ~f:snd)
      ~binary_result:(fun ~names:_ ~oids:_ -> true)
    >>| ok
  in
  if r#binary_tuples then "binary" else "text"
;;

let%expect_test "statements are prepared once; results turn binary on the second run" =
  with_conn (fun conn ->
    let sql = "SELECT $1::int8 + 1 AS n" in
    let%bind first = formats conn sql ~params:[ Param.int 1 ] in
    let%bind second = formats conn sql ~params:[ Param.int 2 ] in
    let%bind n = server_statements conn in
    print_s
      [%message
        first second (Connection.prepared_statements conn : int) ~on_server:(n : int)];
    [%expect {| (text binary ("Connection.prepared_statements conn" 1) (on_server 1)) |}];
    (* Without parameters: the simple protocol, text, nothing cached. *)
    let%bind simple = formats conn "SELECT 1 AS n" ~params:[] in
    print_s [%message simple (Connection.prepared_statements conn : int)];
    [%expect {| (text ("Connection.prepared_statements conn" 1)) |}];
    return ())
;;

let%expect_test "a decoder with a text-only codec keeps the query in text" =
  with_conn (fun conn ->
    let run () =
      Query.fetch_one
        conn
        "SELECT $1::numeric AS a, $2::int8 AS b"
        ~params:[ Param.numeric "1.50"; Param.int 7 ]
        ~row:
          Row.Let_syntax.(
            let%map_open a = column "a" numeric
            and b = column "b" int in
            a, b)
    in
    let%bind r1 = run () in
    let%bind r2 = run () in
    print_s [%sexp (r1 : (string * int, Pg_error.t) Result.t)];
    print_s [%sexp (r2 : (string * int, Pg_error.t) Result.t)];
    [%expect {|
      (Ok (1.50 7))
      (Ok (1.50 7))
      |}];
    return ())
;;

let%expect_test "text and binary decode to the same values" =
  with_conn (fun conn ->
    let both name sql ty sexp_of =
      let get () =
        Query.fetch_one
          conn
          ("SELECT " ^ sql ^ " AS v WHERE $1::int IS NOT NULL")
          ~params:[ Param.int 1 ]
          ~row:(Row.column "v" ty)
      in
      let%bind text = get () in
      let%map binary = get () in
      let show r = Result.map r ~f:sexp_of in
      match show text, show binary with
      | Ok a, Ok b when Sexp.equal a b -> print_s [%message name ~_:(a : Sexp.t)]
      | Error _, Error _ ->
        (* The messages differ between the formats; both refusing is what matters. *)
        print_s [%message name "rejected in both formats"]
      | _ ->
        print_s
          [%message
            "MISMATCH"
              name
              ~text:(show text : (Sexp.t, Pg_error.t) Result.t)
              ~binary:(show binary : (Sexp.t, Pg_error.t) Result.t)]
    in
    let%bind () = both "int2" "(-7)::int2" Type.int [%sexp_of: int] in
    let%bind () =
      both "int8 max" "9223372036854775807::int8" Type.int64 [%sexp_of: int64]
    in
    let%bind () =
      both "int8 too big" "9223372036854775807::int8" Type.int [%sexp_of: int]
    in
    let%bind () = both "float8 nan" "'NaN'::float8" Type.float [%sexp_of: float] in
    let%bind () = both "bpchar" "'ab'::char(4)" Type.string [%sexp_of: string] in
    let%bind () = both "bool" "true" Type.bool [%sexp_of: bool] in
    let%bind () =
      both
        "timestamptz"
        "'2026-10-14 12:00:00.123456+02'::timestamptz"
        Type.time_ns
        [%sexp_of: Time_ns.Alternate_sexp.t]
    in
    let%bind () =
      both
        "timestamptz infinity"
        "'infinity'::timestamptz"
        Type.time_ns
        [%sexp_of: Time_ns.Alternate_sexp.t]
    in
    let%bind () = both "date" "'1999-12-31'::date" Type.date [%sexp_of: Date.t] in
    let%bind () = both "date BC" "'0044-03-15 BC'::date" Type.date [%sexp_of: Date.t] in
    let%bind () = both "bytea" "'\\x00ff10'::bytea" Type.bytea [%sexp_of: string] in
    let%bind () =
      both "json" "'{\"b\": 1, \"a\": [2]}'::json" Type.json [%sexp_of: string]
    in
    let%bind () =
      both "jsonb" "'{\"b\": 1, \"a\": [2]}'::jsonb" Type.json [%sexp_of: string]
    in
    let%bind () =
      both
        "uuid"
        "'A0EEBC99-9C0B-4EF8-BB6D-6BB9BD380A11'::uuid"
        Type.uuid
        [%sexp_of: string]
    in
    let%bind () =
      both
        "interval"
        "'1 year -2 days 03:04:05.000006'::interval"
        Type.interval
        Type.Interval.sexp_of_t
    in
    let%bind () =
      both "time" "'23:59:59.999999'::time" Type.ofday Time_ns.Ofday.sexp_of_t
    in
    let%bind () =
      both "inet" "'::ffff:10.1.2.3/120'::inet" Type.inet Type.Inet.sexp_of_t
    in
    let%bind () =
      both
        "int8[] with NULL"
        "ARRAY[1, NULL, 3]::int8[]"
        (Type.array (Type.nullable Type.int))
        [%sexp_of: int option list]
    in
    let%bind () =
      both
        "empty text[]"
        "ARRAY[]::text[]"
        (Type.array Type.string)
        [%sexp_of: string list]
    in
    let%bind () =
      both
        "array with bounds"
        "'[0:1]={1,2}'::int8[]"
        (Type.array Type.int)
        [%sexp_of: int list]
    in
    let%bind () =
      both
        "daterange"
        "daterange('2026-01-01', NULL)"
        (Type.range Type.date)
        [%sexp_of: Date.t Type.Range.t]
    in
    let%bind () =
      both
        "empty range"
        "'empty'::int8range"
        (Type.range Type.int)
        [%sexp_of: int Type.Range.t]
    in
    [%expect
      {|
      (int2 -7)
      ("int8 max" 9223372036854775807)
      ("int8 too big" "rejected in both formats")
      ("float8 nan" NAN)
      (bpchar "ab  ")
      (bool true)
      (timestamptz "2026-10-14 10:00:00.123456Z")
      ("timestamptz infinity" "rejected in both formats")
      (date 1999-12-31)
      ("date BC" "rejected in both formats")
      (bytea "\000\255\016")
      (json "{\"b\": 1, \"a\": [2]}")
      (jsonb "{\"a\": [2], \"b\": 1}")
      (uuid a0eebc99-9c0b-4ef8-bb6d-6bb9bd380a11)
      (interval ((months 12) (days -2) (microseconds 11045000006)))
      (time 23:59:59.999999000)
      (inet ((address ::ffff:10.1.2.3) (prefix_length 120)))
      ("int8[] with NULL" ((1) () (3)))
      ("empty text[]" ())
      ("array with bounds" "rejected in both formats")
      (daterange (Non_empty (lower (Inclusive 2026-01-01)) (upper Unbounded)))
      ("empty range" Empty)
      |}];
    return ())
;;

let%expect_test "a table change under a cached statement re-prepares it" =
  with_conn (fun conn ->
    let%bind () =
      Connection.execute_script conn "CREATE TABLE t (a int8); INSERT INTO t VALUES (1)"
      >>| ok
    in
    let select () =
      Query.fetch_all
        conn
        "SELECT * FROM t WHERE a >= $1"
        ~params:[ Param.int 0 ]
        ~row:(Row.nth 0 Type.int)
    in
    let%bind r = select () in
    print_s [%sexp (r : (int list, Pg_error.t) Result.t)];
    let%bind () =
      Connection.execute_script conn "ALTER TABLE t ADD COLUMN b text" >>| ok
    in
    let%bind r = select () in
    print_s [%sexp (r : (int list, Pg_error.t) Result.t)];
    [%expect {|
      (Ok (1))
      (Ok (1))
      |}];
    (* Inside a transaction the failure aborts the transaction, so it is returned. *)
    let%bind () =
      Connection.execute_script conn "ALTER TABLE t ADD COLUMN c text" >>| ok
    in
    let%bind r =
      Tx.with_transaction conn ~f:(fun conn ->
        let%bind (_ : int) = Query.execute conn "SELECT 1" >>| ok in
        select ())
    in
    print_s
      [%sexp
        (Result.map_error r ~f:Pg_error.sqlstate : (int list, string option) Result.t)];
    let%bind r = select () in
    print_s [%sexp (r : (int list, Pg_error.t) Result.t)];
    [%expect {|
      (Error (0A000))
      (Ok (1))
      |}];
    return ())
;;

let%expect_test "statements dropped on the server are prepared again" =
  with_conn (fun conn ->
    let select () =
      Query.fetch_one
        conn
        "SELECT $1::int8 AS n"
        ~params:[ Param.int 5 ]
        ~row:(Row.column "n" Type.int)
    in
    let%bind (_ : int) = select () >>| ok in
    (* Not seen by the cache: a plain DEALLOCATE by name. *)
    let%bind () = Connection.execute_script conn "DEALLOCATE pg_async_1" >>| ok in
    let%bind r = select () in
    print_s [%sexp (r : (int, Pg_error.t) Result.t)];
    (* Seen: DISCARD ALL clears the cache. *)
    let%bind () = Connection.execute_script conn "DISCARD ALL" >>| ok in
    print_s [%message (Connection.prepared_statements conn : int)];
    let%bind r = select () in
    print_s [%sexp (r : (int, Pg_error.t) Result.t)];
    [%expect
      {|
      (Ok 5)
      ("Connection.prepared_statements conn" 0)
      (Ok 5)
      |}];
    return ())
;;

let%expect_test "the least recently used statement is evicted and deallocated" =
  with_conn
    ~tweak:(fun c -> { c with statement_cache_size = 2 })
    (fun conn ->
      let run k =
        Query.fetch_one
          conn
          (sprintf "SELECT $1::int8 + %d AS n" k)
          ~params:[ Param.int 1 ]
          ~row:(Row.column "n" Type.int)
        >>| ok
      in
      let%bind (_ : int list) =
        Deferred.List.map ~how:`Sequential [ 1; 2; 1; 3 ] ~f:run
      in
      let%bind n = server_statements conn in
      print_s [%message (Connection.prepared_statements conn : int) ~on_server:(n : int)];
      (* The DEALLOCATE goes out before the next prepare. *)
      let%bind (_ : int) = run 4 in
      let%bind n = server_statements conn in
      print_s [%message (Connection.prepared_statements conn : int) ~on_server:(n : int)];
      [%expect
        {|
        (("Connection.prepared_statements conn" 2) (on_server 2))
        (("Connection.prepared_statements conn" 2) (on_server 2))
        |}];
      return ())
;;

let%expect_test "statement_cache_size = 0 never prepares" =
  with_conn
    ~tweak:(fun c -> { c with statement_cache_size = 0 })
    (fun conn ->
      let sql = "SELECT $1::int8 AS n" in
      let%bind a = formats conn sql ~params:[ Param.int 1 ] in
      let%bind b = formats conn sql ~params:[ Param.int 1 ] in
      let%bind n = server_statements conn in
      print_s [%message a b ~on_server:(n : int)];
      [%expect {| (text text (on_server 0)) |}];
      return ())
;;
