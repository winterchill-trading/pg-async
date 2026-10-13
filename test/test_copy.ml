open! Core
open! Async
open Pg_async
open Helpers

let row_t =
  let open Row.Let_syntax in
  let%map_open id = column "id" int
  and note = column "note" (nullable string)
  and blob = column "blob" bytea
  and at = column "at" time_ns
  and tags = column "tags" (array (nullable string)) in
  id, note, blob, at, tags
;;

type row = int * string option * string * Time_ns.Alternate_sexp.t * string option list
[@@deriving sexp_of, equal]

let%expect_test "COPY round trips awkward values" =
  with_conn (fun conn ->
    let%bind () =
      Connection.execute_script
        conn
        "CREATE TABLE \"Odd Table\" (id int8, note text, blob bytea, at timestamptz, \
         tags text[])"
      >>| ok
    in
    let rows : row list =
      Quickcheck.random_value
        ~seed:(`Deterministic "copy")
        (List.gen_with_length
           5_000
           (Quickcheck.Generator.tuple4
              (Option.quickcheck_generator Gens.utf8_string)
              String.quickcheck_generator
              Gens.time_ns
              (List.quickcheck_generator (Option.quickcheck_generator Gens.utf8_string))))
      |> List.mapi ~f:(fun i (note, blob, at, tags) -> i, note, blob, at, tags)
    in
    let%bind n =
      Copy.from_pipe
        conn
        ~table:"Odd Table"
        ~columns:[ "id"; "note"; "blob"; "at"; "tags" ]
        (Pipe.of_list
           (List.map rows ~f:(fun (id, note, blob, at, tags) ->
              [ Param.int id
              ; Param.option Type.string note
              ; Param.bytea blob
              ; Param.time_ns at
              ; Param.array (Type.nullable Type.string) tags
              ])))
    in
    print_result [%sexp_of: int] n;
    [%expect {| (Ok 5000) |}];
    let%bind back =
      Query.fetch_all conn "SELECT * FROM \"Odd Table\" ORDER BY id" ~row:row_t >>| ok
    in
    print_s [%sexp (List.equal equal_row rows back : bool)];
    [%expect {| true |}];
    return ())
;;

let%expect_test "COPY errors leave nothing behind and the connection usable" =
  with_conn (fun conn ->
    let%bind () =
      Connection.execute_script conn "CREATE TABLE t (a int8, b text)" >>| ok
    in
    let count () =
      Query.fetch_one conn "SELECT count(*) FROM t" ~row:(Row.nth 0 Type.int)
    in
    let%bind r =
      Copy.from_list
        conn
        ~table:"t"
        ~columns:[ "a"; "b" ]
        [ [ Param.int 1; Param.string "ok" ]
        ; [ Param.string "x"; Param.string "bad int" ]
        ]
    in
    print_s [%sexp (Result.error r |> Option.bind ~f:Pg_error.sqlstate : string option)];
    [%expect {| (22P02) |}];
    let%bind r =
      Copy.from_list
        conn
        ~table:"t"
        ~columns:[ "a"; "b" ]
        [ [ Param.int 1; Param.string "ok" ]; [ Param.int 2 ] ]
    in
    print_result [%sexp_of: int] r;
    [%expect
      {| (Error (Protocol "COPY row has 1 values for 2 columns; nothing was loaded")) |}];
    let%bind r =
      Copy.from_list conn ~table:"missing" ~columns:[ "a" ] [ [ Param.int 1 ] ]
    in
    print_s [%sexp (Result.error r |> Option.bind ~f:Pg_error.sqlstate : string option)];
    [%expect {| (42P01) |}];
    let%bind n = count () in
    print_result [%sexp_of: int] n;
    [%expect {| (Ok 0) |}];
    return ())
;;
