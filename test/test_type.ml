open! Core
open! Async
open Pg_async
open Helpers
open Round_trip

let%expect_test "codecs round trip without a server" =
  pure_round_trip
    ~name:"int"
    Type.int
    Int.quickcheck_generator
    ~equal:Int.equal
    ~sexp_of:Int.sexp_of_t;
  pure_round_trip
    ~name:"int32"
    Type.int32
    Int32.quickcheck_generator
    ~equal:Int32.equal
    ~sexp_of:Int32.sexp_of_t;
  pure_round_trip
    ~name:"int64"
    Type.int64
    Int64.quickcheck_generator
    ~equal:Int64.equal
    ~sexp_of:Int64.sexp_of_t;
  pure_round_trip
    ~name:"string"
    Type.string
    String.quickcheck_generator
    ~equal:String.equal
    ~sexp_of:String.sexp_of_t;
  pure_round_trip
    ~name:"bool"
    Type.bool
    Bool.quickcheck_generator
    ~equal:Bool.equal
    ~sexp_of:Bool.sexp_of_t;
  pure_round_trip
    ~name:"float"
    Type.float
    Gens.float
    ~equal:Gens.float_equal
    ~sexp_of:Float.sexp_of_t;
  pure_round_trip
    ~name:"time_ns"
    Type.time_ns
    Gens.time_ns
    ~equal:Time_ns.equal
    ~sexp_of:Time_ns.Alternate_sexp.sexp_of_t;
  pure_round_trip
    ~name:"date"
    Type.date
    Gens.date
    ~equal:Date.equal
    ~sexp_of:Date.sexp_of_t;
  pure_round_trip
    ~name:"bytea"
    Type.bytea
    String.quickcheck_generator
    ~equal:String.equal
    ~sexp_of:String.sexp_of_t;
  pure_round_trip
    ~name:"yojson"
    Type.yojson
    Gens.yojson
    ~equal:Yojson.Safe.equal
    ~sexp_of:(fun j -> Sexp.Atom (Yojson.Safe.to_string j));
  pure_round_trip
    ~name:"nullable int"
    (Type.nullable Type.int)
    (Option.quickcheck_generator Int.quickcheck_generator)
    ~equal:[%equal: int option]
    ~sexp_of:[%sexp_of: int option];
  pure_round_trip
    ~name:"text array with NULLs"
    (Type.array (Type.nullable Type.string))
    (List.quickcheck_generator (Option.quickcheck_generator String.quickcheck_generator))
    ~equal:[%equal: string option list]
    ~sexp_of:[%sexp_of: string option list];
  pure_round_trip
    ~name:"bytea array"
    (Type.array Type.bytea)
    (List.quickcheck_generator String.quickcheck_generator)
    ~equal:[%equal: string list]
    ~sexp_of:[%sexp_of: string list];
  [%expect
    {|
    int ok
    int32 ok
    int64 ok
    string ok
    bool ok
    float ok
    time_ns ok
    date ok
    bytea ok
    yojson ok
    nullable int ok
    text array with NULLs ok
    bytea array ok
    |}];
  return ()
;;

let%expect_test "array literals as the server writes them" =
  List.iter
    [ "{}"
    ; "{1,2,3}"
    ; "{NULL,null,\"NULL\"}"
    ; "{\"a b\",c,\"d\\\"e\",\"f\\\\g\"}"
    ; "{ 1 , 2 }"
    ; "{{1,2},{3,4}}"
    ; "[0:1]={1,2}"
    ; "{\"open"
    ]
    ~f:(fun s ->
      print_s
        [%sexp
          (s : string)
          , (Type.Array_literal.of_string s : (string option list, string) Result.t)]);
  [%expect
    {|
    ({} (Ok ()))
    ({1,2,3} (Ok ((1) (2) (3))))
    ("{NULL,null,\"NULL\"}" (Ok (() () (NULL))))
    ("{\"a b\",c,\"d\\\"e\",\"f\\\\g\"}" (Ok (("a b") (c) ("d\"e") ("f\\g"))))
    ("{ 1 , 2 }" (Ok ((1) (2))))
    ({{1,2},{3,4}}
     (Error "array literal \"{{1,2},{3,4}}\": nested arrays are not supported"))
    ([0:1]={1,2}
     (Error
      "array literal \"[0:1]={1,2}\": expected {...} (only one-dimensional arrays with default bounds)"))
    ("{\"open"
     (Error
      "array literal \"{\\\"open\": expected {...} (only one-dimensional arrays with default bounds)"))
    |}];
  return ()
;;

let%expect_test "timestamptz offsets" =
  List.iter
    [ "2026-10-13 12:00:00+00"
    ; "2026-10-13 12:00:00.5+00"
    ; "2026-10-13 17:30:00.123456+05:30"
    ; "1900-01-01 00:00:00-00:19:32"
    ; "2026-10-13T12:00:00Z"
    ; "infinity"
    ; "0044-03-15 12:00:00+00 BC"
    ]
    ~f:(fun s ->
      print_s
        [%sexp
          (s : string)
          , (Type.Timestamptz.of_string s : (Time_ns.Alternate_sexp.t, string) Result.t)]);
  [%expect
    {|
    ("2026-10-13 12:00:00+00" (Ok "2026-10-13 12:00:00Z"))
    ("2026-10-13 12:00:00.5+00" (Ok "2026-10-13 12:00:00.5Z"))
    ("2026-10-13 17:30:00.123456+05:30" (Ok "2026-10-13 12:00:00.123456Z"))
    ("1900-01-01 00:00:00-00:19:32" (Ok "1900-01-01 00:19:32Z"))
    (2026-10-13T12:00:00Z (Ok "2026-10-13 12:00:00Z"))
    (infinity (Error "cannot read \"infinity\" as timestamptz"))
    ("0044-03-15 12:00:00+00 BC"
     (Error "cannot read \"0044-03-15 12:00:00+00 BC\" as timestamptz"))
    |}];
  return ()
;;

let%expect_test "codecs round trip through the server" =
  with_conn (fun conn ->
    let%bind () =
      server_round_trip
        conn
        ~name:"int"
        Type.int
        Int.quickcheck_generator
        ~equal:Int.equal
        ~sexp_of:Int.sexp_of_t
    in
    let%bind () =
      server_round_trip
        conn
        ~name:"int32"
        Type.int32
        Int32.quickcheck_generator
        ~equal:Int32.equal
        ~sexp_of:Int32.sexp_of_t
    in
    let%bind () =
      server_round_trip
        conn
        ~name:"int64"
        Type.int64
        Int64.quickcheck_generator
        ~equal:Int64.equal
        ~sexp_of:Int64.sexp_of_t
    in
    let%bind () =
      server_round_trip
        conn
        ~name:"text"
        Type.string
        Gens.utf8_string
        ~equal:String.equal
        ~sexp_of:String.sexp_of_t
    in
    let%bind () =
      server_round_trip
        conn
        ~name:"bool"
        Type.bool
        Bool.quickcheck_generator
        ~equal:Bool.equal
        ~sexp_of:Bool.sexp_of_t
    in
    let%bind () =
      server_round_trip
        conn
        ~name:"float8"
        Type.float
        Gens.float
        ~equal:Gens.float_equal
        ~sexp_of:Float.sexp_of_t
    in
    let%bind () =
      server_round_trip
        conn
        ~name:"timestamptz"
        Type.time_ns
        Gens.time_ns
        ~equal:Time_ns.equal
        ~sexp_of:Time_ns.Alternate_sexp.sexp_of_t
    in
    let%bind () =
      server_round_trip
        conn
        ~name:"date"
        Type.date
        Gens.date
        ~equal:Date.equal
        ~sexp_of:Date.sexp_of_t
    in
    let%bind () =
      server_round_trip
        conn
        ~name:"bytea"
        Type.bytea
        String.quickcheck_generator
        ~equal:String.equal
        ~sexp_of:String.sexp_of_t
    in
    let%bind () =
      server_round_trip
        conn
        ~name:"uuid"
        Type.uuid
        Gens.uuid
        ~equal:String.equal
        ~sexp_of:String.sexp_of_t
    in
    let%bind () =
      server_round_trip
        conn
        ~name:"numeric"
        Type.numeric
        Gens.numeric
        ~equal:String.equal
        ~sexp_of:String.sexp_of_t
    in
    let%bind () =
      server_round_trip
        conn
        ~name:"jsonb"
        Type.yojson
        Gens.yojson
        ~equal:(fun a b ->
          Yojson.Safe.equal (Gens.canonical_json a) (Gens.canonical_json b))
        ~sexp_of:(fun j -> Sexp.Atom (Yojson.Safe.to_string j))
    in
    let%bind () =
      server_round_trip
        conn
        ~name:"nullable int"
        (Type.nullable Type.int)
        (Option.quickcheck_generator Int.quickcheck_generator)
        ~equal:[%equal: int option]
        ~sexp_of:[%sexp_of: int option]
    in
    let%bind () =
      server_round_trip
        conn
        ~name:"text[] with NULLs"
        (Type.array (Type.nullable Type.string))
        (List.quickcheck_generator (Option.quickcheck_generator Gens.utf8_string))
        ~equal:[%equal: string option list]
        ~sexp_of:[%sexp_of: string option list]
    in
    let%bind () =
      server_round_trip
        conn
        ~name:"int8[]"
        (Type.array Type.int)
        (List.quickcheck_generator Int.quickcheck_generator)
        ~equal:[%equal: int list]
        ~sexp_of:[%sexp_of: int list]
    in
    let%bind () =
      server_round_trip
        conn
        ~name:"bytea[]"
        (Type.array Type.bytea)
        (List.quickcheck_generator String.quickcheck_generator)
        ~equal:[%equal: string list]
        ~sexp_of:[%sexp_of: string list]
    in
    let%bind () =
      server_round_trip
        conn
        ~name:"timestamptz[]"
        (Type.array Type.time_ns)
        (List.quickcheck_generator Gens.time_ns)
        ~equal:[%equal: Time_ns.t list]
        ~sexp_of:[%sexp_of: Time_ns.Alternate_sexp.t list]
    in
    let%bind () =
      server_round_trip
        conn
        ~name:"float8[] with NULLs"
        (Type.array (Type.nullable Type.float))
        (List.quickcheck_generator (Option.quickcheck_generator Gens.float))
        ~equal:(List.equal (Option.equal Gens.float_equal))
        ~sexp_of:[%sexp_of: float option list]
    in
    let%bind () =
      server_round_trip
        conn
        ~name:"jsonb[]"
        (Type.array Type.yojson)
        (List.quickcheck_generator Gens.yojson)
        ~trials:50
        ~equal:
          (List.equal (fun a b ->
             Yojson.Safe.equal (Gens.canonical_json a) (Gens.canonical_json b)))
        ~sexp_of:(fun l -> [%sexp (List.map l ~f:Yojson.Safe.to_string : string list)])
    in
    [%expect
      {|
      int ok
      int32 ok
      int64 ok
      text ok
      bool ok
      float8 ok
      timestamptz ok
      date ok
      bytea ok
      uuid ok
      numeric ok
      jsonb ok
      nullable int ok
      text[] with NULLs ok
      int8[] ok
      bytea[] ok
      timestamptz[] ok
      float8[] with NULLs ok
      jsonb[] ok
      |}];
    return ())
;;

let%expect_test "int4 and int2 columns read as int; int8 overflow is reported" =
  with_conn (fun conn ->
    let%bind r =
      Query.fetch_one
        conn
        "SELECT 7::int2 AS a, 8::int4 AS b, 9::int8 AS c"
        ~row:
          Row.Let_syntax.(
            let%map_open a = column "a" int
            and b = column "b" int
            and c = column "c" int in
            a, b, c)
    in
    print_result [%sexp_of: int * int * int] r;
    [%expect {| (Ok (7 8 9)) |}];
    let%bind r =
      Query.fetch_one
        conn
        "SELECT 9223372036854775807::int8 AS big"
        ~row:(Row.column "big" Type.int)
    in
    print_result [%sexp_of: int] r;
    [%expect
      {|
      (Error
       (Decode (row 0) (column big)
        (message "int8: cannot read \"9223372036854775807\" as a 63-bit int")))
      |}];
    return ())
;;
