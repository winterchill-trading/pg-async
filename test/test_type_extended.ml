(* interval, time, inet/cidr and range codecs. *)

open! Core
open! Async
open Pg_async
open Helpers
open Round_trip

let int_range = Gens.discrete_range Int.quickcheck_generator ~compare:Int.compare
let int32_range = Gens.discrete_range Int32.quickcheck_generator ~compare:Int32.compare
let date_range = Gens.discrete_range Gens.date ~compare:Date.compare
let tstz_range = Gens.continuous_range Gens.time_ns ~compare:Time_ns.compare

let%expect_test "codecs round trip without a server" =
  pure_round_trip
    ~name:"interval"
    Type.interval
    Gens.interval
    ~equal:Type.Interval.equal
    ~sexp_of:Type.Interval.sexp_of_t;
  pure_round_trip
    ~name:"time"
    Type.ofday
    Gens.ofday
    ~equal:Time_ns.Ofday.equal
    ~sexp_of:Time_ns.Ofday.sexp_of_t;
  pure_round_trip
    ~name:"inet"
    Type.inet
    Gens.inet
    ~equal:Type.Inet.equal
    ~sexp_of:Type.Inet.sexp_of_t;
  pure_round_trip
    ~name:"int8range"
    (Type.range Type.int)
    int_range
    ~equal:[%equal: int Type.Range.t]
    ~sexp_of:[%sexp_of: int Type.Range.t];
  pure_round_trip
    ~name:"tstzrange"
    (Type.range Type.time_ns)
    tstz_range
    ~equal:[%equal: Time_ns.t Type.Range.t]
    ~sexp_of:[%sexp_of: Time_ns.Alternate_sexp.t Type.Range.t];
  pure_round_trip
    ~name:"awkward text in a range literal"
    (* numeric's codec passes text through, so this exercises quoting only. *)
    (Type.range Type.numeric)
    (Gens.continuous_range Gens.utf8_string ~compare:String.compare)
    ~equal:[%equal: string Type.Range.t]
    ~sexp_of:[%sexp_of: string Type.Range.t];
  [%expect
    {|
    interval ok
    time ok
    inet ok
    int8range ok
    tstzrange ok
    awkward text in a range literal ok
    |}];
  return ()
;;

let%expect_test "codecs round trip through the server" =
  with_conn (fun conn ->
    let%bind () =
      server_round_trip
        conn
        ~name:"interval"
        Type.interval
        Gens.interval
        ~equal:Type.Interval.equal
        ~sexp_of:Type.Interval.sexp_of_t
    in
    let%bind () =
      server_round_trip
        conn
        ~name:"time"
        Type.ofday
        Gens.ofday
        ~equal:Time_ns.Ofday.equal
        ~sexp_of:Time_ns.Ofday.sexp_of_t
    in
    let%bind () =
      server_round_trip
        conn
        ~name:"inet"
        Type.inet
        Gens.inet
        ~equal:Type.Inet.equal
        ~sexp_of:Type.Inet.sexp_of_t
    in
    let%bind () =
      server_round_trip
        conn
        ~name:"cidr"
        Type.cidr
        Gens.cidr
        ~equal:Type.Inet.equal
        ~sexp_of:Type.Inet.sexp_of_t
    in
    let%bind () =
      server_round_trip
        conn
        ~name:"int8range"
        (Type.range Type.int)
        int_range
        ~equal:[%equal: int Type.Range.t]
        ~sexp_of:[%sexp_of: int Type.Range.t]
    in
    let%bind () =
      server_round_trip
        conn
        ~name:"int4range"
        (Type.range Type.int32)
        int32_range
        ~equal:[%equal: int32 Type.Range.t]
        ~sexp_of:[%sexp_of: int32 Type.Range.t]
    in
    let%bind () =
      server_round_trip
        conn
        ~name:"daterange"
        (Type.range Type.date)
        date_range
        ~equal:[%equal: Date.t Type.Range.t]
        ~sexp_of:[%sexp_of: Date.t Type.Range.t]
    in
    let%bind () =
      server_round_trip
        conn
        ~name:"tstzrange"
        (Type.range Type.time_ns)
        tstz_range
        ~equal:[%equal: Time_ns.t Type.Range.t]
        ~sexp_of:[%sexp_of: Time_ns.Alternate_sexp.t Type.Range.t]
    in
    let%bind () =
      server_round_trip
        conn
        ~name:"interval[]"
        (Type.array Type.interval)
        (List.quickcheck_generator Gens.interval)
        ~equal:[%equal: Type.Interval.t list]
        ~sexp_of:[%sexp_of: Type.Interval.t list]
    in
    let%bind () =
      server_round_trip
        conn
        ~name:"int8range[]"
        (Type.array (Type.range Type.int))
        (List.quickcheck_generator int_range)
        ~equal:[%equal: int Type.Range.t list]
        ~sexp_of:[%sexp_of: int Type.Range.t list]
    in
    [%expect
      {|
      interval ok
      time ok
      inet ok
      cidr ok
      int8range ok
      int4range ok
      daterange ok
      tstzrange ok
      interval[] ok
      int8range[] ok
      |}];
    return ())
;;

let%expect_test "server spellings" =
  with_conn (fun conn ->
    let show sql ty sexp_of =
      let%map r =
        Query.fetch_one conn ("SELECT " ^ sql ^ " AS v") ~row:(Row.column "v" ty)
      in
      print_s [%message sql ~_:(Result.map r ~f:sexp_of : (Sexp.t, Pg_error.t) Result.t)]
    in
    let interval sql = show sql Type.interval Type.Interval.sexp_of_t in
    let%bind () = interval "'1 year 2 months -3 days 04:05:06.789'::interval" in
    let%bind () = interval "'-1 day -00:00:00.000001'::interval" in
    let%bind () = interval "'0'::interval" in
    let%bind () = interval "'100 hours'::interval" in
    let%bind () = Query.execute conn "SET IntervalStyle = 'iso_8601'" >>| ok >>| ignore in
    let%bind () = interval "'1 year 2 months -3 days 04:05:06.789'::interval" in
    let%bind () = interval "'-1 day -00:00:00.000001'::interval" in
    let%bind () = interval "'0'::interval" in
    let%bind () = show "'24:00:00'::time" Type.ofday Time_ns.Ofday.sexp_of_t in
    let%bind () = show "'10.1.0.0/16'::cidr" Type.cidr Type.Inet.sexp_of_t in
    let%bind () = show "'::ffff:10.1.2.3'::inet" Type.inet Type.Inet.sexp_of_t in
    let%bind () =
      show "'[1,5]'::int8range" (Type.range Type.int) [%sexp_of: int Type.Range.t]
    in
    let%bind () =
      show "'(,)'::int8range" (Type.range Type.int) [%sexp_of: int Type.Range.t]
    in
    let%bind () =
      show "'[3,3)'::int8range" (Type.range Type.int) [%sexp_of: int Type.Range.t]
    in
    let%bind () =
      show
        "tstzrange('2026-01-01', 'infinity')"
        (Type.range Type.time_ns)
        [%sexp_of: Time_ns.Alternate_sexp.t Type.Range.t]
    in
    [%expect
      {|
      ("'1 year 2 months -3 days 04:05:06.789'::interval"
       (Ok ((months 14) (days -3) (microseconds 14706789000))))
      ("'-1 day -00:00:00.000001'::interval"
       (Ok ((months 0) (days -1) (microseconds -1))))
      ('0'::interval (Ok ((months 0) (days 0) (microseconds 0))))
      ("'100 hours'::interval"
       (Ok ((months 0) (days 0) (microseconds 360000000000))))
      ("'1 year 2 months -3 days 04:05:06.789'::interval"
       (Ok ((months 14) (days -3) (microseconds 14706789000))))
      ("'-1 day -00:00:00.000001'::interval"
       (Ok ((months 0) (days -1) (microseconds -1))))
      ('0'::interval (Ok ((months 0) (days 0) (microseconds 0))))
      ('24:00:00'::time (Ok 24:00:00.000000000))
      ('10.1.0.0/16'::cidr (Ok ((address 10.1.0.0) (prefix_length 16))))
      ('::ffff:10.1.2.3'::inet
       (Ok ((address ::ffff:10.1.2.3) (prefix_length 128))))
      ('[1,5]'::int8range
       (Ok (Non_empty (lower (Inclusive 1)) (upper (Exclusive 6)))))
      ("'(,)'::int8range" (Ok (Non_empty (lower Unbounded) (upper Unbounded))))
      ("'[3,3)'::int8range" (Ok Empty))
      ("tstzrange('2026-01-01', 'infinity')"
       (Error
        (Decode (row 0) (column v)
         (message "tstzrange: cannot read \"infinity\" as timestamptz"))))
      |}];
    return ())
;;
