open! Core
open! Async
open Pg_async
open Helpers

let%expect_test "histogram and Prometheus output" =
  let m = Metrics.create () in
  List.iter [ 50; 300; 300; 2_000; 20_000_000 ] ~f:(fun us ->
    Metrics.record m ~label:"insert" ~latency:(Time_ns.Span.of_int_us us) ~ok:true);
  Metrics.record m ~label:"select \"x\"" ~latency:(Time_ns.Span.of_int_ms 1) ~ok:false;
  List.iter (Metrics.snapshot m) ~f:(fun s ->
    print_s
      [%message
        s.label
          (s.count : int)
          (s.errors : int)
          ~p50:(Metrics.Label_stats.quantile_upper_bound s 0.5 : Time_ns.Span.t option)
          ~p99:(Metrics.Label_stats.quantile_upper_bound s 0.99 : Time_ns.Span.t option)]);
  [%expect
    {|
    (insert (s.count 5) (s.errors 0) (p50 (500us)) (p99 ()))
    ("select \"x\"" (s.count 1) (s.errors 1) (p50 (1ms)) (p99 (1ms)))
    |}];
  print_string (Metrics.to_prometheus m);
  [%expect
    {|
    # TYPE pg_queries_total counter
    pg_queries_total{label="insert"} 5
    pg_queries_total{label="select \"x\""} 1
    # TYPE pg_query_errors_total counter
    pg_query_errors_total{label="insert"} 0
    pg_query_errors_total{label="select \"x\""} 1
    # TYPE pg_query_duration_seconds histogram
    pg_query_duration_seconds_bucket{label="insert",le="0.0001"} 1
    pg_query_duration_seconds_bucket{label="insert",le="0.00025"} 1
    pg_query_duration_seconds_bucket{label="insert",le="0.0005"} 3
    pg_query_duration_seconds_bucket{label="insert",le="0.001"} 3
    pg_query_duration_seconds_bucket{label="insert",le="0.0025"} 4
    pg_query_duration_seconds_bucket{label="insert",le="0.005"} 4
    pg_query_duration_seconds_bucket{label="insert",le="0.01"} 4
    pg_query_duration_seconds_bucket{label="insert",le="0.025"} 4
    pg_query_duration_seconds_bucket{label="insert",le="0.05"} 4
    pg_query_duration_seconds_bucket{label="insert",le="0.1"} 4
    pg_query_duration_seconds_bucket{label="insert",le="0.25"} 4
    pg_query_duration_seconds_bucket{label="insert",le="0.5"} 4
    pg_query_duration_seconds_bucket{label="insert",le="1"} 4
    pg_query_duration_seconds_bucket{label="insert",le="2.5"} 4
    pg_query_duration_seconds_bucket{label="insert",le="5"} 4
    pg_query_duration_seconds_bucket{label="insert",le="10"} 4
    pg_query_duration_seconds_bucket{label="insert",le="+Inf"} 5
    pg_query_duration_seconds_sum{label="insert"} 20.002650
    pg_query_duration_seconds_count{label="insert"} 5
    pg_query_duration_seconds_bucket{label="select \"x\"",le="0.0001"} 0
    pg_query_duration_seconds_bucket{label="select \"x\"",le="0.00025"} 0
    pg_query_duration_seconds_bucket{label="select \"x\"",le="0.0005"} 0
    pg_query_duration_seconds_bucket{label="select \"x\"",le="0.001"} 1
    pg_query_duration_seconds_bucket{label="select \"x\"",le="0.0025"} 1
    pg_query_duration_seconds_bucket{label="select \"x\"",le="0.005"} 1
    pg_query_duration_seconds_bucket{label="select \"x\"",le="0.01"} 1
    pg_query_duration_seconds_bucket{label="select \"x\"",le="0.025"} 1
    pg_query_duration_seconds_bucket{label="select \"x\"",le="0.05"} 1
    pg_query_duration_seconds_bucket{label="select \"x\"",le="0.1"} 1
    pg_query_duration_seconds_bucket{label="select \"x\"",le="0.25"} 1
    pg_query_duration_seconds_bucket{label="select \"x\"",le="0.5"} 1
    pg_query_duration_seconds_bucket{label="select \"x\"",le="1"} 1
    pg_query_duration_seconds_bucket{label="select \"x\"",le="2.5"} 1
    pg_query_duration_seconds_bucket{label="select \"x\"",le="5"} 1
    pg_query_duration_seconds_bucket{label="select \"x\"",le="10"} 1
    pg_query_duration_seconds_bucket{label="select \"x\"",le="+Inf"} 1
    pg_query_duration_seconds_sum{label="select \"x\""} 0.001000
    pg_query_duration_seconds_count{label="select \"x\""} 1
    |}];
  return ()
;;

let%expect_test "connections record queries by label" =
  let metrics = Metrics.create () in
  with_conn ~metrics (fun conn ->
    let%bind (_ : int) = Query.execute ~label:"one" conn "SELECT 1" >>| ok in
    let%bind (_ : int) = Query.execute ~label:"one" conn "SELECT 1" >>| ok in
    let%bind (_ : (int, Pg_error.t) Result.t) = Query.execute ~label:"bad" conn "SELEC" in
    List.iter (Metrics.snapshot metrics) ~f:(fun s ->
      print_s [%message s.label (s.count : int) (s.errors : int)]);
    [%expect
      {|
      (bad (s.count 1) (s.errors 1))
      (one (s.count 2) (s.errors 0))
      |}];
    return ())
;;
