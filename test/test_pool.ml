open! Core
open! Async
open Pg_async
open Helpers

let with_pool
  ?max_size
  ?min_size
  ?max_lifetime
  ?max_idle_time
  ?acquire_timeout
  ?health_check_after
  f
  =
  let%bind cluster = Pg_test.shared () in
  let%bind config = Pg_test.create_database cluster in
  let pool =
    Pool.create
      ?max_size
      ?min_size
      ?max_lifetime
      ?max_idle_time
      ?acquire_timeout
      ?health_check_after
      config
  in
  Monitor.protect (fun () -> f pool) ~finally:(fun () -> Pool.close pool)
;;

let pid_row = Row.nth 0 Type.int

let%expect_test "concurrency is bounded by max_size" =
  with_pool ~max_size:3 (fun pool ->
    let%bind pids =
      Deferred.List.init ~how:`Parallel 20 ~f:(fun _ ->
        Pool.fetch_one pool "SELECT pg_backend_pid() FROM pg_sleep(0.02)" ~row:pid_row
        >>| ok)
    in
    let distinct = List.dedup_and_sort pids ~compare:Int.compare |> List.length in
    print_s [%message (distinct : int) ~stats:(Pool.stats pool : Pool.Stats.t)];
    [%expect
      {|
      ((distinct 3)
       (stats ((size 3) (idle 3) (waiting 0) (opened 3) (discarded 0))))
      |}];
    return ())
;;

let%expect_test "waiting longer than acquire_timeout fails" =
  with_pool ~max_size:1 ~acquire_timeout:(Time_ns.Span.of_int_ms 100) (fun pool ->
    let release = Ivar.create () in
    let holder =
      Pool.with_connection pool ~f:(fun _ ->
        let%map () = Ivar.read release in
        Ok ())
    in
    let%bind r = Pool.execute pool "SELECT 1" in
    print_result [%sexp_of: int] r;
    [%expect {| (Error (Timeout 100ms)) |}];
    Ivar.fill_exn release ();
    let%bind () = holder >>| ok in
    let%bind r = Pool.execute pool "SELECT 1" in
    print_result [%sexp_of: int] r;
    [%expect {| (Ok 1) |}];
    return ())
;;

let%expect_test "a killed connection is replaced" =
  with_pool ~max_size:2 ~health_check_after:Time_ns.Span.zero (fun pool ->
    let%bind first = Pool.fetch_one pool "SELECT pg_backend_pid()" ~row:pid_row >>| ok in
    let%bind cluster = Pg_test.shared () in
    let%bind admin = Connection.connect (Pg_test.config cluster) >>| ok in
    let%bind (_ : bool) =
      Query.fetch_one
        admin
        "SELECT pg_terminate_backend($1)"
        ~params:[ Param.int32 (Int32.of_int_exn first) ]
        ~row:(Row.nth 0 Type.bool)
      >>| ok
    in
    let%bind () = Connection.close admin in
    (* Give the server a moment to drop the session. *)
    let%bind () = Clock_ns.after (Time_ns.Span.of_int_ms 100) in
    let%bind second = Pool.fetch_one pool "SELECT pg_backend_pid()" ~row:pid_row >>| ok in
    print_s
      [%message
        "" ~replaced:(first <> second : bool) ~stats:(Pool.stats pool : Pool.Stats.t)];
    [%expect
      {|
      ((replaced true)
       (stats ((size 1) (idle 1) (waiting 0) (opened 2) (discarded 1))))
      |}];
    return ())
;;

let%expect_test "a connection returned inside a transaction is rolled back" =
  with_pool ~max_size:1 (fun pool ->
    let%bind () =
      Pool.with_connection pool ~f:(fun conn ->
        (* Separate scripts: a BEGIN inside a multi-statement string pulls the statements
           before it into the transaction too. *)
        let%bind () = Connection.execute_script conn "CREATE TABLE t (v int)" >>| ok in
        Connection.execute_script conn "BEGIN; INSERT INTO t VALUES (1)")
      >>| ok
    in
    let%bind r =
      Pool.with_connection pool ~f:(fun conn ->
        let%map n =
          Query.fetch_one conn "SELECT count(*) FROM t" ~row:(Row.nth 0 Type.int)
        in
        Result.map n ~f:(fun n -> n, Connection.in_transaction conn))
    in
    print_result [%sexp_of: int * bool] r;
    [%expect {| (Ok (0 false)) |}];
    return ())
;;

let%expect_test "pool transaction and close" =
  with_pool (fun pool ->
    let%bind r =
      Pool.with_transaction pool ~f:(fun conn ->
        Query.fetch_one
          conn
          "SELECT 40 + $1::int8"
          ~params:[ Param.int 2 ]
          ~row:(Row.nth 0 Type.int))
    in
    print_result [%sexp_of: int] r;
    [%expect {| (Ok 42) |}];
    let%bind () = Pool.close pool in
    let%bind r = Pool.execute pool "SELECT 1" in
    print_result [%sexp_of: int] r;
    [%expect {| (Error Closed) |}];
    return ())
;;

let wait_until ?(timeout = Time_ns.Span.of_int_sec 10) f =
  let deadline = Time_ns.add (Time_ns.now ()) timeout in
  let rec loop () =
    if f ()
    then return true
    else if Time_ns.( > ) (Time_ns.now ()) deadline
    then return false
    else (
      let%bind () = Clock_ns.after (Time_ns.Span.of_int_ms 10) in
      loop ())
  in
  loop ()
;;

let%expect_test "min_size connections are opened up front and survive idling" =
  with_pool
    ~max_size:4
    ~min_size:2
    ~max_idle_time:(Time_ns.Span.of_int_ms 100)
    (fun pool ->
       let%bind warm = wait_until (fun () -> (Pool.stats pool).idle = 2) in
       print_s [%message (warm : bool) ~stats:(Pool.stats pool : Pool.Stats.t)];
       [%expect
         {|
        ((warm true)
         (stats ((size 2) (idle 2) (waiting 0) (opened 2) (discarded 0))))
        |}];
       (* Four at once, then idle well past max_idle_time: back down to two. *)
       let%bind (_ : int list) =
         Deferred.List.init ~how:`Parallel 4 ~f:(fun _ ->
           Pool.fetch_one pool "SELECT pg_backend_pid() FROM pg_sleep(0.05)" ~row:pid_row
           >>| ok)
       in
       let%bind () = Clock_ns.after (Time_ns.Span.of_int_ms 500) in
       print_s [%sexp (Pool.stats pool : Pool.Stats.t)];
       [%expect {| ((size 2) (idle 2) (waiting 0) (opened 4) (discarded 2)) |}];
       return ())
;;

let%expect_test "connections older than max_lifetime are replaced" =
  with_pool
    ~max_size:1
    ~min_size:1
    ~max_lifetime:(Time_ns.Span.of_int_ms 300)
    (fun pool ->
       let pid () = Pool.fetch_one pool "SELECT pg_backend_pid()" ~row:pid_row >>| ok in
       let%bind first = pid () in
       let%bind again = pid () in
       (* Expired while idle: the reaper closes it and min_size opens a new one. *)
       let%bind replaced =
         wait_until (fun () ->
           let s = Pool.stats pool in
           s.discarded >= 1 && s.idle = 1)
       in
       let%bind later = pid () in
       print_s
         [%message
           ""
             ~reused:(first = again : bool)
             (replaced : bool)
             ~same_after_expiry:(first = later : bool)
             ~stats:(Pool.stats pool : Pool.Stats.t)];
       [%expect
         {|
        ((reused true) (replaced true) (same_after_expiry false)
         (stats ((size 1) (idle 1) (waiting 0) (opened 2) (discarded 1))))
        |}];
       return ())
;;

let%expect_test "a connection held past max_lifetime is closed when returned" =
  with_pool ~max_size:1 ~max_lifetime:(Time_ns.Span.of_int_ms 100) (fun pool ->
    let%bind r =
      Pool.with_connection pool ~f:(fun conn ->
        let%bind () = Clock_ns.after (Time_ns.Span.of_int_ms 200) in
        Query.fetch_one conn "SELECT pg_backend_pid()" ~row:pid_row)
    in
    let first = ok r in
    print_s [%sexp (Pool.stats pool : Pool.Stats.t)];
    let%bind second = Pool.fetch_one pool "SELECT pg_backend_pid()" ~row:pid_row >>| ok in
    print_s [%message "" ~same_backend:(first = second : bool)];
    [%expect
      {|
      ((size 0) (idle 0) (waiting 0) (opened 1) (discarded 1))
      (same_backend false)
      |}];
    return ())
;;

let%expect_test "min_size is validated" =
  let%bind cluster = Pg_test.shared () in
  let config = Pg_test.config cluster in
  print_s
    [%sexp
      (Or_error.try_with (fun () -> Pool.create ~max_size:2 ~min_size:3 config)
       |> Result.map ~f:(const ())
       : unit Or_error.t)];
  [%expect
    {|
    (Error
     ("Pool.create: min_size must be between 0 and max_size" (min_size 3)
      (max_size 2)))
    |}];
  return ()
;;
