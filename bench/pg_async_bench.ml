(* Throughput of simple statements through a Connection, a Pool and COPY, against a
   throwaway local cluster (fsync off). Run with: dune exec ./bench/pg_async_bench.exe --
   [-n N] *)

open! Core
open! Async
open Pg_async

let ok = Pg_error.ok_exn

let report name ~ops ~elapsed =
  let secs = Time_ns.Span.to_sec elapsed in
  printf
    "%-44s %9d ops %8.0f ops/s %8.1f us/op\n%!"
    name
    ops
    (Float.of_int ops /. secs)
    (secs *. 1e6 /. Float.of_int ops)
;;

let timed f =
  let start = Time_ns.now () in
  let%map () = f () in
  Time_ns.diff (Time_ns.now ()) start
;;

(* Run [n] jobs with at most [concurrency] in flight. *)
let run_concurrently ~n ~concurrency f =
  let next = ref 0 in
  let rec worker () =
    if !next >= n
    then return ()
    else (
      let i = !next in
      incr next;
      let%bind () = f i in
      worker ())
  in
  Deferred.all_unit (List.init concurrency ~f:(fun _ -> worker ()))
;;

let select_one conn i =
  Query.fetch_one
    conn
    "SELECT $1::int8 + 1"
    ~params:[ Param.int i ]
    ~row:(Row.nth 0 Type.int)
  >>| ok
  >>| fun (_ : int) -> ()
;;

let main ~n ~copy_rows =
  Pg_test.with_temp_cluster (fun cluster ->
    let%bind config = Pg_test.create_database cluster in
    let%bind conn = Connection.connect config >>| ok in
    let%bind () =
      Connection.execute_script
        conn
        "CREATE TABLE item (id int8 PRIMARY KEY, name text, at timestamptz)"
      >>| ok
    in
    (* Baseline: the same statement through blocking libpq calls. *)
    let blocking = new Postgresql.connection ~conninfo:(Config.to_conninfo config) () in
    let%bind elapsed =
      timed (fun () ->
        for i = 1 to n do
          ignore
            (blocking#exec
               ~expect:[ Tuples_ok ]
               ~param_types:[| 20 |]
               ~params:[| Int.to_string i |]
               "SELECT $1::int8 + 1"
             : Postgresql.result)
        done;
        return ())
    in
    blocking#finish;
    report "blocking libpq exec, sequential (baseline)" ~ops:n ~elapsed;
    let%bind elapsed =
      timed (fun () ->
        Deferred.List.iter ~how:`Sequential (List.init n ~f:Fn.id) ~f:(select_one conn))
    in
    report "Connection select, sequential" ~ops:n ~elapsed;
    let uncached = { config with statement_cache_size = 0 } in
    let%bind conn_uncached = Connection.connect uncached >>| ok in
    let%bind elapsed =
      timed (fun () ->
        Deferred.List.iter
          ~how:`Sequential
          (List.init n ~f:Fn.id)
          ~f:(select_one conn_uncached))
    in
    report "Connection select, sequential, no stmt cache" ~ops:n ~elapsed;
    let%bind elapsed =
      timed (fun () -> run_concurrently ~n ~concurrency:64 (select_one conn))
    in
    report "Connection select, 64 callers (queued)" ~ops:n ~elapsed;
    let%bind () =
      Deferred.List.iter ~how:`Sequential [ 1; 4; 8; 16 ] ~f:(fun size ->
        let pool = Pool.create ~max_size:size config in
        let%bind elapsed =
          timed (fun () ->
            run_concurrently ~n ~concurrency:64 (fun i ->
              Pool.with_connection pool ~f:(fun conn ->
                select_one conn i >>| Result.return)
              >>| ok))
        in
        report (sprintf "Pool(%d) select, 64 callers" size) ~ops:n ~elapsed;
        Pool.close pool)
    in
    let pool = Pool.create ~max_size:8 config in
    let%bind elapsed =
      timed (fun () ->
        run_concurrently ~n ~concurrency:64 (fun i ->
          Pool.execute
            pool
            "INSERT INTO item (id, name, at) VALUES ($1, $2, $3)"
            ~params:
              [ Param.int i
              ; Param.string (sprintf "item %d" i)
              ; Param.time_ns (Time_ns.now ())
              ]
          >>| ok
          >>| fun (_ : int) -> ()))
    in
    report "Pool(8) insert, 64 callers" ~ops:n ~elapsed;
    let%bind elapsed =
      timed (fun () ->
        run_concurrently ~n ~concurrency:64 (fun i ->
          Pool.fetch_one
            pool
            "SELECT name, at FROM item WHERE id = $1"
            ~params:[ Param.int i ]
            ~row:Row.(both (nth 0 Type.string) (nth 1 Type.time_ns))
          >>| ok
          >>| fun (_ : string * Time_ns.t) -> ()))
    in
    report "Pool(8) select by primary key, 64 callers" ~ops:n ~elapsed;
    let%bind () = Pool.close pool in
    (* Decoding cost: 1000 rows of (int8, text, timestamptz) per statement; binary once
       the statement is cached, text without the cache. *)
    let wide_row =
      Row.Let_syntax.(
        let%map_open id = nth 0 int
        and name = nth 1 string
        and at = nth 2 time_ns in
        id, name, at)
    in
    let fetch_wide conn =
      Query.fetch_all
        conn
        "SELECT id, name, at FROM item WHERE id < $1 ORDER BY id"
        ~params:[ Param.int 1000 ]
        ~row:wide_row
      >>| ok
      >>| fun (_ : (int * string * Time_ns.t) list) -> ()
    in
    let wide_n = Int.max 1 (n / 50) in
    let%bind elapsed =
      timed (fun () ->
        Deferred.List.iter ~how:`Sequential (List.init wide_n ~f:Fn.id) ~f:(fun _ ->
          fetch_wide conn))
    in
    report "1000-row select, binary results (rows)" ~ops:(wide_n * 1000) ~elapsed;
    let%bind elapsed =
      timed (fun () ->
        Deferred.List.iter ~how:`Sequential (List.init wide_n ~f:Fn.id) ~f:(fun _ ->
          fetch_wide conn_uncached))
    in
    report "1000-row select, text results (rows)" ~ops:(wide_n * 1000) ~elapsed;
    let%bind () = Connection.close conn_uncached in
    let%bind elapsed =
      timed (fun () ->
        let rows =
          Pipe.of_sequence
            (Sequence.init copy_rows ~f:(fun i ->
               [ Param.int (n + i)
               ; Param.string (sprintf "copied %d" i)
               ; Param.time_ns (Time_ns.now ())
               ]))
        in
        let%map loaded =
          Copy.from_pipe conn ~table:"item" ~columns:[ "id"; "name"; "at" ] rows >>| ok
        in
        assert (loaded = copy_rows))
    in
    report "COPY FROM STDIN" ~ops:copy_rows ~elapsed;
    Connection.close conn)
;;

let () =
  Command_unix.run
    (Command.async
       ~summary:"pg-async throughput on a throwaway local cluster"
       (let%map_open.Command n =
          flag "-n" (optional_with_default 20_000 int) ~doc:"N statements per case"
        and copy_rows =
          flag "-copy-rows" (optional_with_default 200_000 int) ~doc:"N rows for COPY"
        in
        fun () -> main ~n ~copy_rows))
;;
