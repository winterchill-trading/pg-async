(* Throwaway clusters: a cluster whose owning process died is reaped by the next start. *)

open! Core
open! Async
open Pg_async

let%expect_test "clusters of dead test processes are stopped and removed" =
  let%bind t = Pg_test.start () >>| ok_exn in
  let dir = Filename.dirname (Pg_test.data_dir t) in
  let can_connect () =
    match%bind
      Connection.connect
        { (Pg_test.config t) with connect_timeout = Time_ns.Span.of_int_sec 2 }
    with
    | Ok conn ->
      let%map () = Connection.close conn in
      true
    | Error _ -> return false
  in
  let%bind before = can_connect () in
  (* A pid that existed and is gone: a shell that printed its own and exited. *)
  let%bind dead = Process.run_exn ~prog:"/bin/sh" ~args:[ "-c"; "echo $$" ] () in
  let%bind () = Writer.save (dir ^/ "owner.pid") ~contents:(String.strip dead) in
  (* Test files run in parallel processes, and any of them starting a cluster may reap
     this one first; either way it is gone afterwards. *)
  let%bind (_ : string list) = Pg_test.reap_orphans () in
  let%bind after = can_connect () in
  let%bind exists = Sys.file_exists dir in
  print_s [%message (before : bool) (after : bool) (exists : [ `Yes | `No | `Unknown ])];
  [%expect {| ((before true) (after false) (exists No)) |}];
  (* Live owners are left alone: the shared cluster is still up. *)
  let%bind shared = Pg_test.shared () in
  let%bind reaped = Pg_test.reap_orphans () in
  print_s
    [%message
      ""
        ~shared_reaped:
          (List.exists reaped ~f:(fun d ->
             String.is_prefix (Pg_test.data_dir shared) ~prefix:d)
           : bool)];
  [%expect {| (shared_reaped false) |}];
  Pg_test.stop t
;;
