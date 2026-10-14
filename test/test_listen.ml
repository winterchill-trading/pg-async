open! Core
open! Async
open Pg_async
open Helpers

let next reader =
  match%map Clock_ns.with_timeout (Time_ns.Span.of_int_sec 5) (Pipe.read reader) with
  | `Timeout -> print_endline "timed out"
  | `Result `Eof -> print_endline "eof"
  | `Result (`Ok (n : Connection.Notification.t)) ->
    print_s [%message n.channel n.payload]
;;

let%expect_test "notifications arrive while idle, at commit, until unlisten" =
  let%bind cluster = Pg_test.shared () in
  let%bind config = Pg_test.create_database cluster in
  let%bind listener = Listen.create config >>| ok in
  let%bind sender = Connection.connect config >>| ok in
  let reader = Listen.notifications listener in
  let%bind () = Listen.listen listener "jobs \"ready\"" >>| ok in
  let%bind () = Listen.notify sender ~channel:"jobs \"ready\"" ~payload:"1" >>| ok in
  let%bind () = next reader in
  [%expect {| ("jobs \"ready\"" 1) |}];
  let%bind () =
    Tx.with_transaction sender ~f:(fun conn ->
      let%bind () =
        Listen.notify conn ~channel:"jobs \"ready\"" ~payload:"in tx" >>| ok
      in
      (* Not delivered before COMMIT. *)
      let%map r =
        Clock_ns.with_timeout (Time_ns.Span.of_int_ms 100) (Pipe.values_available reader)
      in
      print_s
        [%sexp
          ((match r with
            | `Timeout -> "nothing yet"
            | `Result _ -> "early")
           : string)];
      Ok ())
    >>| ok
  in
  [%expect {| "nothing yet" |}];
  let%bind () = next reader in
  [%expect {| ("jobs \"ready\"" "in tx") |}];
  let%bind () = Listen.unlisten listener "jobs \"ready\"" >>| ok in
  let%bind () = Listen.notify sender ~channel:"jobs \"ready\"" ~payload:"2" >>| ok in
  let%bind () = Listen.listen listener "other" >>| ok in
  let%bind () = Listen.notify sender ~channel:"other" ~payload:"3" >>| ok in
  let%bind () = next reader in
  [%expect {| (other 3) |}];
  let%bind () = Connection.close sender in
  let%bind () = Listen.close listener in
  let%bind () = next reader in
  [%expect {| eof |}];
  return ()
;;

let%expect_test "the pipe closes when the server drops the connection" =
  let%bind cluster = Pg_test.shared () in
  let%bind config = Pg_test.create_database cluster in
  let%bind listener = Listen.create config >>| ok in
  let%bind () = Listen.listen listener "c" >>| ok in
  let%bind admin = Connection.connect config >>| ok in
  let%bind (_ : int) =
    Query.execute
      admin
      "SELECT pg_terminate_backend(pid) FROM pg_stat_activity WHERE datname = \
       current_database() AND pid <> pg_backend_pid()"
    >>| ok
  in
  let%bind () = next (Listen.notifications listener) in
  [%expect {| eof |}];
  Connection.close admin
;;

let terminate_others config =
  let%bind admin = Connection.connect config >>| ok in
  let%bind (_ : int) =
    Query.execute
      admin
      "SELECT pg_terminate_backend(pid) FROM pg_stat_activity WHERE datname = \
       current_database() AND pid <> pg_backend_pid()"
    >>| ok
  in
  Connection.close admin
;;

let%expect_test "a reconnecting listener survives a dropped connection" =
  let%bind cluster = Pg_test.shared () in
  let%bind config = Pg_test.create_database cluster in
  let reconnected = Mvar.create () in
  let%bind listener =
    Listen.create
      ~reconnect:true
      ~backoff:(fun ~attempt:_ -> Time_ns.Span.of_int_ms 20)
      ~on_reconnect:(fun () -> Mvar.set reconnected ())
      config
    >>| ok
  in
  let reader = Listen.notifications listener in
  let%bind () = Listen.listen listener "a" >>| ok in
  let%bind () = Listen.listen listener "b" >>| ok in
  let%bind () = terminate_others config in
  let%bind () = Mvar.take reconnected in
  print_s
    [%message (Listen.reconnects listener : int) (Listen.channels listener : string list)];
  [%expect {| (("Listen.reconnects listener" 1) ("Listen.channels listener" (a b))) |}];
  let%bind sender = Connection.connect config >>| ok in
  let%bind () = Listen.notify sender ~channel:"b" ~payload:"after" >>| ok in
  let%bind () = next reader in
  [%expect {| (b after) |}];
  let%bind () = Connection.close sender in
  (* Once more, with a channel added around the time the session drops. *)
  let%bind () = terminate_others config in
  let%bind () = Listen.listen listener "c" >>| ok in
  let%bind () = Mvar.take reconnected in
  let%bind sender = Connection.connect config >>| ok in
  let%bind () = Listen.notify sender ~channel:"c" ~payload:"new" >>| ok in
  let%bind () = next reader in
  print_s [%message (Listen.reconnects listener : int)];
  [%expect {|
    (c new)
    ("Listen.reconnects listener" 2)
    |}];
  let%bind () = Connection.close sender in
  let%bind () = Listen.close listener in
  let%bind () = next reader in
  [%expect {| eof |}];
  return ()
;;

let%expect_test "closing a listener stops it reconnecting" =
  let%bind cluster = Pg_test.shared () in
  let%bind config = Pg_test.create_database cluster in
  let%bind listener =
    Listen.create
      ~reconnect:true
      ~backoff:(fun ~attempt:_ -> Time_ns.Span.of_int_ms 50)
      config
    >>| ok
  in
  let%bind () = Listen.listen listener "a" >>| ok in
  let%bind () = terminate_others config in
  let%bind () = Listen.close listener in
  let%bind () = Clock_ns.after (Time_ns.Span.of_int_ms 200) in
  let%bind r = Listen.listen listener "b" in
  print_s [%message (Listen.reconnects listener : int) (r : (unit, Pg_error.t) Result.t)];
  let%bind () = next (Listen.notifications listener) in
  [%expect {|
    (("Listen.reconnects listener" 0) (r (Error Closed)))
    eof
    |}];
  return ()
;;
