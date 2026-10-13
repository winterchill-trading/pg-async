open! Core
open! Async
open Pg_async

let show r =
  match r with
  | Error e -> print_s [%sexp (e : Error.t)]
  | Ok (c : Config.t) ->
    print_s [%sexp (c : Config.t)];
    print_endline (Config.to_conninfo c);
    print_endline (Config.to_string_hum c)
;;

let%expect_test "of_url" =
  show
    (Config.of_url
       "postgres://app:s3cr%40t@db.internal:6432/main?sslmode=verify-ca&sslrootcert=/etc/ca.pem&application_name=api&statement_timeout=1500&query_timeout=2000&target_session_attrs=read-write");
  [%expect
    {|
    ((host (db.internal)) (port (6432)) (user (app)) (password (***))
     (dbname (main)) (ssl_mode Verify_ca) (ssl_root_cert (/etc/ca.pem))
     (application_name api) (connect_timeout 10s) (statement_timeout (1.5s))
     (query_timeout (2s)) (extra ((target_session_attrs read-write))))
    host='db.internal' port='6432' user='app' password='s3cr@t' dbname='main' sslmode='verify-ca' sslrootcert='/etc/ca.pem' application_name='api' connect_timeout='10' target_session_attrs='read-write'
    postgres://app:***@db.internal:6432/main?sslmode=verify-ca&application_name=api
    |}];
  show (Config.of_url "postgresql://%2Fvar%2Frun%2Fpostgresql/db");
  [%expect
    {|
    ((host (/var/run/postgresql)) (port ()) (user ()) (password ()) (dbname (db))
     (ssl_mode Verify_full) (ssl_root_cert ()) (application_name pg-async)
     (connect_timeout 10s) (statement_timeout ()) (query_timeout ()) (extra ()))
    host='/var/run/postgresql' dbname='db' sslmode='verify-full' application_name='pg-async' connect_timeout='10'
    postgres:///var/run/postgresql/db?sslmode=verify-full&application_name=pg-async
    |}];
  show (Config.of_url "mysql://x/y");
  [%expect {| "expected a postgres:// or postgresql:// URL" |}];
  show (Config.of_url "postgres://h/d?sslmode=sometimes");
  [%expect {| ("unknown sslmode" sometimes) |}];
  return ()
;;

let%expect_test "of_env" =
  let env l name = List.Assoc.find l name ~equal:String.equal in
  show
    (Config.of_env
       ~getenv:
         (env
            [ "PGHOST", "localhost"
            ; "PGPORT", "5433"
            ; "PGUSER", "me"
            ; "PGPASSWORD", "it's"
            ; "PGDATABASE", "d"
            ; "PGSSLMODE", "disable"
            ; "PGAPPNAME", "worker"
            ])
       ());
  [%expect
    {|
    ((host (localhost)) (port (5433)) (user (me)) (password (***)) (dbname (d))
     (ssl_mode Disable) (ssl_root_cert ()) (application_name worker)
     (connect_timeout 10s) (statement_timeout ()) (query_timeout ()) (extra ()))
    host='localhost' port='5433' user='me' password='it\'s' dbname='d' sslmode='disable' application_name='worker' connect_timeout='10'
    postgres://me:***@localhost:5433/d?sslmode=disable&application_name=worker
    |}];
  show
    (Config.of_env
       ~getenv:(env [ "DATABASE_URL", "postgres://u@h/db"; "PGHOST", "ignored" ])
       ());
  [%expect
    {|
    ((host (h)) (port ()) (user (u)) (password ()) (dbname (db))
     (ssl_mode Verify_full) (ssl_root_cert ()) (application_name pg-async)
     (connect_timeout 10s) (statement_timeout ()) (query_timeout ()) (extra ()))
    host='h' user='u' dbname='db' sslmode='verify-full' application_name='pg-async' connect_timeout='10'
    postgres://u@h/db?sslmode=verify-full&application_name=pg-async
    |}];
  show (Config.of_env ~getenv:(env [ "PGPORT", "abc" ]) ());
  [%expect {| ("invalid number" (what PGPORT) (value abc)) |}];
  return ()
;;
