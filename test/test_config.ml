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
       "postgres://app:s3cr%40t@db.internal:6432/main?sslmode=verify-ca&sslrootcert=/etc/ca.pem&application_name=api&statement_timeout=1500&query_timeout=2000&statement_cache_size=0&target_session_attrs=read-write");
  [%expect
    {|
    ((host (db.internal)) (port (6432)) (user (app)) (password (***))
     (dbname (main)) (ssl_mode Verify_ca) (ssl_root_cert (/etc/ca.pem))
     (application_name api) (connect_timeout 10s) (statement_timeout (1.5s))
     (query_timeout (2s)) (statement_cache_size 0)
     (extra ((target_session_attrs read-write))))
    host='db.internal' port='6432' user='app' password='s3cr@t' dbname='main' sslmode='verify-ca' sslrootcert='/etc/ca.pem' application_name='api' connect_timeout='10' target_session_attrs='read-write'
    postgres://app:***@db.internal:6432/main?sslmode=verify-ca&application_name=api
    |}];
  show (Config.of_url "postgresql://%2Fvar%2Frun%2Fpostgresql/db");
  [%expect
    {|
    ((host (/var/run/postgresql)) (port ()) (user ()) (password ()) (dbname (db))
     (ssl_mode Verify_full) (ssl_root_cert ()) (application_name pg-async)
     (connect_timeout 10s) (statement_timeout ()) (query_timeout ())
     (statement_cache_size 256) (extra ()))
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
     (connect_timeout 10s) (statement_timeout ()) (query_timeout ())
     (statement_cache_size 256) (extra ()))
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
     (connect_timeout 10s) (statement_timeout ()) (query_timeout ())
     (statement_cache_size 256) (extra ()))
    host='h' user='u' dbname='db' sslmode='verify-full' application_name='pg-async' connect_timeout='10'
    postgres://u@h/db?sslmode=verify-full&application_name=pg-async
    |}];
  show (Config.of_env ~getenv:(env [ "PGPORT", "abc" ]) ());
  [%expect {| ("invalid number" (what PGPORT) (value abc)) |}];
  return ()
;;

let%expect_test "of_url with several hosts" =
  show (Config.of_url "postgres://app@db1:5432,db2:5433/main?sslmode=disable");
  [%expect
    {|
    ((host (db1,db2)) (port ()) (user (app)) (password ()) (dbname (main))
     (ssl_mode Disable) (ssl_root_cert ()) (application_name pg-async)
     (connect_timeout 10s) (statement_timeout ()) (query_timeout ())
     (statement_cache_size 256) (extra ((port 5432,5433))))
    host='db1,db2' user='app' dbname='main' sslmode='disable' application_name='pg-async' connect_timeout='10' port='5432,5433'
    postgres://app@db1:5432,db2:5433/main?sslmode=disable&application_name=pg-async
    |}];
  show (Config.of_url "postgres://db1,db2/main?sslmode=disable");
  [%expect
    {|
    ((host (db1,db2)) (port ()) (user ()) (password ()) (dbname (main))
     (ssl_mode Disable) (ssl_root_cert ()) (application_name pg-async)
     (connect_timeout 10s) (statement_timeout ()) (query_timeout ())
     (statement_cache_size 256) (extra ()))
    host='db1,db2' dbname='main' sslmode='disable' application_name='pg-async' connect_timeout='10'
    postgres://db1,db2/main?sslmode=disable&application_name=pg-async
    |}];
  show (Config.of_url "postgres://[::1]:6432,db2/main?sslmode=disable");
  [%expect
    {|
    ((host (::1,db2)) (port ()) (user ()) (password ()) (dbname (main))
     (ssl_mode Disable) (ssl_root_cert ()) (application_name pg-async)
     (connect_timeout 10s) (statement_timeout ()) (query_timeout ())
     (statement_cache_size 256) (extra ((port 6432,))))
    host='::1,db2' dbname='main' sslmode='disable' application_name='pg-async' connect_timeout='10' port='6432,'
    postgres://[::1]:6432,db2/main?sslmode=disable&application_name=pg-async
    |}];
  show (Config.of_url "postgres://db1:x,db2/main");
  [%expect {| ("invalid number" (what port) (value x)) |}];
  return ()
;;

let%expect_test "a multi-host URL falls through to the host that answers" =
  let%bind cluster = Pg_test.shared () in
  let config =
    Config.of_url
      (sprintf
         "postgres://postgres@127.0.0.1:1,127.0.0.1:%d/postgres?sslmode=disable"
         (Pg_test.port cluster))
    |> ok_exn
  in
  let%bind conn = Connection.connect config >>| Pg_error.ok_exn in
  let%bind r = Query.fetch_one conn "SELECT 1 AS one" ~row:(Row.column "one" Type.int) in
  print_s [%sexp (r : (int, Pg_error.t) Result.t)];
  [%expect {| (Ok 1) |}];
  Connection.close conn
;;
