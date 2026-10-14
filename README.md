# pg-async

Non-blocking PostgreSQL access for Async on OxCaml, over libpq.

- `Connection`: one session driven by libpq's asynchronous API from the Async scheduler.
- `Pool`: bounded, health-checked, with per-query timeouts.
- `Query`, `Param`, `Row`, `Type`: parameterized statements only, typed parameters and
  applicative row decoders whose errors name the row and column.
- `Tx`: transactions with an isolation level; `with_transaction` retries serialization
  failures (40001) and deadlocks (40P01) with backoff.
- `Migrate`: ordered SQL migrations in one transaction, with checksums and an advisory
  lock.
- `Listen`: LISTEN/NOTIFY into an Async pipe.
- `Copy`: `COPY ... FROM STDIN` bulk loads from a pipe of rows.
- `Metrics`: per-label query counts, errors and latency histograms, with Prometheus
  output.
- `Pg_test` (`pg-async.testing`): throwaway clusters for tests (`initdb` + `pg_ctl` on a
  free port).
- `pg-async` CLI: `check`, `migrate`, `status`, `query`.

The library has no application types and no opinion about schema layout; it is a
client for services that keep their state in PostgreSQL and run on Async.

## Quick start

```ocaml
open! Core
open! Async
open Pg_async

type user =
  { id : int
  ; email : string
  ; created_at : Time_ns.t
  }

let user_row =
  let open Row.Let_syntax in
  let%map_open id = column "id" int
  and email = column "email" string
  and created_at = column "created_at" time_ns in
  { id; email; created_at }
;;

let migrate config =
  let%bind conn = Connection.connect config >>| Pg_error.ok_exn in
  let%bind applied =
    match%bind Migrate.load_dir "migrations" with
    | Error _ as e -> return e
    | Ok migrations -> Migrate.apply conn migrations
  in
  let%map () = Connection.close conn in
  Result.map_error applied ~f:Migrate.Error.to_error |> ok_exn
;;

let register pool email =
  Pool.with_transaction pool ~isolation:Serializable ~f:(fun conn ->
    Query.fetch_one
      conn
      "INSERT INTO users (email) VALUES ($1) RETURNING id, email, created_at"
      ~params:[ Param.string email ]
      ~row:user_row)
;;

let main () =
  let config = Config.of_env () |> ok_exn (* DATABASE_URL or PG* *) in
  let%bind (_ : int list) = migrate config in
  let pool = Pool.create ~max_size:8 config in
  let%bind () =
    match%map register pool "a@example.com" with
    | Ok user -> printf "created %d\n" user.id
    | Error e when Pg_error.is_unique_violation e -> print_endline "already registered"
    | Error e -> Error.raise (Pg_error.to_error e)
  in
  Pool.close pool
;;
```

(Compiled as `test/readme_example.ml`.) Every call returns `('a, Pg_error.t) Result.t Deferred.t`. `Pg_error.t` keeps the
server's SQLSTATE, detail, constraint and table, so callers can branch on them
(`Pg_error.sqlstate`, `is_unique_violation`, `is_retryable_transaction_failure`);
`Pg_error.to_error` / `to_or_error` convert to `Or_error`.

## Settings

`Config.of_env` reads `DATABASE_URL` when set, otherwise `PGHOST`, `PGPORT`, `PGUSER`,
`PGPASSWORD`, `PGDATABASE`, `PGSSLMODE`, `PGSSLROOTCERT`, `PGAPPNAME`,
`PGCONNECT_TIMEOUT`. `Config.of_url` understands `postgres://user:pass@host:port/db` with
the query parameters `sslmode`, `sslrootcert`, `application_name`, `connect_timeout`
(seconds), `statement_timeout` and `query_timeout` (milliseconds); others pass through to
libpq. The password never appears in sexps or `to_string_hum`.

- **TLS is required by default**: `ssl_mode` defaults to `Verify_full` (chain and host
  name). Set `sslrootcert` for a private CA. Local development opts out explicitly with
  `sslmode=disable` (or `~ssl_mode:Disable`); nothing falls back silently.
- `statement_timeout` is set on the session after connecting (server side).
- `query_timeout` is the client-side deadline per call (queueing included); every query
  function also takes `?timeout`. On expiry the statement is cancelled; if the server
  does not answer within 5 s the connection is dropped.
- `application_name` defaults to `pg-async`.
- Sessions run with `TimeZone=UTC`, `DateStyle=ISO, YMD`, `bytea_output=hex`.

## Types

| `Type` | PostgreSQL | OCaml |
|---|---|---|
| `int` | int8 (reads int2/int4) | `int` |
| `int32`, `int64` | int4, int8 | `int32`, `int64` |
| `string` | text | `string` |
| `bool` | bool | `bool` |
| `float` | float8 (NaN, ±Infinity) | `float` |
| `time_ns` | timestamptz (microseconds) | `Time_ns.t` |
| `date` | date | `Date.t` |
| `bytea` | bytea (hex) | `string` |
| `json` / `yojson` | jsonb | `string` / `Yojson.Safe.t` |
| `uuid`, `numeric` | uuid, numeric | `string` (exact text) |
| `nullable t` | NULL allowed | `'a option` |
| `array t` | one-dimensional `t[]` | `'a list` |
| `custom`, `map` | anything else | yours |

Every codec has a Quickcheck round trip, both pure (encode, decode) and through the
server (`SELECT $1`), in `test/test_type.ml`.

## Why libpq and not pgx

`pgx` (the pure-OCaml protocol implementation) pins `ocaml < 5.0` and `core < v0.16`
and does not build on the OxCaml switch. The `postgresql` bindings (5.4.0) install
cleanly and give the whole of libpq: TLS through the system OpenSSL, SCRAM, `.pgpass`,
service files, multi-host failover, and a non-blocking API. The cost is a C dependency
(`libpq-dev`, or Homebrew `postgresql@18`) and the text protocol in this first version.

## How it stays non-blocking

libpq runs in non-blocking mode and never waits on the network:

1. Connect: the host name is resolved on Async's thread pool (`Unix.Host.getbyname`),
   because `PQconnectStart` would otherwise call a blocking `getaddrinfo`; the
   addresses are passed as `hostaddr` (TLS still checks the name). `PQconnectStart` and
   `PQconnectPoll` then drive TCP, TLS and authentication, waiting on
   `Fd.interruptible_ready_to` between polls. `connect_timeout` bounds the handshake
   (libpq ignores it in this mode).
2. Query: `PQsendQueryParams`, then `PQflush` until the output is written, waiting for
   the socket to become writable (or readable, consuming input, so neither side can
   stall the other).
3. Results: `PQconsumeInput`, `PQisBusy`, `PQgetResult` whenever the socket is readable,
   until `PQgetResult` returns NULL. Notifications are drained after every read.
4. Async watches a `dup` of libpq's socket, never libpq's own descriptor: libpq closes
   its socket by itself when the server hangs up, and on Linux epoll then fails with
   EBADF and stops the scheduler. The duplicate is released with a plain `close`
   (Async's `Fd.close` would `shutdown` the shared socket).
5. Calls on one connection are serialized by a sequencer; use `Pool` for parallelism.
6. Timeouts cancel with `pg_cancel_backend` from a short-lived second session: the
   bindings take libpq's cancel handle at `PQconnectStart`, before the backend key
   exists, so `PQcancel` cannot be used.
7. Server notices are dropped inside libpq: an OCaml notice callback would run inside
   the bindings' blocking sections and hang.

`In_thread` is used only for DNS. Nothing else in the library blocks the scheduler.

## Tests

Tests start a throwaway cluster (`Pg_test.shared ()`: one per test process, a fresh
database per test) with `initdb` and `pg_ctl` on a free port, `fsync=off`. Binaries come
from `PG_BINDIR`, `pg_config --bindir`, Homebrew `postgresql@18`/`@17`, or
`/usr/lib/postgresql/<version>/bin`. `initdb` refuses to run as root.

macOS:

```sh
brew install postgresql@18
# Only needed to build the bindings (opam install postgresql):
export PKG_CONFIG_PATH=/opt/homebrew/lib/postgresql@18/pkgconfig
opam install --assume-depexts postgresql
dune build @runtest
```

Ubuntu: `apt-get install libpq-dev postgresql` (CI does this).

Coverage: queries, typed params, server and decode errors, client and server timeouts,
notices, ordering on one connection, connect failures, DNS; codec round trips (pure and
through the server); transactions (rollback, aborted COMMIT, exceptions, serialization
retries between two sessions, retry limit); pool (limit, acquire timeout, replacement of
a killed backend, rollback on release, close); migrations (apply, re-apply, checksum,
missing, out-of-order, duplicate, failure atomicity, two concurrent migrators,
`load_dir`); LISTEN/NOTIFY (idle delivery, commit ordering, unlisten, dropped
connection); COPY (5000 awkward rows round trip, bad data, wrong row width, missing
table); TLS modes against a cluster with a test CA (`verify-full` with right and wrong
CA and host name, `verify-ca`, `require`, `disable`); config parsing; metrics.

A test run killed midway leaves its cluster running under `/tmp/pg_async_*`; stop it
with `pg_ctl -D /tmp/pg_async_.../data stop`.

## Bench

`dune exec ./bench/pg_async_bench.exe` (Apple M3 Max, dev profile, local cluster with
`fsync=off`, 20 000 statements per case):

| Case | ops/s | us/op |
|---|---:|---:|
| blocking libpq `exec`, sequential (baseline) | 31 089 | 32.2 |
| `Connection` select, sequential | 30 011 | 33.3 |
| `Connection` select, 64 callers (queued) | 29 944 | 33.4 |
| `Pool(1)` select, 64 callers | 29 243 | 34.2 |
| `Pool(4)` select, 64 callers | 73 377 | 13.6 |
| `Pool(8)` select, 64 callers | 82 575 | 12.1 |
| `Pool(16)` select, 64 callers | 77 201 | 13.0 |
| `Pool(8)` insert, 64 callers | 79 002 | 12.7 |
| `Pool(8)` select by primary key, 64 callers | 77 183 | 13.0 |
| COPY FROM STDIN (200 000 rows) | 1 162 250 rows/s | 0.9 |

The Async path costs about 1 us per statement over blocking libpq; parallelism comes
from the pool.

## CLI

```sh
pg-async check -url 'postgres://app@db:5432/app'          # version, user, TLS
pg-async migrate -dir migrations                            # DATABASE_URL or PG*
pg-async status -dir migrations
pg-async query 'SELECT id, email FROM users WHERE id = $1' -p 42
```

`-sslmode` overrides the TLS mode. `query` sends parameters as untyped text for the
server to infer.

## Known gaps

- Text format only; no binary results and no prepared-statement cache (each statement
  is parsed by the server; fine at the rates above).
- A query with no parameters goes through the simple protocol (the bindings switch to
  `PQsendQuery`), so such a string may contain several statements. Use parameters for
  any value.
- Arrays are one-dimensional with default bounds. No codecs yet for interval, time,
  inet, or ranges (use `Type.custom`).
- `Time_ns` covers roughly 1823 to 2116; `infinity` timestamps are rejected.
- `Listen` does not reconnect; the pipe closes and the caller creates a new listener.
- Cancelling uses `pg_cancel_backend`, which needs the same role (or
  `pg_signal_backend`).
- `Pool` has no minimum size or maximum connection lifetime.
- `Config.of_url` takes one host; use `extra` or `PGHOST` for multi-host lists.

## Used by

`neobank-api` and `hl-copy-api` in the same organisation.
