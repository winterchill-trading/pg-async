# pg-async

Non-blocking PostgreSQL access for Async, over libpq.

- `Connection`: one session driven by libpq's asynchronous API from the Async scheduler.
- `Pool`: bounded, health-checked, with an optional minimum size and maximum connection
  lifetime.
- `Query`, `Param`, `Row`, `Type`: parameterized statements only, typed parameters and
  applicative row decoders whose errors name the row and column. Statements are
  prepared once per connection and read back in the binary format when every decoded
  column supports it.
- `Tx`: transactions with an isolation level; `with_transaction` retries serialization
  failures (40001) and deadlocks (40P01) with backoff.
- `Migrate`: ordered SQL migrations in one transaction, with checksums and an advisory
  lock.
- `Listen`: LISTEN/NOTIFY into an Async pipe, optionally reconnecting.
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
    | Ok user ->
      printf
        "created %d for %s at %s\n"
        user.id
        user.email
        (Time_ns.to_string_utc user.created_at)
    | Error e when Pg_error.is_unique_violation e -> print_endline "already registered"
    | Error e -> Error.raise (Pg_error.to_error e)
  in
  Pool.close pool
;;
```

(Built in CI as `examples/quick_start.ml`.) Every call returns `('a, Pg_error.t)
Result.t Deferred.t`. `Pg_error.t` keeps the server's SQLSTATE, detail, constraint and
table, so callers can branch on them (`Pg_error.sqlstate`, `is_unique_violation`,
`is_retryable_transaction_failure`); `Pg_error.to_error` / `to_or_error` convert to
`Or_error`.

## Settings

`Config.of_env` reads `DATABASE_URL` when set, otherwise `PGHOST`, `PGPORT`, `PGUSER`,
`PGPASSWORD`, `PGDATABASE`, `PGSSLMODE`, `PGSSLROOTCERT`, `PGAPPNAME`,
`PGCONNECT_TIMEOUT`. `Config.of_url` understands `postgres://user:pass@host:port/db`
with the query parameters `sslmode`, `sslrootcert`, `application_name`,
`connect_timeout` (seconds), `statement_timeout` and `query_timeout` (milliseconds) and
`statement_cache_size`; others pass through to libpq. Several hosts
(`postgres://db1:5432,db2:5433/app`) become libpq's host and port lists, tried in order.
The password never appears in sexps or `to_string_hum`.

- **TLS is required by default**: `ssl_mode` defaults to `Verify_full` (chain and host
  name). Set `sslrootcert` for a private CA. Local development opts out explicitly with
  `sslmode=disable` (or `~ssl_mode:Disable`); nothing falls back silently.
- `statement_timeout` is set on the session after connecting (server side).
- `query_timeout` is the client-side deadline per call (queueing included); every query
  function also takes `?timeout`. On expiry the statement is cancelled; if the server
  does not answer within 5 s the connection is dropped.
- `application_name` defaults to `pg-async`.
- `statement_cache_size` (default 256) bounds the prepared statements kept per
  connection; `0` turns preparing, and so binary results, off (needed behind PgBouncer
  before 1.21 in transaction mode).
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
| `interval` | interval | `Type.Interval.t` (months, days, microseconds) |
| `ofday` | time | `Time_ns.Ofday.t` |
| `inet`, `cidr` | inet, cidr | `Type.Inet.t` (address, prefix length) |
| `range t` | int4range, int8range, numrange, tstzrange, daterange | `'a Type.Range.t` |
| `with_infinity t` | timestamptz or date with `infinity` | `'a Type.Infinite.t` |
| `nullable t` | NULL allowed | `'a option` |
| `array t` | one-dimensional `t[]` | `'a list` |
| `custom`, `map` | anything else | yours |

Parameters are sent as text. Results of a cached statement come back in binary when
every column the row decoder reads accepts it: all codecs above except `numeric` and
`custom` read binary, and the binary decoder must give the same value as the text one
(tested column by column). Every codec has a Quickcheck round trip, both pure (encode,
decode) and through the server (`SELECT $1`, text on the first run and binary after),
in `test/test_type.ml` and `test/test_type_extended.ml`.

## Why libpq and not pgx

`pgx` (the pure-OCaml protocol implementation) pins `ocaml < 5.0` and `core < v0.16`
and does not build on the OxCaml switch. The `postgresql` bindings (5.4.0) install
cleanly and give the whole of libpq: TLS through the system OpenSSL, SCRAM, `.pgpass`,
service files, multi-host failover, and a non-blocking API. The cost is a C dependency
(`libpq-dev`, or Homebrew `postgresql@18`).

## How it stays non-blocking

libpq runs in non-blocking mode and never waits on the network:

1. Connect: the host name is resolved on Async's thread pool (`Unix.Host.getbyname`),
   because `PQconnectStart` would otherwise call a blocking `getaddrinfo`; the
   addresses are passed as `hostaddr` (TLS still checks the name). `PQconnectStart` and
   `PQconnectPoll` then drive TCP, TLS and authentication, waiting on
   `Fd.interruptible_ready_to` between polls. `connect_timeout` bounds the handshake
   (libpq ignores it in this mode).
2. Query: `PQsendPrepare` the first time a statement runs on a connection, then
   `PQsendQueryPrepared` (or `PQsendQuery` for a statement without parameters), then
   `PQflush` until the output is written, waiting for the socket to become writable (or
   readable, consuming input, so neither side can stall the other).
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
connection, reconnection with re-LISTEN, close while reconnecting); prepared statements
(binary from the second run, text-only codecs, eviction with DEALLOCATE, re-prepare
after DDL, DEALLOCATE and DISCARD ALL); pool minimum size and lifetime; COPY (5000
awkward rows round trip, bad data, wrong row width, missing table); TLS modes against a
cluster with a test CA (`verify-full` with right and wrong CA and host name,
`verify-ca`, `require`, `disable`); config parsing including multi-host URLs and
failover; metrics.

A test run killed midway (SIGKILL skips `at_exit`) leaves its cluster running under
`/tmp/pg_async_*`. Each cluster directory records its owner's pid, and the next
`Pg_test.start` stops and removes clusters whose owner is gone
(`Pg_test.reap_orphans`).

## Bench

`dune exec ./bench/pg_async_bench.exe` (Apple M3 Max, dev profile, local cluster with
`fsync=off`, 20 000 statements per case):

| Case | ops/s | us/op |
|---|---:|---:|
| blocking libpq `exec`, sequential (baseline, unprepared) | 31 065 | 32.2 |
| `Connection` select, sequential | 36 246 | 27.6 |
| `Connection` select, sequential, `statement_cache_size = 0` | 29 720 | 33.6 |
| `Connection` select, 64 callers (queued) | 36 406 | 27.5 |
| `Pool(1)` select, 64 callers | 34 268 | 29.2 |
| `Pool(4)` select, 64 callers | 76 751 | 13.0 |
| `Pool(8)` select, 64 callers | 80 078 | 12.5 |
| `Pool(16)` select, 64 callers | 77 442 | 12.9 |
| `Pool(8)` insert, 64 callers | 78 629 | 12.7 |
| `Pool(8)` select by primary key, 64 callers | 79 955 | 12.5 |
| 1000-row select (int8, text, timestamptz), binary | 2 230 649 rows/s | 0.4 |
| 1000-row select (int8, text, timestamptz), text | 1 752 357 rows/s | 0.6 |
| COPY FROM STDIN (200 000 rows) | 940 986 rows/s | 1.1 |

Prepared statements make a sequential select about 6 us cheaper than parsing it every
time (and cheaper than blocking unprepared libpq); binary results decode rows about 25%
faster. Parallelism comes from the pool.

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

These need a design decision or depend on something outside the library:

- A query with no parameters goes through the simple protocol (the bindings switch to
  `PQsendQuery`), so such a string may contain several statements, is not prepared and
  returns text. Changing that would break callers that run multi-statement strings
  through `Query`; use parameters for any value.
- Arrays are one-dimensional with default bounds: a multi-dimensional array needs an
  OCaml representation (nested lists cannot express PostgreSQL's rectangular arrays
  with bounds) that has not been chosen.
- Parameters are always sent in text; only results use the binary format.
- `Time_ns` covers roughly 1823 to 2116, so timestamps outside it are decode errors
  (`with_infinity` covers `infinity`).
- Cancelling uses `pg_cancel_backend` from a second session, which needs the same role
  (or `pg_signal_backend`): the bindings take libpq's cancel handle before the backend
  key exists, so `PQcancel` would need a change in the bindings.

## Portability

The code uses no OxCaml-only syntax or modes (no `[@zero_alloc]`, `or_null`, unboxed
types, `local_` or `let mutable`), only Jane Street's Core and Async APIs. It is built
and tested on the OxCaml switch (`5.2.0+ox`, Jane Street `v0.18~preview`, which exist
only in the OxCaml opam repository); a build on plain OCaml 5.2 with the v0.17 packages
from opam-repository has not been tried yet.

## Releasing

Versions follow [Semantic Versioning](https://semver.org/) and are recorded only in
`CHANGES.md` and git tags (dune-release substitutes `%%VERSION%%` from the tag). The
flow, once the package is ready for opam:

1. Move the `Unreleased` entries in `CHANGES.md` under a new version heading.
2. `dune-release tag vX.Y.Z` to tag the commit.
3. `dune-release distrib` to build the archive (runs the tests), then
   `dune-release publish` for the GitHub release.
4. `dune-release opam pkg` and `dune-release opam submit` to open the opam-repository
   pull request.

## Used by

`neobank-api` and `hl-copy-api` in the same organisation.
