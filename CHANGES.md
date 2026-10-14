# Changelog

All notable changes to this project are documented here. The format follows
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/) and the project uses
[Semantic Versioning](https://semver.org/).

## Unreleased

### Added

- Per-connection prepared-statement cache (`Config.statement_cache_size`, default 256,
  least recently used evicted and deallocated). Statements that the server dropped or
  whose result shape changed after DDL are prepared again and retried once outside a
  transaction. `Connection.prepared_statements` reports the cache size.
- Binary results for cached statements when every decoded column supports it, with
  binary decoders for the built-in codecs (`Type.accepts_binary`,
  `Type.decode_binary`, `Row.accepts_binary`).
- Codecs for `interval` (`Type.Interval`), `time` (`Type.ofday`), `inet` and `cidr`
  (`Type.Inet`), ranges over int4, int8, numeric, timestamptz and date (`Type.Range`,
  `Type.range`), and `infinity` timestamps and dates (`Type.with_infinity`), with
  matching `Param` and `Row` shortcuts.
- `Pool.create ?min_size ?max_lifetime`: connections opened up front and kept through
  idle periods; connections past their lifetime replaced.
- `Listen.create ?reconnect ?backoff ?on_reconnect`: a listener that opens a new session
  after a dropped one and listens to the same channels again; `Listen.channels` and
  `Listen.reconnects`.
- `Config.of_url` accepts several hosts (`postgres://h1:5432,h2:5433/db`).
- `Connection.connected_at`.
- `Pg_test.reap_orphans`, run by `Pg_test.start`: clusters left by killed test
  processes are stopped and removed.

### Changed

- `Config.t` has a new field, `statement_cache_size`; code that builds the record
  literally must set it (`Config.create` and `{ c with ... }` are unaffected).
- Queries with parameters now run as prepared statements, so a server behind a pooler
  without prepared-statement support needs `statement_cache_size = 0`.

## 0.1.0

First release.

### Added

- `Connection`: one session driven through libpq's asynchronous API from the Async
  scheduler, with host name resolution on the thread pool, a client-side connect
  deadline and per-query deadlines that cancel the statement on the server.
- `Pool`: a bounded pool with an acquire timeout, health checks for connections idle
  past a threshold, an idle timeout and rollback of connections returned inside a
  transaction.
- `Query`, `Param`, `Row` and `Type`: parameterized statements, typed text-format codecs
  (integers, text, bool, float8, timestamptz, date, bytea, jsonb, uuid, numeric,
  nullable values and one-dimensional arrays) and applicative row decoders whose errors
  name the row and column.
- `Tx`: transactions with an isolation level and retries of serialization failures and
  deadlocks.
- `Migrate`: ordered SQL migrations with checksums, run under an advisory lock.
- `Listen`: LISTEN/NOTIFY delivered to an Async pipe.
- `Copy`: `COPY ... FROM STDIN` bulk loads from a pipe of rows.
- `Metrics`: per-label counts, errors and latency histograms with Prometheus output.
- `Config`: settings from a URL or the `PG*` environment, TLS `verify-full` by default,
  passwords redacted in sexps.
- `pg-async.testing`: throwaway clusters for tests (`initdb` and `pg_ctl` on a free
  port).
- `pg-async` command line tool: `check`, `migrate`, `status` and `query`.
