# Changelog

All notable changes to this project are documented here. The format follows
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/) and the project uses
[Semantic Versioning](https://semver.org/).

## Unreleased

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
