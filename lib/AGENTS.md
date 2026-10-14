# AGENTS.md: lib/

The `pg-async` library. Every module has an `.mli`; the `.mli` is the contract, so change
it first and keep it documented with odoc.

- `connection.ml`: one session on libpq's non-blocking API, driven by Async's scheduler.
  Calls on one connection are ordered by a sequencer; a LISTEN watcher must yield at once when
  a query is waiting.
- `pool.ml`, `tx.ml`, `query.ml`, `row.ml`, `param.ml`, `type.ml`: pooling, transactions with
  serialization retries, queries and typed codecs. Parameters are always sent separately,
  never spliced into SQL.
- `config.ml`: `DATABASE_URL` and `PG*` settings. TLS is `Verify_full` by default.
- `migrate.ml`, `listen.ml`, `copy.ml`, `metrics.ml`, `pg_error.ml`: the remaining features.
- `pg_async.ml`: the top-level module that re-exports the public surface.

Rules that matter here:

- Do not add a notice callback to a connection; see the root AGENTS.md pitfalls.
- Keep the library free of application types and of blocking calls. Only DNS runs in a thread.
- Add a `test/` case for every new behaviour, and a Quickcheck round trip for every codec.
