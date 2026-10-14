# AGENTS.md: pg-async

## Purpose

pg-async is a non-blocking PostgreSQL client for Async, built on libpq's asynchronous API.
It offers connections, a bounded pool, parameterized queries with typed codecs, transactions,
migrations, LISTEN/NOTIFY, COPY and Prometheus metrics, and holds no application types.

## Layout

| Path | Holds |
| --- | --- |
| `lib/` | The `pg-async` library: sessions, pool, codecs, transactions, migrations, listen, copy, metrics |
| `testing/` | `pg-async.testing` (`Pg_test`): throwaway PostgreSQL clusters for tests |
| `bin/` | The `pg-async` CLI (`check`, `migrate`, `status`, `query`) |
| `test/` | Inline expect and Quickcheck tests for every module |
| `examples/` | Programs built in CI (`quick_start.ml`, `find_user.ml`) |
| `bench/` | `pg_async_bench.exe`, query and pool timings |
| `docs/` | `api-notes.md`: what libpq and the bindings do and where they surprised us |
| `.github/` | `ci.yml` |
| `README.md`, `CHANGES.md` | User documentation and the changelog |

## Build, test, docs

Run from the repository root, inside the workspace (`..` is the workspace root):

```sh
opam exec --switch 5.2.0+ox -- dune build --root .. @pg-async/all
opam exec --switch 5.2.0+ox -- dune build --root .. @pg-async/runtest
opam exec --switch 5.2.0+ox -- dune build --root .. @pg-async/doc
```

For parallel work, give each agent its own build directory, for example
`DUNE_BUILD_DIR=../_build-pg-async-<task>`, so the shared dune lock is not contended.

Standalone (outside the workspace): add the organisation opam repository
(`opam repo add winterchill https://github.com/winterchill-trading/opam-repository.git`),
install the dependencies with `opam install --deps-only .`, then run `dune build` and
`dune test`. The tests need `initdb` and `pg_ctl` on the machine (see README "Tests").

## Conventions

- Format only this repository: `dune build --root .. @pg-async/fmt --auto-promote`. Never run
  `dune fmt` at the workspace root; it reformats every repository.
- Every module has an `.mli`. Use labelled arguments, `open! Core`, and `ppx_jane`.
- Tests are ppx_expect with `-source-tree-root .`. Promote changed expect output with
  `dune promote` after reading the diff.
- Codecs get a Quickcheck round trip, both pure and through the server (`test/test_type*.ml`).
- Every public value has odoc. CI fails on any odoc warning.
- User-visible changes get a line under `Unreleased` in `CHANGES.md`.
- Comments for humans: where a test pins a value that depends on the platform, or works
  around a bindings or libpq quirk, say why in one or two lines at that place.
- Versions are recorded only in `CHANGES.md` and git tags; the release flow is in README.

## Dependencies and dependents

pg-async depends on no sibling repository. It needs libpq (`libpq-dev` on Ubuntu,
`postgresql@18` on Homebrew) through the `postgresql` bindings, plus `core`, `async`,
`core_unix`, `uri` and `yojson`. Repositories that use it: `../neobank-api` and
`../hl-copy-api` (application state store). Changes to the public API are
breaking for them; check both before merging.

## CI

`.github/workflows/ci.yml` installs `libpq-dev` and `postgresql` with apt, runs the
`ox-setup` composite action from `winterchill-trading/infra` (OxCaml switch cache), then
builds, builds `@examples/all`, runs `dune test`, and builds `@doc` with
`ODOC_WARN_ERROR`. The `obj-magic` job calls a reusable workflow in `infra`.
To check a run, use `gh run list --limit 1`, and at most one `gh run watch <id> --exit-status`.
Do not poll in a loop.

## Pitfalls

- Never install a notice callback on a connection (`#set_notice_processor`). With the 5.4.0
  bindings the first server NOTICE can hang the runner. Use the quiet notice mode instead.
- libpq resolves host names synchronously even in non-blocking mode and ignores
  `connect_timeout`. Resolve on Async's thread pool, pass `hostaddr`, and time the handshake here.
- Watch a `dup` of libpq's socket, never libpq's own descriptor (libpq closes it on EOF).
  Release the dup with `Do_not_close_file_descriptor` and `Core_unix.close`; `Fd.close`
  calls `shutdown` on an active socket and kills the shared session.
- Async accepts one `ready_to` per direction per descriptor. For read-or-write, wait on two
  `interruptible_ready_to` calls with one shared interrupt.
- After a client-side timeout, keep collecting results. Re-running the send-and-collect
  closure resends the query.
- A binary decoder must return the same value as the text one; the tests check each column.
- Test clusters: a killed test process leaves its cluster running (`Pg_test.reap_orphans`
  cleans up). On macOS keep socket paths under 104 bytes. In expect output, remove the
  random-port `connection to server at` prefix and the `::1` line.

## Where to read more

- `README.md` (user guide), `docs/api-notes.md` (libpq notes), `CHANGES.md`.
- `../specs/repos/pg-async.md` for the repository's place in the workspace.
- `../oxcaml-claude-skills/AGENTS.md` (workspace guide) and the skills under
  `../oxcaml-claude-skills/skills/`: `janestreet-ocaml-style`, `oxcaml-setup`.
- `../oxcaml-claude-skills/GOTCHAS.md`, section "Fiat rails: pg-async".

## Never do

- Install an opam package that changes the compiler, `core`, `base`, `async` or `ppx_jane`
  on the shared `5.2.0+ox` switch. Dry-run first: `opam install --switch 5.2.0+ox --dry-run <pkg>`.
- Publish, tag or release. Releases follow the flow in README and are done by a person.
- Commit credentials. `.env` is ignored; read connection values only from the environment.
- Print a password or a `DATABASE_URL` with credentials.
