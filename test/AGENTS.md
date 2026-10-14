# AGENTS.md: test/

Inline tests for every library module: ppx_expect plus Quickcheck. Run with
`opam exec --switch 5.2.0+ox -- dune build --root .. @pg-async/runtest` from the repository root.

- Tests start a throwaway PostgreSQL cluster through `Pg_test.shared ()`: one per test
  process, with a fresh database per test. The binaries come from `PG_BINDIR`,
  `pg_config --bindir`, or the Homebrew and Debian install paths.
- After changing expect output, read the diff, then run `dune promote`.
- Expect output must not contain the random port of a test cluster or libpq's connection
  failure prefix; remove both before printing.
- `helpers.ml` and `gens.ml` hold the shared fixtures and generators; put new ones there
  rather than duplicating them.
