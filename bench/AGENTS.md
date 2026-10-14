# AGENTS.md: bench/

`pg_async_bench.exe`: timings for query, prepared-statement and pool paths against a throwaway
cluster from `pg-async.testing`.

- Run it with `opam exec --switch 5.2.0+ox -- dune exec --root .. ./pg-async/bench/pg_async_bench.exe`
  from the workspace; the numbers depend on the machine, so compare runs on one machine only.
- Benchmarks are not part of `dune test`. Do not add a pinned number from a benchmark to a test
  without a comment that names the machine class.
