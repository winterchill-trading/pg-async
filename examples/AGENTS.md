# AGENTS.md: examples/

Two programs that CI builds with `dune build @examples/all`: `quick_start.ml` (the example in
the README, kept identical to it) and `find_user.ml`.

- If you change an API used here, update the example and the README quick start together.
- Examples are not run in CI, only built, so they must compile against the current `lib/`.
