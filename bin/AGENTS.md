# AGENTS.md: bin/

The `pg-async` command-line tool (`pg_async_cli.ml`) with the subcommands `check`,
`migrate`, `status` and `query`.

- Built with `dune build --root .. @pg-async/all` from the repository root.
- The connection comes from a URL argument or from the same environment variables as the
  library (`DATABASE_URL`, or `PG*`). Never print the password.
- `migrate` applies the migrations in one transaction under an advisory lock; keep its
  output and exit codes stable, since scripts depend on them.
- The CLI has no tests of its own beyond the library's; check a change by running it against
  a throwaway cluster from `pg-async.testing`.
