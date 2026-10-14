# AGENTS.md: testing/

The `pg-async.testing` library (`Pg_test`). It starts throwaway PostgreSQL clusters
(`initdb` plus `pg_ctl` on a free port, `fsync=off`) for tests and benchmarks.

- Its public interface is `pg_test.mli`; keep it small and documented.
- Clusters are stopped at exit, but a killed process skips `at_exit`. `Pg_test.start` reaps
  orphans first. Do not remove that step.
- Temp directory names use the `pg-async-` prefix, which `Filename_unix.temp_dir` turns into
  `pg_async_`. Keep socket paths short (macOS limits them to 104 bytes), so use `/tmp`.
- `initdb` refuses to run as root; tests must not assume a root user.
