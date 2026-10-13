# libpq and the `postgresql` bindings: notes

What this library relies on, and what surprised us, with the PostgreSQL 18 client and
server (macOS Homebrew) and the Ubuntu packages in CI.

## Connecting

- `PQconnectStart` + `PQconnectPoll` drive the handshake without blocking, except for
  host name resolution: `PQconnectStart` calls `getaddrinfo` synchronously. We resolve
  on Async's thread pool and pass `hostaddr`, keeping `host` (repeated once per address)
  so TLS verifies the name and libpq tries each address in order.
- The socket can change between polls (each address, and the plaintext retry under
  `sslmode=prefer`/`allow`), so every wait during the handshake uses a fresh Async `Fd`
  that is released (`Do_not_close_file_descriptor`) before the next poll.
- `connect_timeout` is ignored by libpq in non-blocking mode; the caller must enforce
  it. Below 2 s libpq rounds it up anyway.
- The bindings raise `Postgresql.Error (Connection_failure _)` from the constructor if
  `PQconnectStart` itself fails (bad conninfo); later failures surface as
  `Polling_failed` with the text in `#error_message`. A multi-address failure lists one
  line per attempt, `connection to server at "<addr>", port <p> failed: <reason>`.
- `sslmode` values: `verify-full` checks the chain and host name (IP SANs work for IP
  hosts); `verify-ca` the chain only; `require` encrypts without checking, except that
  libpq treats `require` with a `sslrootcert` like `verify-ca`. Against a server without
  TLS, `verify-full` fails with "server does not support SSL, but SSL was required".
- Libpq still reads `PG*` variables for anything the conninfo leaves out.

## Running statements

- Non-blocking sequence: `PQsendQueryParams`, `PQflush` until 0 (waiting for write, and
  consuming input when readable), then `PQconsumeInput`/`PQisBusy`/`PQgetResult` until
  `PQgetResult` returns NULL. Results must be drained to NULL before the next send or
  libpq reports "another command is already in progress".
- The bindings call `PQsendQuery` (simple protocol) instead of `PQsendQueryParams` when
  there are no parameters and text results, so a parameterless string may hold several
  statements, and each statement yields its own result.
- Text-format parameters: NULL is the bindings' `Postgresql.null` sentinel; parameter
  type OID 0 lets the server infer the type.
- `#cmd_tuples` is the affected row count as a string ("" for statements without one).
- `#server_version` splits `PQserverVersion` as two-digit major.minor.revision, the
  pre-10 scheme, so 18.3 (180003) comes back as `(18, 0, 3)`. Rejoin the integer.
- `#error_field` returns "" for absent fields. `SEVERITY_NONLOCALIZED` is stable across
  locales.

## Cancelling

- The bindings capture the `PGcancel` handle (`PQgetCancel`) when the connection object
  is created. With `~startonly:true` that is before the backend key exists, so
  `#request_cancel` cannot cancel anything. We cancel with
  `SELECT pg_cancel_backend(pid)` from a short-lived second session (same role, or
  `pg_signal_backend`), then keep reading: the statement ends with SQLSTATE 57014.
- A server `statement_timeout` also ends with 57014 ("canceling statement due to
  statement timeout").

## Notices

- An OCaml notice processor (`#set_notice_processor`) is called from libpq while it
  parses input, and the bindings run `PQisBusy` inside `caml_enter_blocking_section`.
  The first NOTICE (e.g. `CREATE TABLE IF NOT EXISTS` on an existing table) then hangs
  the process. Use `#set_notice_processing `Quiet`, which stays in C.

## Transactions

- A `BEGIN` inside a multi-statement simple query turns the implicit transaction into a
  real one, including the statements before it.
- `COMMIT` of a transaction in which a statement failed succeeds at the protocol level
  with the command tag `ROLLBACK`. Check the tag or lose writes silently; we report it
  as SQLSTATE 25P02.
- Serialization failures (40001) can come from any statement or from `COMMIT`; under
  `SERIALIZABLE` or `REPEATABLE READ` a concurrent update of the same row gives
  40001 to the second writer once the first commits. Deadlocks are 40P01.
- `RAISE EXCEPTION ... USING ERRCODE = 'serialization_failure'` in a `DO` block is a
  handy way to test retry paths.

## Values (text format)

- Sessions are set to `TimeZone=UTC`, `DateStyle=ISO, YMD`, `bytea_output=hex` so
  output parsing does not depend on server defaults.
- timestamptz keeps microseconds; input with more digits is rounded. Output looks like
  `2026-10-13 12:00:00.5+00`; offsets can be `+05:30` and, for old dates in some zones,
  `-00:19:32`. ISO 8601 input with `T` and `Z` is accepted.
- float8 output is the shortest exact form (`extra_float_digits` defaults to 1 since
  PostgreSQL 12); `NaN`, `Infinity`, `-Infinity` are spelled out; `-0` is kept.
- jsonb normalises: keys sorted (by length, then bytes), duplicate keys keep the last,
  numbers become numeric (`1e20` comes back as `100000000000000000000`), `\u0000` is
  rejected. Compare canonical forms in tests.
- text rejects NUL bytes and invalid UTF-8 in a UTF8 database.
- Arrays: `{a,"b c",NULL}`; elements are quoted when they contain delimiters, quotes,
  backslashes or spaces, or are empty or the word NULL. Unquoted elements may carry
  whitespace around them. Non-default bounds print as `[0:1]={...}`.

## LISTEN/NOTIFY

- Channel names are identifiers and cannot be bind parameters; quote them
  (`"..."` with `"` doubled). `pg_notify($1, $2)` takes the channel as text and needs no
  quoting.
- Notifications sent inside a transaction are delivered at commit. libpq hands them out
  through `PQnotifies` after any `PQconsumeInput`, so an idle listener must keep reading
  the socket.
- When the server terminates the session, the socket reads EOF and
  `PQconsumeInput` fails.

## COPY

- `COPY t (cols) FROM STDIN` (text format): fields separated by tab, rows ended by a
  newline, `\N` for NULL, and backslash, tab, newline and carriage return escaped with a
  backslash. Values use the same text forms as parameters.
- In non-blocking mode `PQputCopyData` returns 0 when its buffer is full: flush (waiting
  for write) and retry. End with `PQputCopyEnd`, flush, then read the final result;
  `cmd_tuples` is the row count.
- `PQputCopyEnd` with an error message aborts the COPY: the server discards every row
  and answers with an error.

## Test clusters

- `initdb -U postgres -A trust --no-sync`, `pg_ctl -w start` with `-c port=...
  -c listen_addresses=127.0.0.1 -c unix_socket_directories=<tmp> -c fsync=off`. About a
  second each on an M-series Mac.
- macOS caps Unix socket paths at 104 bytes; keep the socket directory under `/tmp`.
- `initdb` refuses to run as root (containers).
- Debian/Ubuntu install server binaries in `/usr/lib/postgresql/<version>/bin`, not on
  `PATH`.
- Building the bindings on macOS needs
  `PKG_CONFIG_PATH=/opt/homebrew/lib/postgresql@18/pkgconfig` and
  `opam install --assume-depexts postgresql`.
