# AGENTS.md: docs/

`api-notes.md`: what the libpq bindings and the libpq client library do, and where they
surprised us (connect, socket ownership, notices, cancel, server versions).

- This file is a reference for maintainers, not the user guide; the user guide is `../README.md`.
- When a bindings or libpq behaviour changes a rule in the code, update the note in the same
  commit, and keep the code comment that points to it accurate.
- Do not record workspace paths or machine names here.
