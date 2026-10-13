(lang dune 3.17)

(name pg-async)

(generate_opam_files true)

(license MIT)

(maintainers "winterchill-trading")

(authors "winterchill-trading")

(source
 (github winterchill-trading/pg-async))

(package
 (name pg-async)
 (synopsis "Non-blocking PostgreSQL client for Async, over libpq")
 (description
  "Connections, a bounded pool, typed parameters and row decoders, transactions with serialization retries, migrations, LISTEN/NOTIFY, COPY and metrics. Drives libpq's asynchronous API from the Async scheduler.")
 (depends
  (ocaml
   (>= 5.2))
  dune
  core
  core_unix
  async
  postgresql
  ppx_jane
  yojson
  uri))
