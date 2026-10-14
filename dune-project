(lang dune 3.17)

(name pg-async)

(generate_opam_files true)

(license MIT)

(maintainers "hello@winterchill.xyz")

(authors "WinterChill AI")

(source
 (github winterchill-trading/pg-async))

(homepage "https://github.com/winterchill-trading/pg-async")

(bug_reports "https://github.com/winterchill-trading/pg-async/issues")

(documentation "https://ocaml.org/p/pg-async/latest")

(package
 (name pg-async)
 (synopsis "Non-blocking PostgreSQL client for Async, over libpq")
 (description
  "pg-async drives libpq's asynchronous API from the Async scheduler, so queries, the connection handshake and TLS never block other jobs. It provides connections, a bounded, health-checked pool, typed parameters and applicative row decoders, transactions with serialization retries, ordered migrations, LISTEN/NOTIFY, COPY FROM STDIN bulk loads and Prometheus metrics. The pg-async.testing library starts throwaway PostgreSQL clusters for tests.")
 (tags
  (postgresql database async libpq))
 (depends
  (ocaml
   (>= 5.2))
  dune
  (core
   (>= v0.18~preview.130.106))
  (core_unix
   (>= v0.18~preview.130.106))
  (async
   (>= v0.18~preview.130.106))
  (ppx_jane
   (>= v0.18~preview.130.106))
  (postgresql
   (>= 5.4.0))
  (uri
   (>= 4.4.0))
  (yojson
   (>= 2.2.0))))
