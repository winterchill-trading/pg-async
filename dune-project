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

(documentation "https://github.com/winterchill-trading/pg-async#readme")

(package
 (name pg-async)
 (synopsis "Non-blocking PostgreSQL client for Async, over libpq")
 (description
  "pg-async drives libpq's asynchronous API from the Async scheduler, so queries, the connection handshake and TLS never block other jobs. It provides connections, a bounded pool with a minimum size and a maximum connection lifetime, typed parameters and applicative row decoders, a per-connection prepared-statement cache with binary results, transactions with serialization retries, ordered migrations, LISTEN/NOTIFY with reconnection, COPY FROM STDIN bulk loads and Prometheus metrics. The pg-async.testing library starts throwaway PostgreSQL clusters for tests. Needs libpq; built against the OxCaml compiler and Jane Street's v0.18 preview packages.")
 (tags
  (postgresql database async libpq))
 (depends
  oxcaml
  (ocaml
   (and
    (>= 5.2.0)
    (< 5.3~)))
  dune
  (core
   (and
    (>= v0.18~preview.130.106+341)
    (< v0.19~)))
  (core_unix
   (and
    (>= v0.18~preview.130.106+341)
    (< v0.19~)))
  (async
   (and
    (>= v0.18~preview.130.106+341)
    (< v0.19~)))
  (ppx_jane
   (and
    (>= v0.18~preview.130.106+341)
    (< v0.19~)))
  (postgresql
   (and
    (>= 5.4.0)
    (< 6.0)))
  (uri
   (>= 4.4.0))
  (yojson
   (>= 2.2.0))
  (odoc :with-doc)))
