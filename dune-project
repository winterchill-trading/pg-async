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
  "pg-async drives libpq's asynchronous API from the Async scheduler, so queries, the connection handshake and TLS never block other jobs. It provides connections, a bounded pool with a minimum size and a maximum connection lifetime, typed parameters and applicative row decoders, a per-connection prepared-statement cache with binary results, transactions with serialization retries, ordered migrations, LISTEN/NOTIFY with reconnection, COPY FROM STDIN bulk loads and Prometheus metrics. The pg-async.testing library starts throwaway PostgreSQL clusters for tests. Needs libpq. Builds with upstream OCaml 5.2 and Jane Street v0.17 as well as with OxCaml and the v0.18 previews.")
 (tags
  (postgresql database async libpq))
 (depends
  (ocaml
   (>= 5.2.0))
  dune
  (core
   (and
    (>= v0.17.2)
    (< v0.19~)))
  (core_unix
   (and
    (>= v0.17.1)
    (< v0.19~)))
  (async
   (and
    (>= v0.17.0)
    (< v0.19~)))
  (ppx_jane
   (and
    (>= v0.17.0)
    (< v0.19~)))
  (postgresql
   (and
    (>= 5.4.0)
    (< 6.0)))
  (uri
   (>= 4.4.0))
  (yojson
   (and
    (>= 2.2.0)
    (< 4.0)))
  (odoc :with-doc)))
