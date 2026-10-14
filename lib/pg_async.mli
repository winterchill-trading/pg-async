(** Non-blocking PostgreSQL access for Async, over libpq.

    Every operation returns [('a, Pg_error.t) Result.t Deferred.t] and never blocks the
    Async scheduler: the connection handshake, TLS, queries and COPY all go through
    libpq's asynchronous API.

    {[
      open! Core
      open! Async
      open Pg_async

      let user_row =
        let open Row.Let_syntax in
        let%map_open id = column "id" int
        and email = column "email" string in
        id, email
      ;;

      let find_user pool ~id =
        Pool.fetch_opt
          pool
          "SELECT id, email FROM users WHERE id = $1"
          ~params:[ Param.int id ]
          ~row:user_row
      ;;

      let main () =
        let config = Config.of_env () |> ok_exn in
        let pool = Pool.create ~max_size:8 config in
        let%bind user = find_user pool ~id:42 in
        print_s [%sexp (user : ((int * string) option, Pg_error.t) Result.t)];
        Pool.close pool
      ;;
    ]}

    This is [examples/find_user.ml]; a complete program with migrations and a transaction
    is [examples/quick_start.ml].

    {1 Modules}

    - {!Config}: connection settings from a URL, the environment or code.
    - {!Connection}: one session.
    - {!Pool}: a bounded pool of sessions.
    - {!Query}, {!Param}, {!Row}, {!Type}: statements, parameters, row decoders and value
      codecs.
    - {!Tx}: transactions with retries.
    - {!Migrate}: ordered SQL migrations.
    - {!Listen}: LISTEN/NOTIFY.
    - {!Copy}: bulk loads with [COPY FROM STDIN].
    - {!Metrics}: per-label counters and latency histograms.
    - {!Pg_error}: the error type. *)

module Config = Config
module Connection = Connection
module Copy = Copy
module Listen = Listen
module Metrics = Metrics
module Migrate = Migrate
module Param = Param
module Pg_error = Pg_error
module Pool = Pool
module Query = Query
module Row = Row
module Tx = Tx
module Type = Type
