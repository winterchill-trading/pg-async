(* Quickcheck round trips for codecs: pure (encode then decode) and through the server
   ([SELECT $1]). *)

open! Core
open! Async
open Pg_async

let pure_round_trip ~name ty gen ~equal ~sexp_of =
  match
    Quickcheck.test_or_error
      gen
      ~sexp_of
      ~trials:2_000
      ~seed:(`Deterministic name)
      ~f:(fun v ->
        match Type.decode ty (Type.encode ty v) with
        | Ok v' when equal v v' -> Ok ()
        | r ->
          Or_error.error_s
            [%message
              "round trip failed"
                name
                ~encoded:(Type.encode ty v : string option)
                ~decoded:(Result.map r ~f:sexp_of : (Sexp.t, string) Result.t)])
  with
  | Ok () -> printf "%s ok\n" name
  | Error e -> print_s [%sexp (e : Error.t)]
;;

let server_round_trip conn ~name ?(trials = 150) ?(cast = "") ty gen ~equal ~sexp_of =
  let values =
    Quickcheck.random_sequence ~seed:(`Deterministic name) gen
    |> Fn.flip Sequence.take trials
    |> Sequence.to_list
  in
  let%map failure =
    Deferred.List.find_map values ~f:(fun v ->
      match%map
        Query.fetch_one
          conn
          ("SELECT $1" ^ cast ^ " AS v")
          ~params:[ Param.create ty v ]
          ~row:(Row.column "v" ty)
      with
      | Ok v' when equal v v' -> None
      | r ->
        Some
          [%message
            "server round trip failed"
              name
              ~sent:(sexp_of v : Sexp.t)
              ~got:(Result.map r ~f:sexp_of : (Sexp.t, Pg_error.t) Result.t)])
  in
  match failure with
  | None -> printf "%s ok\n" name
  | Some sexp -> print_s sexp
;;
