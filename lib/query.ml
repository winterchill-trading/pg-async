open! Core
open! Async

let run ?label ?timeout ?(params = []) conn sql =
  let encoded = List.map params ~f:Param.encode in
  Connection.Private.exec
    ?label
    ?timeout
    conn
    ~sql
    ~param_types:(Array.of_list_map encoded ~f:fst)
    ~params:(Array.of_list_map encoded ~f:snd)
;;

let execute ?label ?timeout ?params conn sql =
  let%map r = run ?label ?timeout ?params conn sql in
  Result.map r ~f:(fun r -> Option.value (Int.of_string_opt r#cmd_tuples) ~default:0)
;;

let fetch_all ?label ?timeout ?params conn sql ~row =
  let%map r = run ?label ?timeout ?params conn sql in
  Result.bind r ~f:(Row.decode_all row)
;;

let fold ?label ?timeout ?params conn sql ~row ~init ~f =
  let%map r = run ?label ?timeout ?params conn sql in
  Result.bind r ~f:(fun r -> Row.fold row r ~init ~f)
;;

let fetch_opt ?label ?timeout ?params conn sql ~row =
  let%map r = run ?label ?timeout ?params conn sql in
  Result.bind r ~f:(fun r ->
    match r#ntuples with
    | 0 -> Ok None
    | 1 -> Result.map (Row.decode_row row r 0) ~f:Option.some
    | got -> Error (Pg_error.Row_count { expected = "at most one row"; got }))
;;

let fetch_one ?label ?timeout ?params conn sql ~row =
  let%map r = run ?label ?timeout ?params conn sql in
  Result.bind r ~f:(fun r ->
    match r#ntuples with
    | 1 -> Row.decode_row row r 0
    | got -> Error (Pg_error.Row_count { expected = "exactly one row"; got }))
;;
