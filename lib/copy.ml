open! Core
open! Async

let quote_identifier s =
  "\"" ^ String.substr_replace_all s ~pattern:"\"" ~with_:"\"\"" ^ "\""
;;

let add_field b = function
  | None -> Buffer.add_string b "\\N"
  | Some s ->
    String.iter s ~f:(function
      | '\\' -> Buffer.add_string b "\\\\"
      | '\t' -> Buffer.add_string b "\\t"
      | '\n' -> Buffer.add_string b "\\n"
      | '\r' -> Buffer.add_string b "\\r"
      | c -> Buffer.add_char b c)
;;

let add_row b row =
  List.iteri row ~f:(fun i p ->
    if i > 0 then Buffer.add_char b '\t';
    add_field b (snd (Param.encode p)));
  Buffer.add_char b '\n'
;;

let encode_row row =
  let b = Buffer.create 64 in
  add_row b row;
  Buffer.contents b
;;

let chunk_size = 65_536

let from_pipe ?(label = "copy") ?timeout ?schema conn ~table ~columns rows =
  let target =
    match schema with
    | None -> quote_identifier table
    | Some s -> quote_identifier s ^ "." ^ quote_identifier table
  in
  (* Identifiers only, each quoted; the data travels as COPY data, never as SQL. *)
  let sql =
    sprintf
      "COPY %s (%s) FROM STDIN"
      target
      (String.concat ~sep:", " (List.map columns ~f:quote_identifier))
  in
  let width = List.length columns in
  let bad_row = ref None in
  let data =
    Pipe.create_reader ~close_on_exception:true (fun writer ->
      let b = Buffer.create chunk_size in
      let flush () =
        if Buffer.length b = 0
        then return ()
        else (
          let chunk = Buffer.contents b in
          Buffer.clear b;
          Pipe.write writer chunk)
      in
      let rec loop () =
        match%bind Pipe.read' rows with
        | `Eof -> flush ()
        | `Ok batch ->
          let bad = Queue.find batch ~f:(fun row -> List.length row <> width) in
          (match bad with
           | Some row ->
             bad_row := Some (List.length row);
             return ()
           | None ->
             let%bind () =
               Queue.fold batch ~init:(return ()) ~f:(fun acc row ->
                 let%bind () = acc in
                 add_row b row;
                 if Buffer.length b >= chunk_size then flush () else return ())
             in
             loop ())
      in
      loop ())
  in
  let%map r = Connection.Private.copy_in ~label ?timeout conn ~sql ~data in
  match !bad_row with
  | Some got ->
    (* The partial load ended without its terminator, so the server never committed it;
       report the caller's mistake rather than whatever the server said. *)
    Error
      (Pg_error.Protocol
         (sprintf "COPY row has %d values for %d columns; nothing was loaded" got width))
  | None -> r
;;

let from_list ?label ?timeout ?schema conn ~table ~columns rows =
  from_pipe ?label ?timeout ?schema conn ~table ~columns (Pipe.of_list rows)
;;
