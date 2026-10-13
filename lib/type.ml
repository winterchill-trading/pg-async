open! Core

type 'a t =
  { name : string
  ; oid : int
  ; array_oid : int option
  ; encode : 'a -> string option
  ; decode : string option -> ('a, string) Result.t
  }

let name t = t.name
let oid t = t.oid
let encode t v = t.encode v
let decode t s = t.decode s

let base ~name ~oid ~array_oid ~to_string ~of_string =
  { name
  ; oid
  ; array_oid
  ; encode = (fun v -> Some (to_string v))
  ; decode =
      (function
        | None -> Error "unexpected NULL (wrap the codec in Type.nullable)"
        | Some s -> of_string s)
  }
;;

let custom ~name ~oid ?array_oid ~encode ~decode () =
  base ~name ~oid ~array_oid ~to_string:encode ~of_string:decode
;;

let parse_with ~what f s =
  match f s with
  | v -> Ok v
  | exception _ -> Error (sprintf "cannot read %S as %s" s what)
;;

let int =
  base
    ~name:"int8"
    ~oid:20
    ~array_oid:(Some 1016)
    ~to_string:Int.to_string
    ~of_string:(fun s ->
      match Int.of_string_opt s with
      | Some n -> Ok n
      | None -> Error (sprintf "cannot read %S as a 63-bit int" s))
;;

let int32 =
  base
    ~name:"int4"
    ~oid:23
    ~array_oid:(Some 1007)
    ~to_string:Int32.to_string
    ~of_string:(parse_with ~what:"int4" Int32.of_string)
;;

let int64 =
  base
    ~name:"int8"
    ~oid:20
    ~array_oid:(Some 1016)
    ~to_string:Int64.to_string
    ~of_string:(parse_with ~what:"int8" Int64.of_string)
;;

let string =
  base
    ~name:"text"
    ~oid:25
    ~array_oid:(Some 1009)
    ~to_string:Fn.id
    ~of_string:Result.return
;;

let bool =
  base
    ~name:"bool"
    ~oid:16
    ~array_oid:(Some 1000)
    ~to_string:(fun b -> if b then "t" else "f")
    ~of_string:(function
      | "t" | "true" -> Ok true
      | "f" | "false" -> Ok false
      | s -> Error (sprintf "cannot read %S as bool" s))
;;

let float =
  base
    ~name:"float8"
    ~oid:701
    ~array_oid:(Some 1022)
    ~to_string:(fun f ->
      match Float.classify f with
      | Nan -> "NaN"
      | Infinite -> if Float.is_positive f then "Infinity" else "-Infinity"
      | Normal | Subnormal | Zero -> sprintf "%.17g" f)
    ~of_string:(function
      | "NaN" -> Ok Float.nan
      | "Infinity" -> Ok Float.infinity
      | "-Infinity" -> Ok Float.neg_infinity
      | s ->
        (match Float.of_string_opt s with
         | Some f -> Ok f
         | None -> Error (sprintf "cannot read %S as float8" s)))
;;

module Timestamptz = struct
  let utc = Time_float.Zone.utc
  let to_string t = Time_ns.to_string_iso8601_basic t ~zone:utc

  let digits s ~what =
    if (not (String.is_empty s)) && String.for_all s ~f:Char.is_digit
    then Ok (Int.of_string s)
    else Error (sprintf "bad %s %S" what s)
  ;;

  (* [+HH], [+HH:MM] or [+HH:MM:SS], as seconds east of UTC. *)
  let offset_seconds s =
    let open Result.Let_syntax in
    let sign, rest =
      match s.[0] with
      | '+' -> 1, String.drop_prefix s 1
      | _ -> -1, String.drop_prefix s 1
    in
    let%map parts =
      List.map (String.split rest ~on:':') ~f:(digits ~what:"UTC offset") |> Result.all
    in
    match parts with
    | [ h ] -> sign * (h * 3600)
    | [ h; m ] -> sign * ((h * 3600) + (m * 60))
    | h :: m :: s :: _ -> sign * ((h * 3600) + (m * 60) + s)
    | [] -> 0
  ;;

  let of_string s =
    let open Result.Let_syntax in
    let fail () = Error (sprintf "cannot read %S as timestamptz" s) in
    (* The server writes "2026-10-13 12:00:00.5+00"; [to_string] writes the ISO 8601 form
       with [T] and [Z]. Both are read. *)
    let split =
      match String.lsplit2 s ~on:' ' with
      | Some _ as r -> r
      | None -> String.lsplit2 s ~on:'T'
    in
    match split with
    | None -> fail ()
    | Some (date_s, rest) ->
      let rest =
        match String.chop_suffix rest ~suffix:"Z" with
        | Some r -> r ^ "+00"
        | None -> rest
      in
      (match String.lfindi rest ~f:(fun _ c -> Char.equal c '+' || Char.equal c '-') with
       | None -> fail ()
       | Some k ->
         let time_s = String.prefix rest k in
         let offset_s = String.drop_prefix rest k in
         if String.mem offset_s ' '
         then fail () (* "... BC" *)
         else (
           match String.split time_s ~on:':' with
           | [ h; m; sec ] ->
             let sec, frac =
               match String.lsplit2 sec ~on:'.' with
               | None -> sec, ""
               | Some (sec, frac) -> sec, frac
             in
             let%bind hr = digits h ~what:"hour" in
             let%bind min = digits m ~what:"minute" in
             let%bind sec = digits sec ~what:"second" in
             let%bind ns =
               if String.is_empty frac
               then Ok 0
               else if String.length frac > 9
               then fail ()
               else
                 digits (frac ^ String.make (9 - String.length frac) '0') ~what:"fraction"
             in
             let%bind offset = offset_seconds offset_s in
             (match
                let date = Date.of_string date_s in
                let ofday = Time_ns.Ofday.create ~hr ~min ~sec ~ns () in
                Time_ns.sub
                  (Time_ns.of_date_ofday ~zone:utc date ofday)
                  (Time_ns.Span.of_int_sec offset)
              with
              | t -> Ok t
              | exception _ -> fail ())
           | _ -> fail ()))
  ;;
end

let time_ns =
  base
    ~name:"timestamptz"
    ~oid:1184
    ~array_oid:(Some 1185)
    ~to_string:Timestamptz.to_string
    ~of_string:Timestamptz.of_string
;;

let date =
  base
    ~name:"date"
    ~oid:1082
    ~array_oid:(Some 1182)
    ~to_string:Date.to_string
    ~of_string:(fun s ->
      (* Date.of_string also takes other layouts; insist on the ISO one the session is set
         to. *)
      if String.length s = 10
      then parse_with ~what:"date" Date.of_string s
      else Error (sprintf "cannot read %S as date" s))
;;

let hex_digit c =
  match c with
  | '0' .. '9' -> Some (Char.to_int c - Char.to_int '0')
  | 'a' .. 'f' -> Some (Char.to_int c - Char.to_int 'a' + 10)
  | 'A' .. 'F' -> Some (Char.to_int c - Char.to_int 'A' + 10)
  | _ -> None
;;

let bytea =
  base
    ~name:"bytea"
    ~oid:17
    ~array_oid:(Some 1001)
    ~to_string:(fun s ->
      let b = Buffer.create ((2 * String.length s) + 2) in
      Buffer.add_string b "\\x";
      String.iter s ~f:(fun c -> Buffer.add_string b (sprintf "%02x" (Char.to_int c)));
      Buffer.contents b)
    ~of_string:(fun s ->
      match String.chop_prefix s ~prefix:"\\x" with
      | None -> Error "bytea is not in hex format (bytea_output must be hex)"
      | Some hex when String.length hex % 2 <> 0 -> Error "odd-length bytea hex"
      | Some hex ->
        let n = String.length hex / 2 in
        let out = Bytes.create n in
        let rec go i =
          if i = n
          then Ok (Bytes.to_string out)
          else (
            match hex_digit hex.[2 * i], hex_digit hex.[(2 * i) + 1] with
            | Some hi, Some lo ->
              Bytes.set out i (Char.of_int_exn ((hi * 16) + lo));
              go (i + 1)
            | _ -> Error "bad hex digit in bytea")
        in
        go 0)
;;

let json =
  base
    ~name:"jsonb"
    ~oid:3802
    ~array_oid:(Some 3807)
    ~to_string:Fn.id
    ~of_string:Result.return
;;

let yojson =
  base
    ~name:"jsonb"
    ~oid:3802
    ~array_oid:(Some 3807)
    ~to_string:(fun j -> Yojson.Safe.to_string j)
    ~of_string:(fun s ->
      match Yojson.Safe.from_string s with
      | j -> Ok j
      | exception Yojson.Json_error msg -> Error (sprintf "invalid JSON: %s" msg))
;;

let uuid =
  base
    ~name:"uuid"
    ~oid:2950
    ~array_oid:(Some 2951)
    ~to_string:Fn.id
    ~of_string:Result.return
;;

let numeric =
  base
    ~name:"numeric"
    ~oid:1700
    ~array_oid:(Some 1231)
    ~to_string:Fn.id
    ~of_string:Result.return
;;

let nullable t =
  { name = t.name
  ; oid = t.oid
  ; array_oid = t.array_oid
  ; encode = (fun v -> Option.bind v ~f:t.encode)
  ; decode =
      (function
        | None -> Ok None
        | Some _ as s -> Result.map (t.decode s) ~f:Option.some)
  }
;;

let map t ~name ~f ~f_inv =
  { name
  ; oid = t.oid
  ; array_oid = t.array_oid
  ; encode = (fun v -> t.encode (f_inv v))
  ; decode = (fun s -> Result.bind (t.decode s) ~f)
  }
;;

module Array_literal = struct
  let to_string elements =
    let b = Buffer.create 64 in
    Buffer.add_char b '{';
    List.iteri elements ~f:(fun i e ->
      if i > 0 then Buffer.add_char b ',';
      match e with
      | None -> Buffer.add_string b "NULL"
      | Some s ->
        Buffer.add_char b '"';
        String.iter s ~f:(fun c ->
          if Char.equal c '"' || Char.equal c '\\' then Buffer.add_char b '\\';
          Buffer.add_char b c);
        Buffer.add_char b '"');
    Buffer.add_char b '}';
    Buffer.contents b
  ;;

  let of_string s =
    let n = String.length s in
    let error msg = Error (sprintf "array literal %S: %s" s msg) in
    let rec skip_ws i =
      if i < n && Char.is_whitespace s.[i] then skip_ws (i + 1) else i
    in
    if n < 2 || (not (Char.equal s.[0] '{')) || not (Char.equal s.[n - 1] '}')
    then error "expected {...} (only one-dimensional arrays with default bounds)"
    else (
      let last = n - 1 in
      let buf = Buffer.create 32 in
      (* Returns the element and the index just after it. *)
      let quoted i =
        Buffer.clear buf;
        let rec go i =
          if i >= last
          then error "unterminated quoted element"
          else (
            match s.[i] with
            | '"' -> Ok (Some (Buffer.contents buf), i + 1)
            | '\\' when i + 1 < last ->
              Buffer.add_char buf s.[i + 1];
              go (i + 2)
            | c ->
              Buffer.add_char buf c;
              go (i + 1))
        in
        go i
      in
      let unquoted i =
        Buffer.clear buf;
        let rec go i =
          if i >= last || Char.equal s.[i] ','
          then (
            let v = String.strip (Buffer.contents buf) in
            if String.is_empty v
            then error "empty element"
            else if String.Caseless.equal v "NULL"
            then Ok (None, i)
            else Ok (Some v, i))
          else (
            match s.[i] with
            | '{' -> error "nested arrays are not supported"
            | '\\' when i + 1 < last ->
              Buffer.add_char buf s.[i + 1];
              go (i + 2)
            | c ->
              Buffer.add_char buf c;
              go (i + 1))
        in
        go i
      in
      let rec elements i acc =
        let i = skip_ws i in
        let parsed =
          if i < last && Char.equal s.[i] '"' then quoted (i + 1) else unquoted i
        in
        match parsed with
        | Error _ as e -> e
        | Ok (v, i) ->
          let i = skip_ws i in
          if i >= last
          then Ok (List.rev (v :: acc))
          else if Char.equal s.[i] ','
          then elements (i + 1) (v :: acc)
          else error "expected , between elements"
      in
      if skip_ws 1 >= last then Ok [] else elements 1 [])
  ;;
end

let array t =
  match t.array_oid with
  | None ->
    raise_s [%message "Type.array: no array type for element codec" (t.name : string)]
  | Some oid ->
    { name = t.name ^ "[]"
    ; oid
    ; array_oid = None
    ; encode = (fun l -> Some (Array_literal.to_string (List.map l ~f:t.encode)))
    ; decode =
        (function
          | None -> Error "unexpected NULL (wrap the codec in Type.nullable)"
          | Some s ->
            Result.bind (Array_literal.of_string s) ~f:(fun elements ->
              List.mapi elements ~f:(fun i e ->
                Result.map_error (t.decode e) ~f:(fun msg ->
                  sprintf "element %d: %s" i msg))
              |> Result.all))
    }
;;
