open! Core

type 'a t =
  { name : string
  ; oid : int
  ; array_oid : int option
  ; range_oid : (string * int * int) option
      (* The range type over this one: its name, OID and array OID. *)
  ; encode : 'a -> string option
  ; decode : string option -> ('a, string) Result.t
  ; binary : int -> (string option -> ('a, string) Result.t) option
  (* The binary-format decoder for a column of this OID, if there is one. Each one must
     give the same value as [decode] reading the text form of that column. *)
  }

let null_error = Error "unexpected NULL (wrap the codec in Type.nullable)"

(* Big-endian readers over binary-format values, with bounds checks. *)
module Wire = struct
  exception Short

  let byte s pos = if pos >= String.length s then raise Short else Char.to_int s.[pos]

  let uint32 s pos =
    (byte s pos lsl 24)
    lor (byte s (pos + 1) lsl 16)
    lor (byte s (pos + 2) lsl 8)
    lor byte s (pos + 3)
  ;;

  let int16 s pos =
    let v = (byte s pos lsl 8) lor byte s (pos + 1) in
    if v land 0x8000 <> 0 then v - 0x10000 else v
  ;;

  let int32 s pos =
    let v = uint32 s pos in
    if v land 0x8000_0000 <> 0 then v - 0x1_0000_0000 else v
  ;;

  let int64 s pos =
    Int64.( lor )
      (Int64.shift_left (Int64.of_int (uint32 s pos)) 32)
      (Int64.of_int (uint32 s (pos + 4)))
  ;;

  let expect_length s n ~what =
    if String.length s = n
    then Ok ()
    else Error (sprintf "binary %s: expected %d bytes, got %d" what n (String.length s))
  ;;

  (* Run a reader, turning a short buffer into an error. *)
  let read ~what f =
    match f () with
    | r -> r
    | exception Short -> Error (sprintf "binary %s: value too short" what)
  ;;
end

let no_binary _ = None

(* Binary-format decoders shared by several codecs. *)
module Binary = struct
  let int s =
    Wire.read ~what:"integer" (fun () ->
      match String.length s with
      | 2 -> Ok (Wire.int16 s 0)
      | 4 -> Ok (Wire.int32 s 0)
      | 8 ->
        let v = Wire.int64 s 0 in
        (match Int64.to_int v with
         | Some n -> Ok n
         | None -> Error (sprintf "cannot read %Ld as a 63-bit int" v))
      | n -> Error (sprintf "binary integer of %d bytes" n))
  ;;

  let int64 s =
    Wire.read ~what:"integer" (fun () ->
      match String.length s with
      | 2 -> Ok (Int64.of_int (Wire.int16 s 0))
      | 4 -> Ok (Int64.of_int (Wire.int32 s 0))
      | 8 -> Ok (Wire.int64 s 0)
      | n -> Error (sprintf "binary integer of %d bytes" n))
  ;;

  (* PostgreSQL counts timestamps in microseconds and dates in days from 2000-01-01. *)
  let pg_epoch_us = 946_684_800_000_000
  let pg_epoch_date = Date.create_exn ~y:2000 ~m:Jan ~d:1

  let timestamptz s =
    Result.bind (Wire.expect_length s 8 ~what:"timestamptz") ~f:(fun () ->
      let v = Wire.int64 s 0 in
      if Int64.equal v Int64.max_value || Int64.equal v Int64.min_value
      then Error "timestamptz infinity has no Time_ns value"
      else (
        let us = Int64.to_int_exn v + pg_epoch_us in
        if Int.abs us > Int.max_value / 1000
        then Error (sprintf "timestamptz %Ld us is outside the Time_ns range" v)
        else (
          let t = Time_ns.of_int_ns_since_epoch (us * 1000) in
          if Time_ns.( < ) t Time_ns.min_value_representable
             || Time_ns.( > ) t Time_ns.max_value_representable
          then Error (sprintf "timestamptz %Ld us is outside the Time_ns range" v)
          else Ok t)))
  ;;

  let date s =
    Result.bind (Wire.expect_length s 4 ~what:"date") ~f:(fun () ->
      let days = Wire.int32 s 0 in
      (* The text decoder reads four-digit years only; keep the two in step. *)
      match Date.add_days pg_epoch_date days with
      | d when Date.year d >= 1 && Date.year d <= 9999 -> Ok d
      | _ | (exception _) -> Error (sprintf "date %d days from 2000-01-01" days))
  ;;

  let json ~oid s =
    if oid = 114
    then Ok s
    else if String.length s >= 1 && Char.equal s.[0] '\001'
    then Ok (String.drop_prefix s 1)
    else Error "binary jsonb: unknown version byte"
  ;;

  let uuid s =
    Result.bind (Wire.expect_length s 16 ~what:"uuid") ~f:(fun () ->
      let hex = String.concat_map s ~f:(fun c -> sprintf "%02x" (Char.to_int c)) in
      Ok
        (sprintf
           "%s-%s-%s-%s-%s"
           (String.sub hex ~pos:0 ~len:8)
           (String.sub hex ~pos:8 ~len:4)
           (String.sub hex ~pos:12 ~len:4)
           (String.sub hex ~pos:16 ~len:4)
           (String.sub hex ~pos:20 ~len:12)))
  ;;

  (* One length-prefixed value at [pos] ([-1] is NULL); returns it and the next position. *)
  let value s pos =
    let len = Wire.int32 s pos in
    if len < 0
    then None, pos + 4
    else if pos + 4 + len > String.length s
    then raise Wire.Short
    else Some (String.sub s ~pos:(pos + 4) ~len), pos + 4 + len
  ;;

  let array s ~element =
    Wire.read ~what:"array" (fun () ->
      let ndim = Wire.int32 s 0 in
      let element_oid = Wire.uint32 s 8 in
      match ndim, element element_oid with
      | 0, _ -> Ok []
      | 1, Some decode ->
        let n = Wire.int32 s 12 in
        let lower_bound = Wire.int32 s 16 in
        if lower_bound <> 1
        then Error "only one-dimensional arrays with default bounds"
        else (
          let rec go i pos acc =
            if i = n
            then Ok (List.rev acc)
            else (
              let v, pos = value s pos in
              match decode v with
              | Ok x -> go (i + 1) pos (x :: acc)
              | Error msg -> Error (sprintf "element %d: %s" i msg))
          in
          go 0 20 [])
      | 1, None -> Error (sprintf "no binary decoder for element type OID %d" element_oid)
      | _ -> Error "nested arrays are not supported")
  ;;
end

let name t = t.name
let oid t = t.oid
let encode t v = t.encode v
let decode t s = t.decode s
let accepts_binary t ~oid = Option.is_some (t.binary oid)

let decode_binary t ~oid s =
  match t.binary oid with
  | Some decode -> decode s
  | None -> Error (sprintf "%s: no binary decoder for type OID %d" t.name oid)
;;

(* [binary] is the OIDs read in binary format and the decoder, which gets the column's
   OID. *)
let base ?range_oid ?binary ~name ~oid ~array_oid ~to_string ~of_string () =
  { name
  ; oid
  ; array_oid
  ; range_oid
  ; encode = (fun v -> Some (to_string v))
  ; decode =
      (function
        | None -> null_error
        | Some s -> of_string s)
  ; binary =
      (match binary with
       | None -> no_binary
       | Some (oids, decode) ->
         fun column_oid ->
           if List.mem oids column_oid ~equal:Int.equal
           then
             Some
               (function
                 | None -> null_error
                 | Some s -> decode ~oid:column_oid s)
           else None)
  }
;;

let custom ~name ~oid ?array_oid ~encode ~decode () =
  base ~name ~oid ~array_oid ~to_string:encode ~of_string:decode ()
;;

let parse_with ~what f s =
  match f s with
  | v -> Ok v
  | exception _ -> Error (sprintf "cannot read %S as %s" s what)
;;

let int =
  base
    ~range_oid:("int8range", 3926, 3927)
    ~name:"int8"
    ~oid:20
    ~array_oid:(Some 1016)
    ~to_string:Int.to_string
    ~of_string:(fun s ->
      match Int.of_string_opt s with
      | Some n -> Ok n
      | None -> Error (sprintf "cannot read %S as a 63-bit int" s))
    ~binary:([ 20; 23; 21 ], fun ~oid:_ s -> Binary.int s)
    ()
;;

let int32 =
  base
    ~range_oid:("int4range", 3904, 3905)
    ~name:"int4"
    ~oid:23
    ~array_oid:(Some 1007)
    ~to_string:Int32.to_string
    ~of_string:(parse_with ~what:"int4" Int32.of_string)
    ~binary:
      ( [ 23; 21 ]
      , fun ~oid:_ s ->
          Result.bind (Binary.int s) ~f:(fun n ->
            match Int32.of_int n with
            | Some n -> Ok n
            | None -> Error (sprintf "%d does not fit int4" n)) )
    ()
;;

let int64 =
  base
    ~range_oid:("int8range", 3926, 3927)
    ~name:"int8"
    ~oid:20
    ~array_oid:(Some 1016)
    ~to_string:Int64.to_string
    ~of_string:(parse_with ~what:"int8" Int64.of_string)
    ~binary:([ 20; 23; 21 ], fun ~oid:_ s -> Binary.int64 s)
    ()
;;

let string =
  base
    ~name:"text"
    ~oid:25
    ~array_oid:(Some 1009)
    ~to_string:Fn.id
    ~of_string:Result.return
      (* text, varchar, bpchar and name: the binary form is the text itself. *)
    ~binary:([ 25; 1043; 1042; 19 ], fun ~oid:_ s -> Ok s)
    ()
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
    ~binary:
      ( [ 16 ]
      , fun ~oid:_ s ->
          match s with
          | "\001" -> Ok true
          | "\000" -> Ok false
          | _ -> Error "binary bool: expected one byte 0 or 1" )
    ()
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
      (* Not float4: its text form is the shortest decimal, which reads back as a
         different double than the binary single-precision value. *)
    ~binary:
      ( [ 701 ]
      , fun ~oid:_ s ->
          Result.bind (Wire.expect_length s 8 ~what:"float8") ~f:(fun () ->
            Ok (Int64.float_of_bits (Wire.int64 s 0))) )
    ()
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
    ~range_oid:("tstzrange", 3910, 3911)
    ~name:"timestamptz"
    ~oid:1184
    ~array_oid:(Some 1185)
    ~to_string:Timestamptz.to_string
    ~of_string:Timestamptz.of_string
    ~binary:([ 1184 ], fun ~oid:_ s -> Binary.timestamptz s)
    ()
;;

let date =
  base
    ~range_oid:("daterange", 3912, 3913)
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
    ~binary:([ 1082 ], fun ~oid:_ s -> Binary.date s)
    ()
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
    ~binary:([ 17 ], fun ~oid:_ s -> Ok s)
    ()
;;

let json =
  base
    ~name:"jsonb"
    ~oid:3802
    ~array_oid:(Some 3807)
    ~to_string:Fn.id
    ~of_string:Result.return
    ~binary:([ 3802; 114 ], Binary.json)
    ()
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
    ~binary:
      ( [ 3802; 114 ]
      , fun ~oid s ->
          Result.bind (Binary.json ~oid s) ~f:(fun s ->
            match Yojson.Safe.from_string s with
            | j -> Ok j
            | exception Yojson.Json_error msg -> Error (sprintf "invalid JSON: %s" msg))
      )
    ()
;;

let uuid =
  base
    ~name:"uuid"
    ~oid:2950
    ~array_oid:(Some 2951)
    ~to_string:Fn.id
    ~of_string:Result.return
    ~binary:([ 2950 ], fun ~oid:_ s -> Binary.uuid s)
    ()
;;

let numeric =
  base
    ~range_oid:("numrange", 3906, 3907)
    ~name:"numeric"
    ~oid:1700
    ~array_oid:(Some 1231)
    ~to_string:Fn.id
    ~of_string:Result.return
    ()
;;

let nullable t =
  { name = t.name
  ; oid = t.oid
  ; array_oid = t.array_oid
  ; range_oid = t.range_oid
  ; encode = (fun v -> Option.bind v ~f:t.encode)
  ; decode =
      (function
        | None -> Ok None
        | Some _ as s -> Result.map (t.decode s) ~f:Option.some)
  ; binary =
      (fun oid ->
        Option.map (t.binary oid) ~f:(fun decode -> function
          | None -> Ok None
          | Some _ as s -> Result.map (decode s) ~f:Option.some))
  }
;;

let map t ~name ~f ~f_inv =
  { name
  ; oid = t.oid
  ; array_oid = t.array_oid
  ; range_oid = t.range_oid
  ; encode = (fun v -> t.encode (f_inv v))
  ; decode = (fun s -> Result.bind (t.decode s) ~f)
  ; binary =
      (fun oid ->
        Option.map (t.binary oid) ~f:(fun decode s -> Result.bind (decode s) ~f))
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
    ; range_oid = None
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
    ; binary =
        (fun column_oid ->
          if column_oid <> oid || Option.is_none (t.binary t.oid)
          then None
          else
            Some
              (function
                | None -> null_error
                | Some s -> Binary.array s ~element:t.binary))
    }
;;

module Interval = struct
  type t =
    { months : int
    ; days : int
    ; microseconds : int
    }
  [@@deriving sexp, compare, equal, hash]

  let zero = { months = 0; days = 0; microseconds = 0 }
  let of_span span = { zero with microseconds = Time_ns.Span.to_int_us span }

  let to_span t =
    if t.months = 0 && t.days = 0
    then Some (Time_ns.Span.of_int_us t.microseconds)
    else None
  ;;

  let us_per_sec = 1_000_000
  let us_per_min = 60 * us_per_sec
  let us_per_hour = 60 * us_per_min

  (* The time part goes as [-]H:MM:SS.ffffff: the server reads it exactly, where a large
     "N microseconds" count may go through a float. *)
  let to_string { months; days; microseconds } =
    let sign = if microseconds < 0 then "-" else "" in
    let us = Int.abs microseconds in
    sprintf
      "%d mons %d days %s%d:%02d:%02d.%06d"
      months
      days
      sign
      (us / us_per_hour)
      (us / us_per_min % 60)
      (us / us_per_sec % 60)
      (us % us_per_sec)
  ;;

  let fail s = Error (sprintf "cannot read %S as interval" s)

  let int_of s =
    let digits = String.chop_prefix_if_exists s ~prefix:"+" in
    match Int.of_string_opt digits with
    | Some n when not (String.is_empty digits) -> Some n
    | Some _ | None -> None
  ;;

  (* Seconds with an optional fraction ("06", "06.5", "-6.000001") as microseconds. *)
  let micros_of_seconds s =
    let negative = String.is_prefix s ~prefix:"-" in
    let body = String.lstrip s ~drop:(fun c -> Char.equal c '-' || Char.equal c '+') in
    let whole, frac =
      match String.lsplit2 body ~on:'.' with
      | None -> body, ""
      | Some (w, f) -> w, f
    in
    if String.is_empty whole
       || (not (String.for_all whole ~f:Char.is_digit))
       || String.length frac > 6
       || not (String.for_all frac ~f:Char.is_digit)
    then None
    else (
      let frac_us =
        if String.is_empty frac
        then 0
        else Int.of_string (frac ^ String.make (6 - String.length frac) '0')
      in
      match Int.of_string_opt whole with
      | None -> None
      | Some sec ->
        let us = (sec * us_per_sec) + frac_us in
        Some (if negative then -us else us))
  ;;

  (* [-]H:MM:SS[.ffffff], the time part of the [postgres] interval style. *)
  let micros_of_clock s =
    let negative = String.is_prefix s ~prefix:"-" in
    let body = String.lstrip s ~drop:(fun c -> Char.equal c '-' || Char.equal c '+') in
    match String.split body ~on:':' with
    | [ h; m; sec ] ->
      (match Int.of_string_opt h, Int.of_string_opt m, micros_of_seconds sec with
       | Some h, Some m, Some us when h >= 0 && m >= 0 && us >= 0 ->
         let total = (h * us_per_hour) + (m * us_per_min) + us in
         Some (if negative then -total else total)
       | _ -> None)
    | _ -> None
  ;;

  (* The [postgres] style the server prints by default: "1 year 2 mons -3 days
     +04:05:06.789". *)
  let of_postgres_style s =
    let rec go acc = function
      | [] -> Ok acc
      | clock :: rest when String.mem clock ':' ->
        (match micros_of_clock clock with
         | Some us -> go { acc with microseconds = acc.microseconds + us } rest
         | None -> fail s)
      | n :: unit :: rest ->
        (match int_of n, unit with
         | Some n, ("year" | "years") ->
           go { acc with months = acc.months + (12 * n) } rest
         | Some n, ("mon" | "mons") -> go { acc with months = acc.months + n } rest
         | Some n, ("day" | "days") -> go { acc with days = acc.days + n } rest
         | _ -> fail s)
      | [ _ ] -> fail s
    in
    go zero (String.split s ~on:' ' |> List.filter ~f:(Fn.non String.is_empty))
  ;;

  (* The [iso_8601] style: "P1Y2M-3DT4H5M6.789S", "PT0S". *)
  let of_iso_8601 s =
    let n = String.length s in
    let rec go i ~in_time ~start acc =
      if i >= n
      then if start = i then Ok acc else fail s
      else (
        match s.[i] with
        | 'T' when (not in_time) && start = i ->
          go (i + 1) ~in_time:true ~start:(i + 1) acc
        | ('Y' | 'M' | 'W' | 'D' | 'H' | 'S') as unit ->
          let num = String.sub s ~pos:start ~len:(i - start) in
          let next acc = go (i + 1) ~in_time ~start:(i + 1) acc in
          (match unit, in_time with
           | 'S', true ->
             (match micros_of_seconds num with
              | Some us -> next { acc with microseconds = acc.microseconds + us }
              | None -> fail s)
           | _ ->
             (match int_of num, unit, in_time with
              | Some k, 'Y', false -> next { acc with months = acc.months + (12 * k) }
              | Some k, 'M', false -> next { acc with months = acc.months + k }
              | Some k, 'W', false -> next { acc with days = acc.days + (7 * k) }
              | Some k, 'D', false -> next { acc with days = acc.days + k }
              | Some k, 'H', true ->
                next { acc with microseconds = acc.microseconds + (k * us_per_hour) }
              | Some k, 'M', true ->
                next { acc with microseconds = acc.microseconds + (k * us_per_min) }
              | _ -> fail s))
        | _ -> go (i + 1) ~in_time ~start acc)
    in
    match String.chop_prefix s ~prefix:"P" with
    | Some "" | None -> fail s
    | Some _ -> go 1 ~in_time:false ~start:1 zero
  ;;

  let of_string s =
    if String.is_prefix s ~prefix:"P" then of_iso_8601 s else of_postgres_style s
  ;;
end

let interval =
  base
    ~name:"interval"
    ~oid:1186
    ~array_oid:(Some 1187)
    ~to_string:Interval.to_string
    ~of_string:Interval.of_string
    ~binary:
      ( [ 1186 ]
      , fun ~oid:_ s ->
          Result.bind (Wire.expect_length s 16 ~what:"interval") ~f:(fun () ->
            match Int64.to_int (Wire.int64 s 0) with
            | None -> Error "interval microseconds do not fit a 63-bit int"
            | Some microseconds ->
              Ok
                { Interval.microseconds; days = Wire.int32 s 8; months = Wire.int32 s 12 })
      )
    ()
;;

module Ofday = struct
  let to_string ofday =
    let us =
      Time_ns.Span.to_int_ns (Time_ns.Ofday.to_span_since_start_of_day ofday) / 1000
    in
    sprintf
      "%02d:%02d:%02d.%06d"
      (us / Interval.us_per_hour)
      (us / Interval.us_per_min % 60)
      (us / Interval.us_per_sec % 60)
      (us % Interval.us_per_sec)
  ;;

  let of_micros us =
    match Time_ns.Ofday.of_span_since_start_of_day_exn (Time_ns.Span.of_int_us us) with
    | t -> Some t
    | exception _ -> None
  ;;

  let of_string s =
    match Interval.micros_of_clock s with
    | Some us when us >= 0 && not (String.is_prefix s ~prefix:"-") ->
      (match of_micros us with
       | Some t -> Ok t
       | None -> Error (sprintf "cannot read %S as time" s))
    | _ -> Error (sprintf "cannot read %S as time" s)
  ;;
end

let ofday =
  base
    ~name:"time"
    ~oid:1083
    ~array_oid:(Some 1183)
    ~to_string:Ofday.to_string
    ~of_string:Ofday.of_string
    ~binary:
      ( [ 1083 ]
      , fun ~oid:_ s ->
          Result.bind (Wire.expect_length s 8 ~what:"time") ~f:(fun () ->
            match Ofday.of_micros (Int64.to_int_exn (Wire.int64 s 0)) with
            | Some t -> Ok t
            | None -> Error "binary time out of range") )
    ()
;;

module Inet = struct
  module Inet_addr = Core_unix.Inet_addr

  type t =
    { address : Inet_addr.t
    ; prefix_length : int
    }
  [@@deriving compare, sexp_of]

  let equal a b = compare a b = 0
  let is_ipv6 address = String.mem (Inet_addr.to_string address) ':'
  let max_prefix address = if is_ipv6 address then 128 else 32

  let create ?prefix_length address =
    let max = max_prefix address in
    let prefix_length = Option.value prefix_length ~default:max in
    if prefix_length < 0 || prefix_length > max
    then
      Or_error.error_s
        [%message
          "Inet.create: prefix length out of range" (prefix_length : int) (max : int)]
    else Ok { address; prefix_length }
  ;;

  let to_string { address; prefix_length } =
    sprintf "%s/%d" (Inet_addr.to_string address) prefix_length
  ;;

  let of_string s =
    let fail () = Error (sprintf "cannot read %S as inet" s) in
    let addr_s, prefix =
      match String.lsplit2 s ~on:'/' with
      | None -> s, None
      | Some (a, p) -> a, Some p
    in
    match Inet_addr.of_string addr_s with
    | exception _ -> fail ()
    | address ->
      (match prefix with
       | None -> Ok { address; prefix_length = max_prefix address }
       | Some p ->
         (match Int.of_string_opt p with
          | Some n when n >= 0 && n <= max_prefix address ->
            Ok { address; prefix_length = n }
          | _ -> fail ()))
  ;;

  (* family (2 = IPv4, 3 = IPv6), prefix bits, is_cidr, address length, address. *)
  let of_binary s =
    Wire.read ~what:"inet" (fun () ->
      let family = Wire.byte s 0 in
      let bits = Wire.byte s 1 in
      let nb = Wire.byte s 3 in
      if String.length s <> 4 + nb
      then Error "binary inet: bad length"
      else (
        let byte i = Wire.byte s (4 + i) in
        let text =
          match family, nb with
          | 2, 4 -> Some (sprintf "%d.%d.%d.%d" (byte 0) (byte 1) (byte 2) (byte 3))
          | 3, 16 ->
            Some
              (List.init 8 ~f:(fun i ->
                 sprintf "%x" ((byte (2 * i) lsl 8) lor byte ((2 * i) + 1)))
               |> String.concat ~sep:":")
          | _ -> None
        in
        match text with
        | None -> Error (sprintf "binary inet: family %d with %d bytes" family nb)
        | Some text ->
          (match Inet_addr.of_string text with
           | address -> Ok { address; prefix_length = bits }
           | exception _ -> Error "binary inet: bad address")))
  ;;
end

let inet =
  base
    ~name:"inet"
    ~oid:869
    ~array_oid:(Some 1041)
    ~to_string:Inet.to_string
    ~of_string:Inet.of_string
    ~binary:([ 869 ], fun ~oid:_ s -> Inet.of_binary s)
    ()
;;

let cidr =
  base
    ~name:"cidr"
    ~oid:650
    ~array_oid:(Some 651)
    ~to_string:Inet.to_string
    ~of_string:Inet.of_string
    ~binary:([ 650 ], fun ~oid:_ s -> Inet.of_binary s)
    ()
;;

module Range = struct
  type 'a bound =
    | Inclusive of 'a
    | Exclusive of 'a
    | Unbounded
  [@@deriving sexp, compare, equal]

  type 'a t =
    | Empty
    | Non_empty of
        { lower : 'a bound
        ; upper : 'a bound
        }
  [@@deriving sexp, compare, equal]

  let quote s =
    let b = Buffer.create (String.length s + 2) in
    Buffer.add_char b '"';
    String.iter s ~f:(fun c ->
      if Char.equal c '"' || Char.equal c '\\' then Buffer.add_char b '\\';
      Buffer.add_char b c);
    Buffer.add_char b '"';
    Buffer.contents b
  ;;

  (* Elements are always quoted, so an empty string is not read back as a missing bound. *)
  let to_literal = function
    | Empty -> "empty"
    | Non_empty { lower; upper } ->
      let open_, lo =
        match lower with
        | Inclusive s -> "[", quote s
        | Exclusive s -> "(", quote s
        | Unbounded -> "(", ""
      in
      let close, hi =
        match upper with
        | Inclusive s -> "]", quote s
        | Exclusive s -> ")", quote s
        | Unbounded -> ")", ""
      in
      open_ ^ lo ^ "," ^ hi ^ close
  ;;

  (* Read one bound starting at [i], up to [stop] (',' or the closing bracket); [None] is
     a missing (infinite) bound. Returns the index of the delimiter. *)
  let read_bound s i ~stop =
    let n = String.length s in
    let b = Buffer.create 16 in
    let rec go i ~quoted ~any =
      if i >= n
      then Error "unterminated range literal"
      else (
        match s.[i] with
        | '"' when quoted ->
          if i + 1 < n && Char.equal s.[i + 1] '"'
          then (
            Buffer.add_char b '"';
            go (i + 2) ~quoted ~any)
          else go (i + 1) ~quoted:false ~any
        | '"' -> go (i + 1) ~quoted:true ~any:true
        | '\\' when i + 1 < n ->
          Buffer.add_char b s.[i + 1];
          go (i + 2) ~quoted ~any:true
        | c when (not quoted) && Char.equal c stop ->
          Ok ((if any then Some (Buffer.contents b) else None), i)
        | c ->
          Buffer.add_char b c;
          go (i + 1) ~quoted ~any:true)
    in
    go i ~quoted:false ~any:false
  ;;

  let of_literal s =
    let open Result.Let_syntax in
    let n = String.length s in
    if String.Caseless.equal (String.strip s) "empty"
    then Ok Empty
    else if n < 3
    then Error (sprintf "range literal %S is too short" s)
    else (
      let%bind lower_inclusive =
        match s.[0] with
        | '[' -> Ok true
        | '(' -> Ok false
        | _ -> Error (sprintf "range literal %S: expected [ or (" s)
      in
      let%bind upper_inclusive =
        match s.[n - 1] with
        | ']' -> Ok true
        | ')' -> Ok false
        | _ -> Error (sprintf "range literal %S: expected ] or )" s)
      in
      let%bind lo, comma = read_bound s 1 ~stop:',' in
      let%bind hi, close =
        read_bound (String.prefix s (n - 1) ^ "\000") (comma + 1) ~stop:'\000'
      in
      let%map () =
        if close = n - 1
        then Ok ()
        else Error (sprintf "range literal %S: trailing data" s)
      in
      let bound v inclusive =
        match v with
        | None -> Unbounded
        | Some v -> if inclusive then Inclusive v else Exclusive v
      in
      Non_empty { lower = bound lo lower_inclusive; upper = bound hi upper_inclusive })
  ;;

  let map t ~f =
    let bound = function
      | Inclusive v -> Inclusive (f v)
      | Exclusive v -> Exclusive (f v)
      | Unbounded -> Unbounded
    in
    match t with
    | Empty -> Empty
    | Non_empty { lower; upper } -> Non_empty { lower = bound lower; upper = bound upper }
  ;;
end

(* flags (empty 0x01, lower inclusive 0x02, upper inclusive 0x04, lower infinite 0x08,
   upper infinite 0x10), then each finite bound as a length-prefixed value. *)
let range_of_binary s ~element =
  Wire.read ~what:"range" (fun () ->
    let flags = Wire.byte s 0 in
    if flags land 0x01 <> 0
    then Ok Range.Empty
    else (
      let bound ~inclusive_bit ~infinite_bit pos =
        if flags land infinite_bit <> 0
        then Ok (Range.Unbounded, pos)
        else (
          match Binary.value s pos with
          | None, _ -> Error "binary range: NULL bound"
          | (Some _ as v), pos ->
            Result.map (element v) ~f:(fun x ->
              ( (if flags land inclusive_bit <> 0 then Range.Inclusive x else Exclusive x)
              , pos )))
      in
      let open Result.Let_syntax in
      let%bind lower, pos = bound ~inclusive_bit:0x02 ~infinite_bit:0x08 1 in
      let%map upper, _ = bound ~inclusive_bit:0x04 ~infinite_bit:0x10 pos in
      Range.Non_empty { lower; upper }))
;;

let range t =
  match t.range_oid with
  | None ->
    raise_s [%message "Type.range: no range type over element codec" (t.name : string)]
  | Some (range_name, oid, array_oid) ->
    let element_encode v =
      match t.encode v with
      | Some s -> s
      | None ->
        raise_s [%message "Type.range: an element encoded to NULL" (t.name : string)]
    in
    let decode_bound = function
      | Range.Inclusive s ->
        Result.map (t.decode (Some s)) ~f:(fun v -> Range.Inclusive v)
      | Exclusive s -> Result.map (t.decode (Some s)) ~f:(fun v -> Range.Exclusive v)
      | Unbounded -> Ok Range.Unbounded
    in
    { name = range_name
    ; oid
    ; array_oid = Some array_oid
    ; range_oid = None
    ; encode = (fun r -> Some (Range.to_literal (Range.map r ~f:element_encode)))
    ; decode =
        (function
          | None -> Error "unexpected NULL (wrap the codec in Type.nullable)"
          | Some s ->
            Result.bind (Range.of_literal s) ~f:(function
              | Range.Empty -> Ok Range.Empty
              | Non_empty { lower; upper } ->
                let open Result.Let_syntax in
                let%bind lower = decode_bound lower in
                let%map upper = decode_bound upper in
                Range.Non_empty { lower; upper }))
    ; binary =
        (fun column_oid ->
          match column_oid = oid, t.binary t.oid with
          | true, Some element ->
            Some
              (function
                | None -> null_error
                | Some s -> range_of_binary s ~element)
          | _ -> None)
    }
;;
