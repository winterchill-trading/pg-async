(* Generators for values PostgreSQL can store and give back unchanged. *)

open! Core
module G = Quickcheck.Generator

let utf8_string =
  (* Mostly ASCII, with the characters array and COPY syntax care about, plus the rest of
     Unicode. Never NUL: text and jsonb reject it. *)
  let special =
    G.of_list [ '"'; '\\'; '{'; '}'; ','; ' '; '\t'; '\n'; '\r'; '\''; 'N' ]
  in
  let uchar =
    G.weighted_union
      [ 6., G.map (Int.gen_incl 1 127) ~f:Uchar.of_scalar_exn
      ; 3., G.map special ~f:Uchar.of_char
      ; 1., G.map (Int.gen_incl 0x80 0xD7FF) ~f:Uchar.of_scalar_exn
      ; 1., G.map (Int.gen_incl 0xE000 0x10FFFF) ~f:Uchar.of_scalar_exn
      ]
  in
  G.map (G.list uchar) ~f:(fun us ->
    let b = Stdlib.Buffer.create 16 in
    List.iter us ~f:(Stdlib.Buffer.add_utf_8_uchar b);
    Stdlib.Buffer.contents b)
;;

let micros_between lo hi =
  let to_us t = Time_ns.to_int_ns_since_epoch t / 1000 in
  G.map
    (Int.gen_incl (to_us lo) (to_us hi))
    ~f:(fun us -> Time_ns.of_int_ns_since_epoch (us * 1000))
;;

let time_ns =
  let t s = Time_ns.of_string_with_utc_offset s in
  micros_between (t "1900-01-01 00:00:00Z") (t "2100-01-01 00:00:00Z")
;;

let date =
  Date.gen_incl (Date.create_exn ~y:1 ~m:Jan ~d:1) (Date.create_exn ~y:9999 ~m:Dec ~d:31)
;;

let float =
  G.weighted_union
    [ 8., Float.quickcheck_generator
    ; 1., G.of_list [ Float.nan; Float.infinity; Float.neg_infinity; -0.; 0.; 0.1 ]
    ]
;;

let float_equal a b =
  (Float.is_nan a && Float.is_nan b)
  || Int64.equal (Int64.bits_of_float a) (Int64.bits_of_float b)
;;

let uuid =
  G.map
    (G.list_with_length 16 (Int.gen_incl 0 255))
    ~f:(fun bytes ->
      let hex =
        String.concat_map
          (String.of_char_list (List.map bytes ~f:Char.of_int_exn))
          ~f:(fun c -> sprintf "%02x" (Char.to_int c))
      in
      sprintf
        "%s-%s-%s-%s-%s"
        (String.sub hex ~pos:0 ~len:8)
        (String.sub hex ~pos:8 ~len:4)
        (String.sub hex ~pos:12 ~len:4)
        (String.sub hex ~pos:16 ~len:4)
        (String.sub hex ~pos:20 ~len:12))
;;

let numeric =
  let open G.Let_syntax in
  let%map negative = G.bool
  and int_part = Int.gen_incl 0 1_000_000_000
  and frac =
    Option.quickcheck_generator
      (G.bind (Int.gen_incl 1 12) ~f:(fun n -> String.gen_with_length n Char.gen_digit))
  in
  let int_part = Int.to_string int_part in
  let sign =
    if negative && not (String.equal int_part "0" && Option.is_none frac) then "-" else ""
  in
  match frac with
  | None -> sign ^ int_part
  | Some f -> sign ^ int_part ^ "." ^ f
;;

(* Values jsonb stores as given: unique keys (it keeps the last duplicate and sorts keys,
   so tests compare canonical forms), floats with a short exact decimal form. *)
let yojson : Yojson.Safe.t G.t =
  G.recursive_union
    [ G.return `Null
    ; G.map G.bool ~f:(fun b -> `Bool b)
    ; G.map Int.quickcheck_generator ~f:(fun i -> `Int i)
    ; G.map (Int.gen_incl (-1_000_000) 1_000_000) ~f:(fun k ->
        `Float (Float.of_int k /. 8.))
    ; G.map utf8_string ~f:(fun s -> `String s)
    ]
    ~f:(fun self ->
      [ G.map (G.list self) ~f:(fun l -> `List l)
      ; G.map (G.list (G.both utf8_string self)) ~f:(fun l -> `Assoc l)
      ])
;;

let rec canonical_json : Yojson.Safe.t -> Yojson.Safe.t = function
  | `Assoc l ->
    let last_wins =
      List.fold l ~init:String.Map.empty ~f:(fun m (k, v) -> Map.set m ~key:k ~data:v)
    in
    `Assoc (Map.to_alist last_wins |> List.map ~f:(fun (k, v) -> k, canonical_json v))
  | `List l -> `List (List.map l ~f:canonical_json)
  | `Float f when Float.is_integer f && Float.(abs f < 1e15) ->
    (* jsonb keeps "2.0" as 2.0 but Yojson may print 2. as "2.0"; both read back as Float,
       so compare floats by value. *)
    `Float f
  | j -> j
;;

let interval =
  let open G.Let_syntax in
  let%map months = Int.gen_incl (-100_000) 100_000
  and days = Int.gen_incl (-1_000_000) 1_000_000
  and microseconds =
    G.weighted_union
      [ 3., Int.gen_incl (-1_000_000_000_000_000) 1_000_000_000_000_000
      ; 1., Int.gen_incl (-2_000_000) 2_000_000
      ]
  in
  { Pg_async.Type.Interval.months; days; microseconds }
;;

let ofday =
  G.map (Int.gen_incl 0 86_400_000_000) ~f:(fun us ->
    Time_ns.Ofday.of_span_since_start_of_day_exn (Time_ns.Span.of_int_us us))
;;

(* [bits] leading bits of [bytes] kept, the rest cleared, as cidr requires. *)
let mask bytes ~bits =
  List.mapi bytes ~f:(fun i b ->
    let keep = Int.clamp_exn (bits - (8 * i)) ~min:0 ~max:8 in
    b land (0xff lsl (8 - keep)) land 0xff)
;;

let address_of_bytes bytes =
  match bytes with
  | [ a; b; c; d ] -> Core_unix.Inet_addr.of_string (sprintf "%d.%d.%d.%d" a b c d)
  | _ ->
    List.chunks_of bytes ~length:2
    |> List.map ~f:(function
      | [ hi; lo ] -> sprintf "%x" ((hi lsl 8) lor lo)
      | _ -> assert false)
    |> String.concat ~sep:":"
    |> Core_unix.Inet_addr.of_string
;;

let inet_with ~masked =
  let open G.Let_syntax in
  let%bind width = G.of_list [ 4; 16 ] in
  let%bind bytes = G.list_with_length width (Int.gen_incl 0 255) in
  let%map prefix_length = Int.gen_incl 0 (8 * width) in
  let bytes = if masked then mask bytes ~bits:prefix_length else bytes in
  { Pg_async.Type.Inet.address = address_of_bytes bytes; prefix_length }
;;

let inet = inet_with ~masked:false
let cidr = inet_with ~masked:true

module R = Pg_async.Type.Range

(* Ranges the server returns unchanged: for discrete element types, the canonical
   [lower, upper) form with lower < upper. *)
let discrete_range gen ~compare =
  let open G.Let_syntax in
  let%bind a = gen
  and b = gen in
  let lo, hi = if compare a b <= 0 then a, b else b, a in
  if compare lo hi = 0
  then return R.Empty
  else (
    let%map lower = G.of_list [ R.Inclusive lo; R.Unbounded ]
    and upper = G.of_list [ R.Exclusive hi; R.Unbounded ] in
    R.Non_empty { lower; upper })
;;

let continuous_range gen ~compare =
  let open G.Let_syntax in
  let%bind a = gen
  and b = gen in
  let lo, hi = if compare a b <= 0 then a, b else b, a in
  if compare lo hi = 0
  then return R.Empty
  else (
    let%map lower = G.of_list [ R.Inclusive lo; R.Exclusive lo; R.Unbounded ]
    and upper = G.of_list [ R.Inclusive hi; R.Exclusive hi; R.Unbounded ] in
    R.Non_empty { lower; upper })
;;
