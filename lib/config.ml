open! Core

module Ssl_mode = struct
  type t =
    | Disable
    | Allow
    | Prefer
    | Require
    | Verify_ca
    | Verify_full
  [@@deriving sexp, compare, equal, enumerate]

  let to_string = function
    | Disable -> "disable"
    | Allow -> "allow"
    | Prefer -> "prefer"
    | Require -> "require"
    | Verify_ca -> "verify-ca"
    | Verify_full -> "verify-full"
  ;;

  let of_string s =
    match List.find all ~f:(fun t -> String.equal (to_string t) s) with
    | Some t -> Ok t
    | None -> Or_error.error_s [%message "unknown sslmode" ~_:(s : string)]
  ;;
end

type t =
  { host : string option
  ; port : int option
  ; user : string option
  ; password : string option
  ; dbname : string option
  ; ssl_mode : Ssl_mode.t
  ; ssl_root_cert : string option
  ; application_name : string
  ; connect_timeout : Time_ns.Span.t
  ; statement_timeout : Time_ns.Span.t option
  ; query_timeout : Time_ns.Span.t option
  ; statement_cache_size : int
  ; extra : (string * string) list
  }

let sexp_of_t t =
  [%sexp
    { host : string option = t.host
    ; port : int option = t.port
    ; user : string option = t.user
    ; password = (Option.map t.password ~f:(const "***") : string option)
    ; dbname : string option = t.dbname
    ; ssl_mode : Ssl_mode.t = t.ssl_mode
    ; ssl_root_cert : string option = t.ssl_root_cert
    ; application_name : string = t.application_name
    ; connect_timeout : Time_ns.Span.t = t.connect_timeout
    ; statement_timeout : Time_ns.Span.t option = t.statement_timeout
    ; query_timeout : Time_ns.Span.t option = t.query_timeout
    ; statement_cache_size : int = t.statement_cache_size
    ; extra : (string * string) list = t.extra
    }]
;;

let default_application_name = "pg-async"
let default_statement_cache_size = 256

let create
  ?host
  ?port
  ?user
  ?password
  ?dbname
  ?(ssl_mode = Ssl_mode.Verify_full)
  ?ssl_root_cert
  ?(application_name = default_application_name)
  ?(connect_timeout = Time_ns.Span.of_int_sec 10)
  ?statement_timeout
  ?query_timeout
  ?(statement_cache_size = default_statement_cache_size)
  ?(extra = [])
  ()
  =
  if statement_cache_size < 0
  then
    raise_s
      [%message
        "Config.create: statement_cache_size must not be negative"
          (statement_cache_size : int)];
  { host
  ; port
  ; user
  ; password
  ; dbname
  ; ssl_mode
  ; ssl_root_cert
  ; application_name
  ; connect_timeout
  ; statement_timeout
  ; query_timeout
  ; statement_cache_size
  ; extra
  }
;;

let parse_int ~what s =
  match Int.of_string_opt s with
  | Some n when n >= 0 -> Ok n
  | Some _ | None ->
    Or_error.error_s [%message "invalid number" (what : string) ~value:(s : string)]
;;

(* [host[:port]], with brackets around an IPv6 literal. *)
let split_host_port entry =
  let host, port =
    if String.is_prefix entry ~prefix:"["
    then (
      match String.lsplit2 entry ~on:']' with
      | Some (h, rest) -> String.drop_prefix h 1, String.chop_prefix rest ~prefix:":"
      | None -> entry, None)
    else (
      match String.rsplit2 entry ~on:':' with
      | Some (h, p) -> h, Some p
      | None -> entry, None)
  in
  Uri.pct_decode host, port
;;

(* libpq URLs may list several hosts ([postgres://h1:5432,h2:5433/db]), which [Uri] does
   not parse. Take the host list out, parse the rest with the first host, and return the
   list. *)
let extract_hosts url =
  match String.substr_index url ~pattern:"://" with
  | None -> url, None
  | Some i ->
    let start = i + 3 in
    let stop =
      String.lfindi url ~pos:start ~f:(fun _ c -> Char.equal c '/' || Char.equal c '?')
      |> Option.value ~default:(String.length url)
    in
    let authority = String.sub url ~pos:start ~len:(stop - start) in
    let userinfo, hosts =
      match String.rsplit2 authority ~on:'@' with
      | Some (u, h) -> u ^ "@", h
      | None -> "", authority
    in
    if not (String.mem hosts ',')
    then url, None
    else (
      let entries = String.split hosts ~on:',' in
      let first = List.hd_exn entries in
      ( String.prefix url start ^ userinfo ^ first ^ String.drop_prefix url stop
      , Some (List.map entries ~f:split_host_port) ))
;;

let of_url url =
  let open Or_error.Let_syntax in
  let url, hosts = extract_hosts url in
  let uri = Uri.of_string url in
  let%bind () =
    match Uri.scheme uri with
    | Some ("postgres" | "postgresql") -> Ok ()
    | _ -> Or_error.error_s [%message "expected a postgres:// or postgresql:// URL"]
  in
  let nonempty = function
    | Some "" | None -> None
    | Some s -> Some s
  in
  let dbname =
    match String.chop_prefix (Uri.path uri) ~prefix:"/" with
    | Some "" | None -> None
    | Some db -> Some (Uri.pct_decode db)
  in
  let host = Uri.host uri |> Option.map ~f:Uri.pct_decode |> nonempty in
  (* Several hosts: libpq takes comma-separated [host] and [port] lists, a port per host
     (an empty entry means the default). *)
  let%bind host, port, extra =
    match hosts with
    | None -> Ok (host, Uri.port uri, [])
    | Some hosts ->
      let%map ports =
        List.map hosts ~f:(fun (_, port) ->
          match port with
          | None | Some "" -> Ok ""
          | Some p -> parse_int ~what:"port" p >>| Int.to_string)
        |> Or_error.all
      in
      let host = String.concat ~sep:"," (List.map hosts ~f:fst) in
      if List.for_all ports ~f:String.is_empty
      then Some host, None, []
      else Some host, None, [ "port", String.concat ~sep:"," ports ]
  in
  let t =
    create
      ?host
      ?port
      ~extra
      ?user:(Uri.user uri |> Option.map ~f:Uri.pct_decode |> nonempty)
      ?password:(Uri.password uri |> Option.map ~f:Uri.pct_decode |> nonempty)
      ?dbname
      ()
  in
  List.fold_result (Uri.query uri) ~init:t ~f:(fun t (key, values) ->
    let value = String.concat ~sep:"," values in
    match key with
    | "sslmode" ->
      let%map ssl_mode = Ssl_mode.of_string value in
      { t with ssl_mode }
    | "sslrootcert" -> Ok { t with ssl_root_cert = Some value }
    | "application_name" -> Ok { t with application_name = value }
    | "connect_timeout" ->
      let%map s = parse_int ~what:"connect_timeout" value in
      { t with connect_timeout = Time_ns.Span.of_int_sec s }
    | "statement_timeout" ->
      let%map ms = parse_int ~what:"statement_timeout" value in
      { t with statement_timeout = Some (Time_ns.Span.of_int_ms ms) }
    | "query_timeout" ->
      let%map ms = parse_int ~what:"query_timeout" value in
      { t with query_timeout = Some (Time_ns.Span.of_int_ms ms) }
    | "statement_cache_size" ->
      let%map n = parse_int ~what:"statement_cache_size" value in
      { t with statement_cache_size = n }
    | _ -> Ok { t with extra = t.extra @ [ key, value ] })
;;

let of_env ?(getenv = Sys.getenv) () =
  let open Or_error.Let_syntax in
  match getenv "DATABASE_URL" with
  | Some url when not (String.is_empty url) -> of_url url
  | Some _ | None ->
    let get name =
      match getenv name with
      | Some "" | None -> None
      | Some v -> Some v
    in
    let%bind port =
      match get "PGPORT" with
      | None -> Ok None
      | Some p -> parse_int ~what:"PGPORT" p >>| Option.some
    in
    let%bind ssl_mode =
      match get "PGSSLMODE" with
      | None -> Ok Ssl_mode.Verify_full
      | Some m -> Ssl_mode.of_string m
    in
    let%map connect_timeout =
      match get "PGCONNECT_TIMEOUT" with
      | None -> Ok (Time_ns.Span.of_int_sec 10)
      | Some s -> parse_int ~what:"PGCONNECT_TIMEOUT" s >>| Time_ns.Span.of_int_sec
    in
    create
      ?host:(get "PGHOST")
      ?port
      ?user:(get "PGUSER")
      ?password:(get "PGPASSWORD")
      ?dbname:(get "PGDATABASE")
      ~ssl_mode
      ?ssl_root_cert:(get "PGSSLROOTCERT")
      ?application_name:(get "PGAPPNAME")
      ~connect_timeout
      ()
;;

(* libpq conninfo values: single-quoted, with backslash and quote escaped. *)
let quote value =
  let b = Buffer.create (String.length value + 2) in
  Buffer.add_char b '\'';
  String.iter value ~f:(fun c ->
    if Char.equal c '\'' || Char.equal c '\\' then Buffer.add_char b '\\';
    Buffer.add_char b c);
  Buffer.add_char b '\'';
  Buffer.contents b
;;

let to_conninfo t =
  let opt key v = Option.map v ~f:(fun v -> key, v) in
  let seconds span =
    (* libpq treats 0 as "wait forever" and rounds anything below 2 s up to 2 s. *)
    Int.to_string (Int.max 1 (Float.iround_up_exn (Time_ns.Span.to_sec span)))
  in
  [ opt "host" t.host
  ; opt "port" (Option.map t.port ~f:Int.to_string)
  ; opt "user" t.user
  ; opt "password" t.password
  ; opt "dbname" t.dbname
  ; Some ("sslmode", Ssl_mode.to_string t.ssl_mode)
  ; opt "sslrootcert" t.ssl_root_cert
  ; Some ("application_name", t.application_name)
  ; Some ("connect_timeout", seconds t.connect_timeout)
  ]
  |> List.filter_opt
  |> fun l ->
  l @ t.extra |> List.map ~f:(fun (k, v) -> k ^ "=" ^ quote v) |> String.concat ~sep:" "
;;

let to_string_hum t =
  let user =
    match t.user, t.password with
    | None, _ -> ""
    | Some u, None -> u ^ "@"
    | Some u, Some _ -> u ^ ":***@"
  in
  let hosts =
    let host = Option.value t.host ~default:"" in
    match t.port, List.Assoc.find t.extra "port" ~equal:String.equal with
    | Some p, _ -> host ^ ":" ^ Int.to_string p
    | None, None -> host
    | None, Some ports ->
      (match List.zip (String.split host ~on:',') (String.split ports ~on:',') with
       | Ok pairs ->
         List.map pairs ~f:(fun (h, p) ->
           let h = if String.mem h ':' then "[" ^ h ^ "]" else h in
           if String.is_empty p then h else h ^ ":" ^ p)
         |> String.concat ~sep:","
       | Unequal_lengths -> host)
  in
  sprintf
    "postgres://%s%s/%s?sslmode=%s&application_name=%s"
    user
    hosts
    (Option.value t.dbname ~default:"")
    (Ssl_mode.to_string t.ssl_mode)
    t.application_name
;;
