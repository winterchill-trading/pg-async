open! Core
open! Async

module Migration = struct
  type t =
    { version : int
    ; name : string
    ; sql : string
    }

  let sexp_of_t t = [%sexp { version : int = t.version; name : string = t.name }]
  let checksum t = Md5.digest_string t.sql |> Md5.to_hex
end

module Error = struct
  type t =
    | Pg of Pg_error.t
    | Invalid of string
    | Checksum_mismatch of
        { version : int
        ; name : string
        ; applied : string
        ; file : string
        }
    | Missing of
        { version : int
        ; name : string
        }
    | Out_of_order of
        { version : int
        ; latest_applied : int
        }
  [@@deriving sexp_of]

  let to_error t = Core.Error.create_s [%message "migration failed" ~_:(t : t)]
end

let pg r = Result.map_error r ~f:(fun e -> Error.Pg e)

let parse_file_name file =
  match String.chop_suffix file ~suffix:".sql" with
  | None -> None
  | Some stem ->
    let digits = String.take_while stem ~f:Char.is_digit in
    if String.is_empty digits
    then None
    else (
      let rest = String.drop_prefix stem (String.length digits) in
      let name = String.chop_prefix rest ~prefix:"_" |> Option.value ~default:rest in
      Some (Int.of_string digits, name))
;;

let check_unique migrations =
  let sorted =
    List.sort migrations ~compare:(fun (a : Migration.t) b ->
      Int.compare a.version b.version)
  in
  match
    List.find_consecutive_duplicate sorted ~equal:(fun (a : Migration.t) b ->
      a.version = b.version)
  with
  | Some (a, b) ->
    Error
      (Error.Invalid (sprintf "version %d used twice (%s, %s)" a.version a.name b.name))
  | None -> Ok sorted
;;

let load_dir dir =
  match%bind Monitor.try_with (fun () -> Sys.readdir dir) with
  | Error exn ->
    return (Error (Error.Invalid (sprintf "cannot read %s: %s" dir (Exn.to_string exn))))
  | Ok files ->
    let files =
      Array.to_list files
      |> List.filter_map ~f:(fun f -> Option.map (parse_file_name f) ~f:(fun p -> f, p))
    in
    let%map migrations =
      Deferred.List.map ~how:`Sequential files ~f:(fun (file, (version, name)) ->
        let%map sql = Reader.file_contents (dir ^/ file) in
        { Migration.version; name; sql })
    in
    check_unique migrations
;;

let table_ddl =
  "CREATE TABLE IF NOT EXISTS schema_migrations (version bigint PRIMARY KEY, name text \
   NOT NULL, checksum text NOT NULL, applied_at timestamptz NOT NULL DEFAULT now())"
;;

(* An arbitrary constant naming this library's lock. *)
let lock_key = 0x7067_6173_796e_63

type applied =
  { version : int
  ; name : string
  ; checksum : string
  ; applied_at : Time_ns.t
  }

let applied_row =
  let open Row.Let_syntax in
  let%map_open version = column "version" int
  and name = column "name" string
  and checksum = column "checksum" string
  and applied_at = column "applied_at" time_ns in
  { version; name; checksum; applied_at }
;;

let read_applied conn =
  Query.fetch_all
    ~label:"migrate"
    conn
    "SELECT version, name, checksum, applied_at FROM schema_migrations ORDER BY version"
    ~row:applied_row
;;

let verify (migrations : Migration.t list) applied =
  let by_version =
    Int.Map.of_alist_reduce
      (List.map migrations ~f:(fun m -> m.version, m))
      ~f:(fun a _ -> a)
  in
  let applied_versions =
    Int.Set.of_list (List.map applied ~f:(fun (a : applied) -> a.version))
  in
  let problems =
    List.filter_map applied ~f:(fun (a : applied) ->
      match Map.find by_version a.version with
      | None -> Some (Error.Missing { version = a.version; name = a.name })
      | Some m ->
        let file = Migration.checksum m in
        if String.equal file a.checksum
        then None
        else
          Some
            (Checksum_mismatch
               { version = a.version; name = m.name; applied = a.checksum; file }))
  in
  match problems with
  | p :: _ -> Error p
  | [] ->
    let pending =
      List.filter migrations ~f:(fun m -> not (Set.mem applied_versions m.version))
    in
    (match Set.max_elt applied_versions, pending with
     | Some latest_applied, m :: _ when m.version < latest_applied ->
       Error (Error.Out_of_order { version = m.version; latest_applied })
     | _ -> Ok pending)
;;

let apply conn migrations =
  match check_unique migrations with
  | Error _ as e -> return e
  | Ok migrations ->
    let%map r =
      Tx.with_transaction conn ~commit_if:Result.is_ok ~f:(fun conn ->
        let open Deferred.Result.Let_syntax in
        let%bind (_ : unit list) =
          Query.fetch_all
            ~label:"migrate"
            conn
            "SELECT pg_advisory_xact_lock($1)"
            ~params:[ Param.int lock_key ]
            ~row:(Row.return ())
        in
        let%bind () = Connection.execute_script ~label:"migrate" conn table_ddl in
        let%bind applied = read_applied conn in
        match verify migrations applied with
        | Error e -> return (Error e)
        | Ok pending ->
          let rec run acc = function
            | [] -> return (Ok (List.rev acc))
            | (m : Migration.t) :: rest ->
              let%bind () = Connection.execute_script ~label:"migrate" conn m.sql in
              let%bind (_ : int) =
                Query.execute
                  ~label:"migrate"
                  conn
                  "INSERT INTO schema_migrations (version, name, checksum) VALUES ($1, \
                   $2, $3)"
                  ~params:
                    [ Param.int m.version
                    ; Param.string m.name
                    ; Param.string (Migration.checksum m)
                    ]
              in
              run (m.version :: acc) rest
          in
          run [] pending)
    in
    (match r with
     | Error e -> Error (Error.Pg e)
     | Ok r -> r)
;;

module Status = struct
  type state =
    | Applied of Time_ns.Alternate_sexp.t
    | Pending
    | Checksum_mismatch
  [@@deriving sexp_of]

  type t =
    { version : int
    ; name : string
    ; state : state
    }
  [@@deriving sexp_of]
end

let status conn migrations =
  match check_unique migrations with
  | Error _ as e -> return e
  | Ok migrations ->
    let%map applied =
      match%bind
        Query.fetch_one
          ~label:"migrate"
          conn
          "SELECT to_regclass('schema_migrations') IS NOT NULL"
          ~row:(Row.nth 0 Type.bool)
      with
      | Ok false -> return (Ok [])
      | Ok true -> read_applied conn
      | Error _ as e -> return e
    in
    Result.map (pg applied) ~f:(fun applied ->
      let applied =
        Int.Map.of_alist_reduce
          (List.map applied ~f:(fun a -> a.version, a))
          ~f:(fun a _ -> a)
      in
      List.map migrations ~f:(fun (m : Migration.t) ->
        let state : Status.state =
          match Map.find applied m.version with
          | None -> Pending
          | Some a when String.equal a.checksum (Migration.checksum m) ->
            Applied a.applied_at
          | Some _ -> Checksum_mismatch
        in
        { Status.version = m.version; name = m.name; state }))
;;
