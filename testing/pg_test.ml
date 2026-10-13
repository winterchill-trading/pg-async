open! Core
open! Async

type t =
  { bindir : string
  ; dir : string
  ; port : int
  ; mutable stopped : bool
  }

let port t = t.port
let data_dir t = t.dir ^/ "data"

let config ?(dbname = "postgres") ?application_name t =
  Pg_async.Config.create
    ~host:"127.0.0.1"
    ~port:t.port
    ~user:"postgres"
    ~dbname
    ~ssl_mode:Disable
    ?application_name
    ()
;;

let file_exists path =
  match%map Sys.file_exists path with
  | `Yes -> true
  | `No | `Unknown -> false
;;

let has_initdb dir = file_exists (dir ^/ "initdb")

let debian_bindirs () =
  match%map Monitor.try_with (fun () -> Sys.readdir "/usr/lib/postgresql") with
  | Error _ -> []
  | Ok versions ->
    Array.to_list versions
    |> List.filter_map ~f:(fun v -> Option.map (Int.of_string_opt v) ~f:(fun n -> n, v))
    |> List.sort ~compare:(fun (a, _) (b, _) -> Int.compare b a)
    |> List.map ~f:(fun (_, v) -> "/usr/lib/postgresql" ^/ v ^/ "bin")
;;

let find_bindir () =
  let%bind from_pg_config =
    match%map Process.run ~prog:"pg_config" ~args:[ "--bindir" ] () with
    | Ok out -> [ String.strip out ]
    | Error _ -> []
  in
  let%bind debian = debian_bindirs () in
  let candidates =
    Option.to_list (Sys.getenv "PG_BINDIR")
    @ from_pg_config
    @ [ "/opt/homebrew/opt/postgresql@18/bin"
      ; "/opt/homebrew/opt/postgresql@17/bin"
      ; "/usr/local/opt/postgresql@18/bin"
      ]
    @ debian
  in
  match%map Deferred.List.find candidates ~f:has_initdb with
  | Some dir -> Ok dir
  | None ->
    Or_error.error_s
      [%message
        "no PostgreSQL server binaries (initdb) found; set PG_BINDIR"
          (candidates : string list)]
;;

let free_port () =
  let s = Core_unix.socket ~domain:PF_INET ~kind:SOCK_STREAM ~protocol:0 () in
  Exn.protect
    ~finally:(fun () -> Core_unix.close s)
    ~f:(fun () ->
      Core_unix.bind s ~addr:(ADDR_INET (Core_unix.Inet_addr.localhost, 0));
      match Core_unix.getsockname s with
      | ADDR_INET (_, port) -> port
      | ADDR_UNIX _ -> assert false)
;;

let run_tool ~bindir prog args =
  Process.run ~prog:(bindir ^/ prog) ~args ()
  |> Deferred.Or_error.map ~f:(ignore : string -> unit)
;;

let start_once ~bindir ~server_options ~setup_data_dir =
  let open Deferred.Or_error.Let_syntax in
  (* A short path: the Unix socket lives here and macOS caps socket paths at 104 bytes. *)
  let dir = Filename_unix.temp_dir ~in_dir:"/tmp" "pg-async-" "" in
  let data = dir ^/ "data" in
  let%bind () =
    run_tool
      ~bindir
      "initdb"
      [ "-D"
      ; data
      ; "-U"
      ; "postgres"
      ; "-A"
      ; "trust"
      ; "-E"
      ; "UTF8"
      ; "--locale=C"
      ; "--no-sync"
      ]
  in
  let%bind () = Deferred.ok (setup_data_dir data) in
  let port = free_port () in
  let options =
    [ "port", Int.to_string port
    ; "listen_addresses", "127.0.0.1"
    ; "unix_socket_directories", dir
    ; "fsync", "off"
    ; "synchronous_commit", "off"
    ; "full_page_writes", "off"
    ; "max_connections", "200"
    ]
    @ server_options
    |> List.map ~f:(fun (k, v) -> sprintf "-c %s=%s" k v)
    |> String.concat ~sep:" "
  in
  let t = { bindir; dir; port; stopped = false } in
  match%bind.Deferred
    run_tool
      ~bindir
      "pg_ctl"
      [ "-D"; data; "-l"; dir ^/ "server.log"; "-w"; "-t"; "60"; "-o"; options; "start" ]
  with
  | Ok () -> return t
  | Error e ->
    let%bind.Deferred log =
      Monitor.try_with (fun () -> Reader.file_contents (dir ^/ "server.log"))
    in
    let log = Result.ok log |> Option.value ~default:"" in
    let%bind.Deferred (_ : string Or_error.t) =
      Process.run ~prog:"rm" ~args:[ "-rf"; dir ] ()
    in
    Deferred.Or_error.fail (Error.tag_s e ~tag:[%message "pg_ctl start" (log : string)])
;;

let start ?bindir ?(server_options = []) ?(setup_data_dir = fun _ -> return ()) () =
  let open Deferred.Or_error.Let_syntax in
  let%bind bindir =
    match bindir with
    | Some b -> return b
    | None -> find_bindir ()
  in
  (* The free port can be taken between probing and binding; try a few times. *)
  let rec attempt n =
    match%bind.Deferred start_once ~bindir ~server_options ~setup_data_dir with
    | Ok t -> return t
    | Error _ when n > 1 -> attempt (n - 1)
    | Error _ as e -> Deferred.return e
  in
  attempt 3
;;

let stop_sync t =
  if not t.stopped
  then (
    t.stopped <- true;
    let cmd =
      sprintf
        "%s -D %s -m immediate -w stop >/dev/null 2>&1; rm -rf %s"
        (Filename.quote (t.bindir ^/ "pg_ctl"))
        (Filename.quote (data_dir t))
        (Filename.quote t.dir)
    in
    ignore (Core_unix.system cmd : Core_unix.Exit_or_signal.t))
;;

let stop t =
  if t.stopped
  then return ()
  else (
    t.stopped <- true;
    let%bind (_ : unit Or_error.t) =
      run_tool
        ~bindir:t.bindir
        "pg_ctl"
        [ "-D"; data_dir t; "-m"; "immediate"; "-w"; "stop" ]
    in
    let%map (_ : string Or_error.t) = Process.run ~prog:"rm" ~args:[ "-rf"; t.dir ] () in
    ())
;;

let with_temp_cluster ?server_options ?setup_data_dir f =
  let%bind t = start ?server_options ?setup_data_dir () >>| ok_exn in
  Monitor.protect (fun () -> f t) ~finally:(fun () -> stop t)
;;

let database_counter = ref 0

let create_database t =
  incr database_counter;
  let dbname =
    sprintf "test_%d_%d" (Core_unix.getpid () |> Pid.to_int) !database_counter
  in
  let%bind conn = Pg_async.Connection.connect (config t) >>| Pg_async.Pg_error.ok_exn in
  (* CREATE DATABASE takes an identifier, not a parameter; the name is generated above
     from digits only. *)
  let%bind () =
    Pg_async.Connection.execute_script conn (sprintf "CREATE DATABASE %s" dbname)
    >>| Pg_async.Pg_error.ok_exn
  in
  let%map () = Pg_async.Connection.close conn in
  config ~dbname t
;;

let shared_cluster =
  lazy
    (start ()
     >>| ok_exn
     >>| fun t ->
     at_exit (fun () -> stop_sync t);
     t)
;;

let shared () = Lazy.force shared_cluster
