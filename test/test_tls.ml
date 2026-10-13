open! Core
open! Async
open Pg_async

(* A CA and a server certificate for 127.0.0.1, made with the openssl CLI. *)
let make_certs dir =
  let script =
    {|set -e
openssl req -x509 -newkey rsa:2048 -nodes -days 2 -subj /CN=test-ca -keyout ca.key -out ca.crt
openssl req -x509 -newkey rsa:2048 -nodes -days 2 -subj /CN=other-ca -keyout other.key -out other.crt
openssl req -newkey rsa:2048 -nodes -subj /CN=127.0.0.1 -keyout server.key -out server.csr
printf 'subjectAltName=IP:127.0.0.1\n' > san.ext
openssl x509 -req -in server.csr -CA ca.crt -CAkey ca.key -CAcreateserial -days 2 -extfile san.ext -out server.crt
chmod 600 server.key|}
  in
  Process.run ~working_dir:dir ~prog:"sh" ~args:[ "-c"; script ] ()
  |> Deferred.Or_error.ok_exn
  |> Deferred.ignore_m
;;

let try_connect config =
  match%bind Connection.connect config with
  | Error (Connect msg) ->
    (* libpq prefixes each attempt with the address and port; keep the reasons. On macOS
       "localhost" tries ::1 first, where nothing listens; Linux runners map it to
       127.0.0.1 only, so that attempt is dropped from the output. *)
    let reasons =
      String.split_lines msg
      |> List.filter_map ~f:(fun line ->
        match String.substr_index line ~pattern:"failed: " with
        | Some i -> Some (String.drop_prefix line (i + 8))
        | None -> None)
      |> List.filter ~f:(fun r ->
        not (String.is_substring r ~substring:"Connection refused"))
      |> List.dedup_and_sort ~compare:String.compare
    in
    return (sprintf "refused: %s" (String.concat ~sep:" | " reasons))
  | Error e -> return (Sexp.to_string [%sexp (e : Pg_error.t)])
  | Ok conn ->
    let%bind r =
      Query.fetch_one
        conn
        "SELECT ssl FROM pg_stat_ssl WHERE pid = pg_backend_pid()"
        ~row:(Row.nth 0 Type.bool)
    in
    let%map () = Connection.close conn in
    sprintf "connected, ssl=%b" (Pg_error.ok_exn r)
;;

let%expect_test "TLS modes against a server with a certificate" =
  let certs = Filename_unix.temp_dir "pg-async-certs" "" in
  let%bind () = make_certs certs in
  let setup_data_dir data =
    Deferred.List.iter ~how:`Sequential [ "server.crt"; "server.key" ] ~f:(fun f ->
      let%bind contents = Reader.file_contents (certs ^/ f) in
      let%bind () = Writer.save (data ^/ f) ~contents in
      Unix.chmod (data ^/ f) ~perm:0o600)
  in
  Pg_test.with_temp_cluster
    ~server_options:[ "ssl", "on" ]
    ~setup_data_dir
    (fun cluster ->
      let base = Pg_test.config cluster in
      let with_tls ?root ?(host = "127.0.0.1") ssl_mode =
        { base with
          host = Some host
        ; ssl_mode
        ; ssl_root_cert = Option.map root ~f:(fun f -> certs ^/ f)
        }
      in
      let cases =
        [ "verify-full, right CA", with_tls ~root:"ca.crt" Verify_full
        ; "verify-full, other CA", with_tls ~root:"other.crt" Verify_full
        ; ( "verify-full, wrong host name"
          , with_tls ~root:"ca.crt" ~host:"localhost" Verify_full )
        ; ( "verify-ca, wrong host name"
          , with_tls ~root:"ca.crt" ~host:"localhost" Verify_ca )
        ; "require, no CA", with_tls Require
        ; "disable", with_tls Disable
        ]
      in
      let%bind () =
        Deferred.List.iter ~how:`Sequential cases ~f:(fun (name, config) ->
          let%map outcome = try_connect config in
          printf "%-30s %s\n" name outcome)
      in
      [%expect
        {|
      verify-full, right CA          connected, ssl=true
      verify-full, other CA          refused: SSL error: certificate verify failed
      verify-full, wrong host name   refused: server certificate for "127.0.0.1" (and 1 other name) does not match host name "localhost"
      verify-ca, wrong host name     connected, ssl=true
      require, no CA                 connected, ssl=true
      disable                        connected, ssl=false
      |}];
      let%map (_ : string) = Process.run_exn ~prog:"rm" ~args:[ "-rf"; certs ] () in
      ())
;;
