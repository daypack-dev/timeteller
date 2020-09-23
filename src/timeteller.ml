open Lwt
open Cohttp
open Cohttp_lwt_unix

let listen_sockaddr = Unix.(ADDR_INET (inet_addr_any, 9999))

let resolve_time_zone_offset_s_via_ip_api_dot_com (sock_addr : Unix.sockaddr) : int option Lwt.t =
  match sock_addr with
  | Unix.ADDR_UNIX _ -> Lwt.return None
  | Unix.ADDR_INET (addr, _port) ->
    let addr_str = Unix.string_of_inet_addr addr in
    let uri = Uri.of_string ("http://ip-api.com/json/" ^ addr_str) in
    let%lwt (resp, body) = Client.get uri in
    let%lwt body_str = Cohttp_lwt.Body.to_string body in
    let%lwt () = Lwt_io.print body_str in
    Lwt.return None

let respond (client_sock_addr : Unix.sockaddr)
    ((ic, oc) : Lwt_io.input_channel * Lwt_io.output_channel) : unit Lwt.t =
  let%lwt input = Lwt_io.read ic in
  let%lwt _ = resolve_time_zone_offset_s_via_ip_api_dot_com client_sock_addr in
  let%lwt () = Lwt_io.printf "Input:\n" in
  let%lwt () = Lwt_io.printf "|%s|\n" input in
  let%lwt () = Lwt_io.write oc "abcd" in
  let%lwt () = Lwt_io.flush oc in
  Lwt.return ()

let server : Lwt_io.server Lwt.t =
  Lwt_io.establish_server_with_client_address listen_sockaddr
    respond

let loop_until_keyword (ic : Lwt_io.input_channel) : unit Lwt.t =
  let rec aux ic =
    let%lwt res = Lwt_io.read_line_opt ic in
    match res with
    | None -> Lwt.return_unit
    | Some s ->
      match s with
      | "exit" -> Lwt.return_unit
      | _ -> aux ic
  in
  aux ic

let run () =
  let%lwt () = Lwt_io.printf "Starting server\n" in
  let%lwt server = server in
  let%lwt () =
    loop_until_keyword Lwt_io.stdin
  in
  Lwt_io.shutdown_server server

let () = Lwt_main.run (run ())
