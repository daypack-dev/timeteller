let listen_sockaddr = Unix.(ADDR_INET (inet_addr_any, 9999))

let respond (client_sock_addr : Unix.sockaddr)
    ((ic, oc) : Lwt_io.input_channel * Lwt_io.output_channel) : unit Lwt.t =
  let%lwt input = Lwt_io.read ic in
  let%lwt () = Lwt_io.printf "Input:\n" in
  let%lwt () = Lwt_io.printf "%s\n" input in
  let%lwt () = Lwt_io.write_line oc "abcd" in
  Lwt.return ()

let server : Lwt_io.server Lwt.t =
  Lwt_io.establish_server_with_client_address ~no_close:true listen_sockaddr
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
