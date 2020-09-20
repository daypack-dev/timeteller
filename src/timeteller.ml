let listen_sockaddr =
  Unix.(ADDR_INET ((inet_addr_of_string "0.0.0.0"), 9999))

let respond (client_sock_addr : Unix.sockaddr)
    ((ic, oc) : Lwt_io.input_channel * Lwt_io.output_channel) : unit Lwt.t =
  Lwt.return ()

let server : Lwt_io.server Lwt.t =
  Lwt_io.establish_server_with_client_address
    listen_sockaddr
    respond

let main =
  let%lwt () = Lwt_io.printf "Starting server\n" in
  let%lwt server = server in
  let%lwt () = Lwt_io.printf "Shutting down server\n" in
  Lwt_io.shutdown_server server

let () =
  Lwt_main.run main
