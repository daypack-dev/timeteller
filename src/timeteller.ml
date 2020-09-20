open Lwt.Infix

let listen_sockaddr =
  Unix.(ADDR_INET (inet_addr_any, 9999))

let respond (client_sock_addr : Unix.sockaddr)
    ((ic, oc) : Lwt_io.input_channel * Lwt_io.output_channel) : unit Lwt.t =
  let%lwt input = Lwt_io.read ic in
  let%lwt () = Lwt_io.printf "Input:\n" in
  let%lwt () = Lwt_io.printf "%s\n" input in
  Lwt.return ()

let server : Lwt_io.server Lwt.t =
  Lwt_io.establish_server_with_client_address
    ~no_close:true
    listen_sockaddr
    respond

let run () =
  let%lwt () = Lwt_io.printf "Starting server\n" in
  let%lwt _server = server in
  fst (Lwt.wait ())

let () =
  Lwt_main.run (run ())
