open Lwt
open Cohttp
open Cohttp_lwt_unix

let listen_sockaddr = Unix.(ADDR_INET (inet_addr_any, 9999))

let look_up_time_zone_offset_s_via_ip_api_dot_com : Unix.sockaddr -> (int, string) Result.t Lwt.t =
  let cache = Tz_offset_cache.create ~random:true 1000 in
  fun sock_addr ->
  match sock_addr with
  | Unix.ADDR_UNIX _ -> Lwt.return (Error "Address is a unix address")
  | Unix.ADDR_INET (addr, _port) ->
    let addr_str = Unix.string_of_inet_addr addr in
    match Tz_offset_cache.find addr_str cache with
    | Some offset -> Lwt.return (Ok offset)
    | None ->
      let uri = Uri.of_string ("http://ip-api.com/json/" ^ addr_str ^ "?fields=offset") in
      let%lwt resp, body = Client.get uri in
      let%lwt body_str = Cohttp_lwt.Body.to_string body in
      let json =
        try
          Some (Yojson.Safe.from_string body_str)
        with
        | Yojson.Safe.Finally _ -> None
      in
      match json with
      | None -> Lwt.return (Error "Failed to parse JSON response from ip-api.com")
      | Some json ->
        match json with
        | `Assoc l -> (
            match
              List.filter_map (fun (k, v) ->
                  if k = "offset" then
                    (match v with
                     | `Int offset ->
                       Tz_offset_cache.add addr_str offset cache;
                       Some offset
                     | _ -> None
                    )
                  else
                    None
                ) l with
            | [offset] -> Lwt.return (Ok offset)
            | _ ->
              Lwt.return (Error "Failed to interpret JSON response from ip-api.com")
          )
        | _ ->
          Lwt.return (Error "Failed to interpret JSON response from ip-api.com")

let respond (client_sock_addr : Unix.sockaddr)
    ((ic, oc) : Lwt_io.input_channel * Lwt_io.output_channel) : unit Lwt.t =
  let%lwt input = Lwt_io.read ~count:Config.max_input_char_count ic in
  let%lwt () = Lwt_io.printf "Input: |%s|\n" input in
  let%lwt offset_res = resolve_time_zone_offset_s_via_ip_api_dot_com client_sock_addr in
  match offset_res with
  | Error msg -> Lwt_io.write oc (Printf.sprintf "Error during time zone offset lookup: %s" msg)
  | Ok offset ->
    match Daypack_lib.Search_param.make_using_years_ahead
            ~search_using_tz_offset_s:offset 100
    with
    | Error _ -> Lwt_io.write_line oc "Error during search param construction"
    | Ok search_param ->
      let%lwt () = Lwt_io.write oc "abcd" in
      let%lwt () = Lwt_io.flush oc in
      Lwt.return ()

let server : Lwt_io.server Lwt.t =
  Lwt_io.establish_server_with_client_address listen_sockaddr respond

let loop_until_keyword (ic : Lwt_io.input_channel) : unit Lwt.t =
  let rec aux ic =
    let%lwt res = Lwt_io.read_line_opt ic in
    match res with
    | None -> Lwt.return_unit
    | Some s -> ( match s with "exit" -> Lwt.return_unit | _ -> aux ic )
  in
  aux ic

let run () =
  let%lwt () = Lwt_io.printf "Starting server\n" in
  let%lwt server = server in
  let%lwt () = loop_until_keyword Lwt_io.stdin in
  Lwt_io.shutdown_server server

let () = Lwt_main.run (run ())
