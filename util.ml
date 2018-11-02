let listen host port dispatch =
  let addr = Unix.(ADDR_INET (inet_addr_of_string host, port)) in
  Lwt_io.establish_server_with_client_address addr dispatch

let with_server host port dispatch proc =
  let%lwt server = listen host port dispatch in
  Lwt.finalize proc (fun () -> Lwt_io.shutdown_server server)
