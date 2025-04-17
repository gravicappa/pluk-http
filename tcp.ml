let safe_close fd =
  try%lwt Lwt_unix.close fd with _ -> Lwt.return_unit

let safe_close_chan chan =
  try%lwt Lwt_io.close chan with _ -> Lwt.return_unit

let listen ?(backlog = 16) host port proc =
  let open_tcp_server ?(backlog = 16) host port =
    let fd = Lwt_unix.(socket ~cloexec: true PF_INET SOCK_STREAM 0) in
    Lwt_unix.(setsockopt fd TCP_NODELAY true);
    Lwt_unix.(setsockopt fd SO_REUSEADDR true);
    let address = Unix.ADDR_INET (Unix.inet_addr_of_string host, port) in
    let%lwt () = Lwt_unix.bind fd address in
    Lwt_unix.listen fd backlog;
    Lwt.return fd in

  let process (fd, address) =
    try%lwt
      Lwt_unix.(setsockopt fd TCP_NODELAY true);
      let input = Lwt_io.(of_fd ~mode: Input fd) in
      let output = Lwt_io.(of_fd ~mode: Output fd) in
      Lwt.finalize
        (fun () -> proc address (input, output))
        (fun () ->
          let%lwt () = safe_close_chan input in
          safe_close_chan output)
    with Unix.Unix_error _ -> Lwt.return_unit in

  let rec accept_loop s =
    let%lwt client = Lwt_unix.accept ~cloexec: true s in
    Lwt.async (fun () -> process client);
    accept_loop s in

  try%lwt
    let%lwt s = open_tcp_server ~backlog host port in
    Lwt.finalize (fun () -> accept_loop s) (fun () -> safe_close s)
  with
  | Lwt.Canceled
  | Lwt_io.Channel_closed _
  | Unix.Unix_error _ -> Lwt.return_unit
