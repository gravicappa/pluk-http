let listen host port dispatch = Tcp.listen host port dispatch

let with_server host port dispatch proc =
  Lwt.pick [proc (); listen host port dispatch]
