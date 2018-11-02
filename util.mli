val listen:
  string ->
  int ->
  (Lwt_unix.sockaddr ->
   Lwt_io.input_channel * Lwt_io.output_channel ->
   unit Lwt.t) ->
  Lwt_io.server Lwt.t

val with_server:
  string ->
  int ->
  (Lwt_unix.sockaddr ->
   Lwt_io.input_channel * Lwt_io.output_channel ->
   unit Lwt.t) ->
  (unit -> 'a Lwt.t) ->
  'a Lwt.t
