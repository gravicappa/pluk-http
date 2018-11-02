(* Pluk-http
  Copyright 2018 Ramil Farkhshatov
  All rights reserved. This file is distributed under the terms of the
  GNU Lesser General Public License version 3 with OCaml linking exception *)

(** Module [pluk-http]: multipart definitions *)

val file_prefix: string
(** A prefix for files stored from multipart entries. Files are created by
    [Lwt_io.open_temp_file]. Directory is taken from
    [Settings.t.file_received_dir] *)

val parse:
  Lwt_io.input_channel ->
  int option ->
  string ->
  bytes ->
  string option ->
  (string -> bool) ->
  (string, Base.Parameter.t) Hashtbl.t ->
  (string, Base.Parameter.t) Hashtbl.t Lwt.t
(** [parse input content_length boundary buffer destination_dir tbl] parses
    multipart body of [content_length] size using preallocated [buffer]
    and modifies [tbl]. *)
