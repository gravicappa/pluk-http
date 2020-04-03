(* Pluk-http
  Copyright 2018 Ramil Farkhshatov
  All rights reserved. This file is distributed under the terms of the
  GNU Lesser General Public License version 3 with OCaml linking exception *)

(** Module [pluk-http]: [static] definitions *)

val content_types: (string, Base.Content_type.t) Hashtbl.t
(** A table containing extension->Content_type table used by [file_content]
    to determine file content type by its extension. *)

val content_type_of_filename: string -> Base.Content_type.t
(** Uses [content_types] table to determine file content type by its
    extension. *)

val add_content_header: Response.t -> Base.Content_type.t -> float -> unit
(** Adds `Content-Type`, `Date`, `Last-Modified`, `Cache-Control` fields
    to response header *)

val string_content:
  ?content_type:Base.Content_type.t -> 
  ?header:(string * string) list ->
  string ->
  Dispatch.t
(** [string_content ~content_type content] creates dispatch function
    that serves [content] with given [content_type]. Default value of
    [content_type] is [application/octet-stream]. *)

val file_content:
  ?content_type:Base.Content_type.t ->
  ?header:(string * string) list ->
  ?buf_size:int -> Lwt_io.file_name -> Dispatch.t
(** Creates dispatch function that serves given file. If [content_type]
    is not given it is calculated via [content_type_of_filename]. *)

val normalize_path_within: string list -> string list option
(** Normalizing path resolving ["."] and [".."] entries. Returns [None]
    if it is exhausted by excess [".."]. *)

val dir_content:
  ?buf_size:int ->
  ?dir_proc:(Lwt_io.file_name -> Dispatch.t) ->
  ?header:(string * string) list ->
  Lwt_io.file_name -> Dispatch.t
(** Creates dispatch function that serves contents of given directory: files
    and subdirectories. For files [file_content] is used, for directories
    — [dir_proc]. If [dir_proc] is not specified then accessing subdirectories 
    (but not their files) returns [Response.(fail_with_code Not_found)]. *)
