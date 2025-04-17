(* Pluk-http
  Copyright 2018 Ramil Farkhshatov
  All rights reserved. This file is distributed under the terms of the
  GNU Lesser General Public License version 3 with OCaml linking exception *)

(** Module [pluk-http]: common definitions *)

type http_method =
  | GET
  | HEAD
  | POST
  | PUT
  | DELETE
  | CONNECT
  | OPTIONS
  | TRACE
  | PATCH
(** HTTP methods *)

(** Uri parsing *)
module Uri:
sig
  type t = {
    protocol: string;
    user: string;
    password: string;
    host: string;
    port: int option;
    path: string list;
    query: (string * string) list;
    fragment: string;
  }
  (** Uri type *)
end

(** Content-type *)
module Content_type:
sig
  type t = {
    media_type: string;
    charset: string option;
    boundary: string option;
  }
  (** type *)

  val create: ?charset:string -> ?boundary:string -> string -> t
  (** Creates instance of Content_type.t *)

  val into_buffer: t -> Buffer.t -> unit
  (** Writes textual representation of a content-type into a [Buffer] *)

  val to_string: t -> string
  (** Returns a textual representation of a content-type *)
end

(** Content-disposition *)
module Content_disposition:
sig
  type mode = Inline | Attachment | Form_data
  type t = { mode: mode; name: string option; filename: string option; }
  (** type *)
end

val esc_into_buffer: string -> Buffer.t -> unit
(** Writes percent-escaped string into a [Buffer] *)

val buffer_of_escaped: string -> Buffer.t
(** Returns a [Buffer] containing percent-escaped string *)

val esc: string -> string
(** Returns percent-escaped string *)

(** RFC 1123 time format *)
module Time:
sig
  val to_string: float -> string
  (** Returns RFC1123 encoded time *)

  val parse: char Pluk_parser.stream -> (float, char) Pluk_parser.parse_result
  (** Parser of RFC1123 encoded time *)
end

(** Cookie *)
module Cookie:
sig
  type t = {
    name: string;
    value: string;
    expires: float option;
    max_age: int option;
    domain: string option;
    path: string option;
  }
  val into_buffer: t -> Buffer.t -> unit
  (** Writes Set-cookie field into a [Buffer] *)
  
  val to_string: t -> string
  (** Returns a Set-cookie field *)
end

(** HTTP request parameter

Represents
  - query parameters
  - POST uri-encoded parameters
  - POST multipart items
*)
module Parameter:
sig
  type file = {
    filename: string; (** Reported filename of a parameter*)
    path: string; (** Path to a local file containing file contents *)
  }
  (** Type of multipart item representing a file *)

  type value = String of string | File of file
  (** [Parameter.t] data value *)

  type t = { name: string; content_type: Content_type.t; value: value; }
  (** Parameter type *)

  val str_debug: t -> string
  (** Returns textual repesentation of a [Parameter.t] with constructors *)
end

(** Server settings *)
module Settings:
sig
  type t = {
    multipart_buf_size: int; (** Size of a block for multipart reading *)
    keep_alive_timeout_s: float option; (** Timeout for keep alive *)
    timeout_s: float; (** Timeout for reading request *)
    file_received_dir: string option; (** Directory for temporary storage of 
                                          received files *)
    file_test: string -> bool; (** Function for filtering files by name *)
  }
  (** type *)

  val create:
    ?multipart_buf_size:int ->
    ?keep_alive_timeout_s:float option ->
    ?timeout_s:float ->
    ?file_received_dir:string option ->
    ?file_test:(string -> bool) -> unit -> t
  (** Creates instance of [Settings.t] *)
end
