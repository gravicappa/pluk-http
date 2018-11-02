(* Pluk-http
  Copyright 2018 Ramil Farkhshatov
  All rights reserved. This file is distributed under the terms of the
  GNU Lesser General Public License version 3 with OCaml linking exception *)

(** Module [pluk-http]: request definitions *)

exception Wrong_method of string
(** Wrong method exception *)

(** Request header *)
module Header:
sig
  type t = {
    http_method: Base.http_method;
    target: Base.Uri.t;
    http_version: string;
    content_length: int option;
    content_type: Base.Content_type.t option;
    content_disposition: Base.Content_disposition.t option;
    cookies: (string, string) Hashtbl.t;
    fields: (string, string) Hashtbl.t;
  }
end

(** Request data *)
module Data:
sig
  type t = {
    parameters: (string, Base.Parameter.t) Hashtbl.t;
    body: string option;
  }
end

type t = {
  header: Header.t;
  data: Data.t Lwt.t Lazy.t;
  input: Lwt_io.input_channel;
  settings: Base.Settings.t;
}
(** Request type *)

val header: t -> Header.t
(** Returns header of a request *)

val data: t -> Data.t Lwt.t
(** Returns data of a request *)

val settings: t -> Base.Settings.t
(** Returns settings of a request *)

val change_settings: t -> (Base.Settings.t -> Base.Settings.t) -> t
(** [change_settings request proc] applies [proc] to request settings
    and returns request with settings modified to [proc] result *)

val http_method: t -> Base.http_method
(** Returns HTTP method of a request *)

val target: t -> Base.Uri.t
(** Returns target URI of a request *)

val content_type: t -> Base.Content_type.t option
(** Returns content type a request *)

val content_length: t -> int option
(** Returns content length a request *)

val field: t -> string -> string
(** Find header field by name. Raises [Not_found] if not found *)

val field_opt: t -> string -> string option
(** A version of [field] but returns option value *)

val cookie: t -> string -> string
(** Find cookie by name. Raises [Not_found] if not found *)

val cookie_opt: t -> string -> string option
(** A version of [cookie] but returns option value *)

val http_method_of_string: string -> Base.http_method option
(** Returns textual representation of a method *)

val parse_http_header: char Pluk_parser.stream ->
  (Header.t, char) Pluk_parser.parse_result
(** Parses http header *)

val clear_files: Data.t -> unit
(** Removes files stored from multipart request *)

val parse_body:
  Header.t ->
  Lwt_io.input_channel ->
  'a -> Base.Settings.t -> ('a -> unit Lwt.t) -> Data.t Lwt.t
(** Parses request body *)
