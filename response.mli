(* Pluk-http
  Copyright 2018 Ramil Farkhshatov
  All rights reserved. This file is distributed under the terms of the
  GNU Lesser General Public License version 3 with OCaml linking exception *)

(** Module [pluk-http]: response definitions *)

type response_code =
  | Ok
  | Not_found
  | Move_permanently
  | Found
  | Bad_request
  | Unauthorized
  | Forbidden
  | Method_not_allowed 
  | Not_implemented
  | Internal_error
(** Response codes *)

type body =
  | String of string
  | Bytes of bytes
  | Proc of int option * (Lwt_io.output_channel -> unit Lwt.t)
(** Response body
    First element of Proc tuple is content length if available *)

type t = {
  mutable code: int;
  mutable content_type: Base.Content_type.t option;
  mutable content_disposition: Base.Content_disposition.t option;
  mutable cookies: Base.Cookie.t list;
  mutable header: (string * string) list;
  mutable body: body;
  mutable header_printed: bool;
  mutable settings: Base.Settings.t;
}
(** Response type *)

val create:
  ?code:int ->
  ?body:body ->
  ?header:(string * string) list ->
  ?cookies:Base.Cookie.t list ->
  ?content_type:Base.Content_type.t ->
  ?content_disposition:Base.Content_disposition.t ->
  ?settings:Base.Settings.t -> unit -> t
(** Creates [Response.t] instance *)

val int_of_code: response_code -> int
(** Returns numeric value of [response_code] HTTP status code *)

val respond_header: t -> Lwt_io.output_channel -> unit Lwt.t
(** Writes response header to output channel *)

val respond: t -> Lwt_io.output_channel -> unit Lwt.t
(** Writes whole response including header to output channel *)

val add_header: t -> string -> string -> unit
(** Adds header to response *)

val add_cookie: t -> Base.Cookie.t -> unit
(** Adds cookie to response *)
