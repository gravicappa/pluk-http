(* Pluk-http
  Copyright 2018 Ramil Farkhshatov
  All rights reserved. This file is distributed under the terms of the
  GNU Lesser General Public License version 3 with OCaml linking exception *)

(** Module [pluk-http]: dispatch definitions *)

type t = string list -> Request.t -> Response.t Lwt.t;;
(** Dispatcher function type *)

val dispatch:
  Lwt_io.input_channel * Lwt_io.output_channel -> Base.Settings.t -> t ->
    unit Lwt.t
(** [dispatch input output settings proc] reads HTTP request and passes it
    into proc. Then sends back response that [proc] returns *)

(** {1 Request routing} *)

(** 

*)

val fail_with_code_num: int -> t
(** Creates dispatch function that produces empty response with given
    HTTP status code *)

val fail_with_code: Response.response_code -> t
(** Like [fail_with_code_num] but takes HTTP status code as
    [Response.response_code] *)

val either: (Request.t -> bool) -> ?fail:t -> t -> t
(** [either test ~fail proc] creates dispatch function that passes request to 
    [test] function and if that returns [true] calls [proc] or otherwise
    it calls [fail]. Default value of [fail] is
    [fail_with_code Response.Bad_request]. *)

val method_only: Base.http_method -> t -> t
(** [method_only http_method proc] calls [proc] only if request method is
    [http_method] else [fail_with_code Response.Method_not_allowed] is
    returned. *)

val select_method: ?fail:t -> (Base.http_method * t) list -> t
(** Uses for calling different dispatch functions depending on request
    methods. Default value of [fail] is
    [fail_with_code Response.Method_not_allowed]. *)

val path_element: ?fail:t -> (string -> t) -> t
(** Creates dispatch function that calls provided function with first element
    of target uri path ensuring that path is not empty otherwise calls [fail].
    Default value of [fail] is [fail_with_code Response.Not_found]. *)

val path_element_int: ?fail:t -> (int -> t) -> t
(** Same as [path_element] but converts first target uri path element to
    integer calling [fail] on failure. *)

val dir: ?fail:t -> ?root:t -> (string * t) list -> t
(** Creates dispatch function that dispatches over a list of functions using
    first element of target uri path.
    Default value of [fail] is [fail_with_code Response.Not_found].
    Default value of [root] is [fail_with_code Response.Not_found].
 *)

val with_settings: (Request.t -> Base.Settings.t) -> t -> t
(** *)
