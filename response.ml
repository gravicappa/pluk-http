(* Pluk-http
  Copyright 2018 Ramil Farkhshatov
  All rights reserved. This file is distributed under the terms of the
  GNU Lesser General Public License version 3 with OCaml linking exception *)

open Base

type response_code =
  | Continue
  | Switching_protocols
  | Early_hints
  | Ok
  | Created
  | Accepted
  | Non_authoritative_information
  | No_content
  | Reset_content
  | Partial_content
  | Multiple_choices
  | Moved_permanently
  | Found
  | See_other
  | Not_modified
  | Temporary_redirect
  | Permanent_redirect
  | Bad_request
  | Unauthorized
  | Payment_required
  | Forbidden
  | Not_found
  | Method_not_allowed
  | Not_acceptable
  | Proxy_authentication_required
  | Request_timeout
  | Conflict
  | Gone
  | Length_required
  | Precondition_failed
  | Payload_too_large
  | Uri_too_long
  | Unsupported_media_type
  | Range_not_satisfiable
  | Expectation_failed
  | Im_a_teapot
  | Unprocessable_entity
  | Too_early
  | Upgrade_required
  | Precondition_required
  | Too_many_requests
  | Request_header_fields_too_large
  | Unavailable_for_legal_reasons
  | Internal_server_error
  | Not_implemented
  | Bad_gateway
  | Service_unavailable
  | Gateway_timeout
  | Http_version_not_supported
  | Variant_also_negotiates
  | Insufficient_storage
  | Loop_detected
  | Not_extended
  | Network_authentication_required

type body =
  | String of string
  | Bytes of bytes
  | Proc of int option * (Lwt_io.output_channel -> unit Lwt.t) 

type t = {
  mutable code : int;
  mutable content_type: Content_type.t option;
  mutable content_disposition: Content_disposition.t option;
  mutable cookies: Cookie.t list;
  mutable header: (string * string) list;
  mutable body: body;
  mutable header_printed: bool;
  mutable settings: Settings.t;
}

let create ?(code = 200) ?(body = (String "")) ?(header = []) ?(cookies = [])
           ?content_type ?content_disposition ?(settings = Settings.create ())
           () =
  {code; body; content_type; content_disposition; settings;
   cookies; header; header_printed = false}


let int_of_code = function
  | Continue -> 100
  | Switching_protocols -> 101
  | Early_hints -> 103
  | Ok -> 200
  | Created -> 201
  | Accepted -> 202
  | Non_authoritative_information -> 203
  | No_content -> 204
  | Reset_content -> 205
  | Partial_content -> 206
  | Multiple_choices -> 300
  | Moved_permanently -> 301
  | Found -> 302
  | See_other -> 303
  | Not_modified -> 304
  | Temporary_redirect -> 307
  | Permanent_redirect -> 308
  | Bad_request -> 400
  | Unauthorized -> 401
  | Payment_required -> 402
  | Forbidden -> 403
  | Not_found -> 404
  | Method_not_allowed -> 405
  | Not_acceptable -> 406
  | Proxy_authentication_required -> 407
  | Request_timeout -> 408
  | Conflict -> 409
  | Gone -> 410
  | Length_required -> 411
  | Precondition_failed -> 412
  | Payload_too_large -> 413
  | Uri_too_long -> 414
  | Unsupported_media_type -> 415
  | Range_not_satisfiable -> 416
  | Expectation_failed -> 417
  | Im_a_teapot -> 418
  | Unprocessable_entity -> 422
  | Too_early -> 425
  | Upgrade_required -> 426
  | Precondition_required -> 428
  | Too_many_requests -> 429
  | Request_header_fields_too_large -> 431
  | Unavailable_for_legal_reasons -> 451
  | Internal_server_error -> 500
  | Not_implemented -> 501
  | Bad_gateway -> 502
  | Service_unavailable -> 503
  | Gateway_timeout -> 504
  | Http_version_not_supported -> 505
  | Variant_also_negotiates -> 506
  | Insufficient_storage -> 507
  | Loop_detected -> 508
  | Not_extended -> 510
  | Network_authentication_required -> 511

let response_codes = [
  (100, "Continue");
  (101, "Switching Protocols");
  (103, "Early Hints");
  (200, "OK");
  (201, "Created");
  (202, "Accepted");
  (203, "Non-Authoritative Information");
  (204, "No Content");
  (205, "Reset Content");
  (206, "Partial Content");
  (300, "Multiple Choices");
  (301, "Moved Permanently");
  (302, "Found");
  (303, "See Other");
  (304, "Not Modified");
  (307, "Temporary Redirect");
  (308, "Permanent Redirect");
  (400, "Bad Request");
  (401, "Unauthorized");
  (402, "Payment Required");
  (403, "Forbidden");
  (404, "Not Found");
  (405, "Method Not Allowed");
  (406, "Not Acceptable");
  (407, "Proxy Authentication Required");
  (408, "Request Timeout");
  (409, "Conflict");
  (410, "Gone");
  (411, "Length Required");
  (412, "Precondition Failed");
  (413, "Payload Too Large");
  (414, "URI Too Long");
  (415, "Unsupported Media Type");
  (416, "Range Not Satisfiable");
  (417, "Expectation Failed");
  (418, "I'm a teapot");
  (422, "Unprocessable Entity");
  (425, "Too Early");
  (426, "Upgrade Required");
  (428, "Precondition Required");
  (429, "Too Many Requests");
  (431, "Request Header Fields Too Large");
  (451, "Unavailable For Legal Reasons");
  (500, "Internal Server Error");
  (501, "Not Implemented");
  (502, "Bad Gateway");
  (503, "Service Unavailable");
  (504, "Gateway Timeout");
  (505, "HTTP Version Not Supported");
  (506, "Variant Also Negotiates");
  (507, "Insufficient Storage");
  (508, "Loop Detected");
  (510, "Not Extended");
  (511, "Network Authentication Required");
]

let respond_header response output =
  let buf = Buffer.create 256 in

  let resp_code_str code = List.assq code response_codes in

  let pr_response buf code =
    let code_str = resp_code_str code in
    let line = Printf.sprintf "HTTP/1.1 %d %s\r\n" code code_str in
    Buffer.add_string buf line in

  let add_hdr buf (k, v) =
    Buffer.add_string buf k;
    Buffer.add_string buf ": ";
    Buffer.add_string buf v;
    Buffer.add_string buf "\r\n" in

  let add_content_length_int buf x =
    add_hdr buf ("Content-Length", string_of_int x) in

  let add_content_length buf = function
    | String str -> String.length str |> add_content_length_int buf
    | Bytes bytes -> Bytes.length bytes |> add_content_length_int buf
    | Proc (Some len, _) -> add_content_length_int buf len
    | _ -> () in

  let add_content_type buf = function
    | Some content_type ->
        Content_type.into_buffer content_type buf;
        Buffer.add_string buf "\r\n"
    | None -> () in

  let add_cookie buf cookie =
    Cookie.into_buffer cookie buf;
    Buffer.add_string buf "\r\n" in

  pr_response buf response.code;
  List.iter (add_hdr buf) response.header;
  add_content_type buf response.content_type;
  add_content_length buf response.body;
  List.iter (add_cookie buf) response.cookies;
  Buffer.add_string buf "\r\n";
  response.header_printed <- true;
  let bytes = (Buffer.to_bytes buf) in
  Lwt_io.write_from_exactly output bytes 0 (Buffer.length buf)

let respond response output =
  let%lwt () = respond_header response output in
  match response.body with
  | String str ->
      let%lwt () = Lwt_io.write output str in
      Lwt_io.flush output
  | Bytes b ->
      let%lwt () = Lwt_io.write_from_exactly output b 0 (Bytes.length b) in
      Lwt_io.flush output
  | Proc (_, proc) ->
      let%lwt () = proc output in
      Lwt_io.flush output

let add_header response key value =
  response.header <- (key, value) :: response.header

let add_cookie response cookie =
  response.cookies <- cookie :: response.cookies
