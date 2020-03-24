(* Pluk-http
  Copyright 2018 Ramil Farkhshatov
  All rights reserved. This file is distributed under the terms of the
  GNU Lesser General Public License version 3 with OCaml linking exception *)

open Base

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
  | Ok -> 200
  | Not_found -> 404
  | Move_permanently -> 301
  | Found -> 302
  | Bad_request -> 400
  | Unauthorized -> 401
  | Forbidden -> 403
  | Method_not_allowed -> 405
  | Not_implemented -> 501
  | Internal_error -> 500 

let response_codes = [
  (100, "Continue");
  (200, "OK");
  (204, "No Response");
  (301, "Moved Permanently");
  (302, "Found");
  (304, "Not Modified");
  (400, "Bad Request");
  (401, "Unauthorized");
  (403, "Forbidden");
  (404, "Not Found");
  (405, "Method Not Allowed");
  (500, "Internal Server Error");
  (501, "Not Implemented");
  (520, "Unknown Error")
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
