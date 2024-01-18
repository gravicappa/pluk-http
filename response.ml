(* Pluk-http
  Copyright 2018 Ramil Farkhshatov
  All rights reserved. This file is distributed under the terms of the
  GNU Lesser General Public License version 3 with OCaml linking exception *)

open Base

type body =
  | String of string
  | Bytes of bytes
  | Proc of int option * (Lwt_io.output_channel -> unit Lwt.t) 

type t = {
  code : int;
  status: string;
  mutable content_type: Content_type.t option;
  content_disposition: Content_disposition.t option;
  mutable cookies: Cookie.t list;
  mutable header: (string * string) list;
  body: body;
  mutable header_printed: bool;
  settings: Settings.t;
}

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

let create ?(code = 200) ?status ?(body = (String "")) ?(header = []) ?(cookies = [])
           ?content_type ?content_disposition ?(settings = Settings.create ())
           () =

  let status = match status with
               | Some s -> s
               | None ->
                   List.assoc_opt code response_codes
                   |> Option.value ~default: "" in

  { code; status; body; content_type; content_disposition; settings;
    cookies; header; header_printed = false }

let respond_header response output =
  let buf = Buffer.create 256 in

  let pr_response buf code status =
    let line = Printf.sprintf "HTTP/1.1 %d %s\r\n" code status in
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

  pr_response buf response.code response.status;
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
