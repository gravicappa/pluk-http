(* Pluk-http
  Copyright 2018 Ramil Farkhshatov
  All rights reserved. This file is distributed under the terms of the
  GNU Lesser General Public License version 3 with OCaml linking exception *)

open Base
module P = Pluk_parser

exception Wrong_method of string

module Header = struct
  type t = {
    http_method: http_method;
    target: Uri.t;
    http_version: string;
    content_length: int option;
    content_type: Content_type.t option;
    content_disposition: Content_disposition.t option;
    cookies: (string, string) Hashtbl.t;
    fields: (string, string) Hashtbl.t;
  }
end

module Data = struct
  type t = {
    parameters: (string, Parameter.t) Hashtbl.t;
    body: string option;
  }
end

type t = {
  header: Header.t;
  data: Data.t Lwt.t Lazy.t;
  input: Lwt_io.input_channel;
  settings: Settings.t;
}

let header req = req.header
let data req = Lazy.force req.data
let settings req = req.settings

let change_settings proc req = {req with settings = (proc req.settings)}

let http_method req = req.header.http_method

let target req = req.header.target

let content_type req = req.header.content_type

let content_length req = req.header.content_length

let field key req =
  Hashtbl.find req.header.fields (String.lowercase_ascii key)

let field_opt key req =
  Hashtbl.find_opt req.header.fields (String.lowercase_ascii key)

let cookie key req = Hashtbl.find req.header.cookies key

let cookie_opt key req = Hashtbl.find_opt req.header.cookies key

let parse_http_header_contents http_method target http_version =
  P.next Parse.http_header_fields (fun cell s ->
    Ok ({Header.http_method; target; http_version;
               fields = cell.generic; cookies = cell.cookies;
               content_length = cell.content_length;
               content_type = cell.content_type;
               content_disposition = cell.content_disposition},
              s))

let http_method_of_string = function
  | "GET" -> Some GET
  | "HEAD" -> Some HEAD
  | "POST" -> Some POST
  | "PUT" -> Some PUT
  | "DELETE" -> Some DELETE
  | "CONNECT" -> Some CONNECT
  | "OPTIONS" -> Some OPTIONS
  | "TRACE" -> Some TRACE
  | "PATCH" -> Some PATCH
  | _ -> None

let parse_http_header =
  let parse_method = P.string_if (fun c -> (c >= 'A') && (c <= 'Z')) in
  P.(next parse_method (fun m ->
       match (http_method_of_string m) with
       | Some http_method ->
         next Parse.space (fun _ ->
           next Parse.uri (fun target ->
             next Parse.space (fun _ -> 
               next Parse.word (fun version ->
                 next Parse.crlf (fun _ ->
                   parse_http_header_contents http_method target version)))))
       | None -> error "Wrong HTTP method"))

let clear_files {Data.parameters; _} =
  let proc _ = function
    | {Parameter.value = File {path; _}; _} ->
        (try Sys.remove path with | Sys_error _ -> ())
    | _ -> () in
  Hashtbl.iter proc parameters

let parse_body (header: Header.t) input output (settings: Settings.t)
               continue =
  let param_of_query (k, v) =
    let content_type = Content_type.create "text/plain" () in
    {Parameter.name = k; content_type; value = String v} in

  let parameters_of_query tbl query = 
    let proc x =
      match param_of_query x with
      | {Parameter.name = name; _} as p -> Hashtbl.add tbl name p in
    let () = List.iter proc query in
    tbl in

  let get_parameters (header: Header.t) =
    let tbl = Hashtbl.create (List.length header.target.query) in
    parameters_of_query tbl header.target.query in

  let maybe_continue (header: Header.t) =
    match Hashtbl.find_opt header.fields "expect" with
    | Some "100-continue" -> continue output
    | _ -> Lwt.return () in

  let read_post_data header input =
    match header with
    | {Header.content_length = Some length; _} ->
        let bytes = Bytes.create length in
        let%lwt () = Lwt_io.read_into_exactly input bytes 0 length in
        Lwt.return (Some (Bytes.to_string bytes))
    | _ -> Lwt.return None in

  let url_parameters str parameters_tbl =
    match Parse.query (P.Stream.of_string str) with
    | Ok (q, _) ->
        let parameters = parameters_of_query parameters_tbl q in
        Lwt.return {Data.parameters = parameters; body = None}
    | Error (_, _) -> Lwt.fail (Parse.Parse_error "Broken URL parameters") in

  let post_url_parameters header input parameters =
    match%lwt read_post_data header input with
    | Some str -> url_parameters str parameters
    | None -> Lwt.return {Data.parameters = Hashtbl.create 0; body = None} in

  let post_parameters header input =
    let parameters = (get_parameters header) in
    let%lwt () = maybe_continue header in
    match header.content_type with
    | Some {Content_type.media_type = "application/x-www-form-urlencoded"; _} ->
        post_url_parameters header input parameters
    | Some {Content_type.media_type = "multipart/form-data";
            boundary = Some boundary; _} ->
        let bytes = Bytes.create settings.multipart_buf_size in
        let%lwt parameters = Multipart.parse input header.content_length
                                             boundary bytes
                                             settings.file_received_dir
                                             settings.file_test
                                             parameters in
        Lwt.return {Data.parameters; body = None}
    | Some {Content_type.media_type = "multipart/form-data"; _} ->
        Lwt.fail (Parse.Parse_error "Broken POST request")
    | _ ->
        let%lwt body = read_post_data header input in
        Lwt.return {Data.parameters; body} in

  match header.http_method with
  | POST | PATCH | PUT -> post_parameters header input
  | GET | HEAD | DELETE ->
      Lwt.return {Data.parameters = get_parameters header; body = None}
  | _ -> Lwt.fail (Wrong_method "")

