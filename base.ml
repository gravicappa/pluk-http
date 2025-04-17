(* Pluk-http
  Copyright 2018 Ramil Farkhshatov
  All rights reserved. This file is distributed under the terms of the
  GNU Lesser General Public License version 3 with OCaml linking exception *)

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

module Uri = struct 
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
end

module Content_type = struct
  type t = {
    media_type: string;
    charset: string option;
    boundary: string option;
  }

  let create ?charset ?boundary media_type = { media_type; charset; boundary }

  let into_buffer {media_type; charset; boundary} buf =
    let add_variant buf key = function
      | Some value ->
          Buffer.add_string buf "; ";
          Buffer.add_string buf key;
          Buffer.add_string buf value
      | None -> () in
    Buffer.add_string buf "Content-Type: ";
    Buffer.add_string buf media_type;
    add_variant buf "charset=" charset;
    add_variant buf "boundary=" boundary

  let to_string content_type =
    let buf = Buffer.create 16 in
    let () = into_buffer content_type buf in
    Buffer.contents buf
end

module Content_disposition = struct
  type mode = Inline | Attachment | Form_data

  type t = {
    mode: mode;
    name: string option;
    filename: string option;
  }
end

let esc_into_buffer str buf =
  let proc ch =
    match ch with
    | '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' | '-' | '_' | '.' | '~' ->
        Buffer.add_char buf ch
    | ch -> Buffer.add_string buf (Printf.sprintf "%%%02x" (Char.code ch))
  in
  String.iter proc str

let buffer_of_escaped str =
  let buf = Buffer.create 16 in
  esc_into_buffer str buf;
  buf

let esc str = buffer_of_escaped str |> Buffer.contents

module Time = struct
  module P = Pluk_parser

  let week_names = [| "Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat"; |]
  let month_names = [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul";
                       "Aug"; "Sep"; "Oct"; "Nov"; "Dec" |]

  let to_string sec =
    match Unix.gmtime sec with
    | { tm_sec; tm_min; tm_hour; tm_mday; tm_mon; tm_year; tm_wday; _ } ->
        let week = week_names.(tm_wday) in
        let month = month_names.(tm_mon) in
        let year = 1900 + tm_year in
        Printf.sprintf "%s, %d %s %04d %02d:%02d:%02d GMT" week tm_mday month
                       year tm_hour tm_min tm_sec

  let parse =
    let is_word c = ((c >= 'A') && (c <= 'Z')) || ((c >= 'a') && (c <= 'z')) in

    let array_index item arr =
      let n = Array.length arr in
      let rec loop i =
        if i < n then begin
          if arr.(i) = item then
            Some i
          else
            loop (i + 1)
        end else None in
      loop 0 in

    let p_ws = P.(satisfy fold ((>=) ' ')) in

    let p_word_enum arr =
      P.(next (into_string_if is_word) (fun word s ->
        match array_index word arr with
        | Some idx -> Ok (idx, s)
        | None -> Error (P.msg "p_word_enum", s))) in

    let p_colon = P.exact_item ':' in

    let p_time =
      P.(next (number 10) (fun hour ->
        next p_colon (fun _ ->
          next (number 10) (fun min ->
            next p_colon (fun _ ->
              next (number 10) (fun sec s ->
                Ok ((hour, min, sec), s))))))) in

    let p_timezone = P.exact_string "GMT" in

    let make_time tm_year tm_mon tm_mday tm_hour tm_min tm_sec =
      let s, _ = Unix.mktime {tm_year; tm_mon; tm_mday; tm_hour; tm_min;
                              tm_sec; tm_yday = 0; tm_wday = 0;
                              tm_isdst = false} in
      s in

    P.(next (p_word_enum week_names) @@ fun _ ->
       next (exact_item ',') @@ fun _ ->
       next p_ws @@ fun _ ->
       next (number 10) @@ fun day ->
       next p_ws @@ fun _ ->
       next (p_word_enum month_names) @@ fun month ->
       next p_ws @@ fun _ ->
       next (number 10) @@ fun year ->
       next p_ws @@ fun _ ->
       next p_time @@ fun (hour, min, sec) ->
       next p_ws @@ fun _ ->
       next p_timezone @@ fun _ s ->
         Ok (make_time year month day hour min sec, s))
end

module Cookie = struct
  type t = {
    name: string;
    value: string;
    expires: float option;
    max_age: int option;
    domain: string option;
    path: string option;
  }

  let into_buffer { name; value; expires; max_age; domain; path } buf =
    let add_float buf key = function
      | Some f ->
          Buffer.add_string buf key;
          Buffer.add_string buf (Time.to_string f)
      | None -> () in

    let add_int buf key = function
      | Some i ->
          Buffer.add_string buf key;
          Buffer.add_string buf (string_of_int i)
      | None -> () in

    let add_string buf key = function
      | Some s ->
          Buffer.add_string buf key;
          Buffer.add_string buf s
      | None -> () in

    Buffer.add_string buf "Set-Cookie: ";
    Buffer.add_string buf (esc name);
    Buffer.add_string buf "=";
    Buffer.add_string buf (esc value);
    add_float buf "Expires=" expires;
    add_int buf "Max-Age=" max_age;
    add_string buf "Domain=" domain;
    add_string buf "Path=" path

  let to_string cookie =
    let buf = Buffer.create 16 in
    into_buffer cookie buf;
    Buffer.contents buf
end

module Parameter = struct
  type file = {
    filename: string;
    path: string
  }

  type value =
    | String of string
    | File of file

  type t = {
    name: string;
    content_type: Content_type.t;
    value: value;
  }

  let str_debug = function
    | {name; content_type; value = String str} ->
        Printf.sprintf ("{Parameter.name = %s; content_type = %s;"
                        ^^ " value = String '%s'}")
                       name content_type.media_type str
    | {name; content_type; value = File {filename; path}} ->
        Printf.sprintf ("{Parameter.name = %s; content_type = %s;"
                        ^^ " value = File {filename = '%s'; path = '%s'}")
                       name content_type.media_type filename path
  
end

module Settings = struct
  type t = {
    multipart_buf_size: int;
    keep_alive_timeout_s: float option;
    timeout_s: float;
    file_received_dir: string option;
    file_test: string -> bool;
  }

  let create ?(multipart_buf_size = 8 * 1024 * 1024)
             ?(keep_alive_timeout_s = None)
             ?(timeout_s = 60.)
             ?(file_received_dir = None)
             ?(file_test = (fun _ -> true)) () =
    { multipart_buf_size; keep_alive_timeout_s; timeout_s; file_received_dir;
      file_test }
end
