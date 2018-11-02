(* Pluk-http
  Copyright 2018 Ramil Farkhshatov
  All rights reserved. This file is distributed under the terms of the
  GNU Lesser General Public License version 3 with OCaml linking exception *)

open Base;;
module P = Pluk_parser;;

type t = string list -> Request.t -> Response.t Lwt.t;;

let dispatch (input, output) settings proc =
  let read_header_buf input timeout_s next_timeout_s =
    let buf = Buffer.create 64 in
    let add_str buf line = Buffer.add_string buf (line ^ "\r\n") in

    let rec wait sec =
      let%lwt () = Lwt_unix.sleep sec in
      Lwt.return_none

    and line_or_timeout input = function
      | Some sec -> Lwt.pick [wait sec; Lwt_io.read_line_opt input]
      | None -> Lwt_io.read_line_opt input

    and loop timeout_s =
      match%lwt line_or_timeout input timeout_s with
        | Some "" -> Lwt.return_some (Buffer.to_bytes buf)
        | Some line ->
            add_str buf line;
            loop next_timeout_s
        | None -> Lwt.return_none in

    loop timeout_s in

  let continue output =
    Response.respond (Response.create ~code: 100 ()) output in

  let parse_data header =
    Request.parse_body header input output settings continue in

  let clear_files data =
    if Lazy.is_val data then 
      let%lwt body = (Lazy.force data) in
      Lwt.return (Request.clear_files body)
    else
      Lwt.return_unit in

  let maybe_body http_method response =
    match http_method with
    | HEAD -> 
        let%lwt () = Response.respond_header response output in
        Lwt_io.flush output
    | _ -> Response.respond response output in

  let do_request (header: Request.Header.t) =
    try%lwt 
      let target_path = header.target.path in
      let data = Lazy.from_fun (fun () -> parse_data header) in
      let request = {Request.header; data; input; settings} in
      let%lwt response = proc target_path request in
      let%lwt () = maybe_body request.header.http_method response in
      clear_files data
    with
    | Unix.Unix_error _ -> Lwt.return_unit
    | exn ->
        let resp_500 = (Response.create ~code: 500 ()) in
        let%lwt () = Response.respond resp_500  output in
        Lwt.fail exn in

  let rec maybe_continue () = 
    match settings with
    | {keep_alive_timeout_s = (Some sec); _} -> dispatch (Some sec)
    | _ -> Lwt.return_unit

  and do_request_and_continue header =
    let%lwt () = do_request header in
    let%lwt () = Lwt_io.flush output in
    maybe_continue ()

  and parse_header header =
    match (Request.parse_http_header (P.stream_of_bytes header 0)) with
    | Parse_ok (header, _) -> do_request_and_continue header
    | Parse_error (_, _) ->
        Response.respond (Response.create ~code: 400 ()) output

  and dispatch timeout_s =
    let settings_timeout_s = Some settings.timeout_s in
    match%lwt read_header_buf input timeout_s settings_timeout_s with
    | Some header -> parse_header header
    | None -> Lwt.return_unit in

  let%lwt () = dispatch None in
  let%lwt () = Lwt_io.close input in
  let%lwt () = Lwt_io.close output in
  Lwt.return_unit;;

let fail_with_code_num code _ _ =
  Lwt.return (Response.create ~code ());;

let fail_with_code code path req =
  fail_with_code_num Response.(int_of_code code) path req;;

let either test ?(fail = fail_with_code_num 400) proc path req =
  (if test req then proc else fail) path req;;

let method_only m proc path req =
  let test req = (Request.http_method req) = m in
  let fail = fail_with_code_num 405 in
  either test ~fail proc path req;;

let select_method ?(fail = fail_with_code_num 405) list =
  let rec mk finish = function
    | (m, proc) :: list ->
        let p = mk finish list in
        (fun path req ->
          if (Request.http_method req) = m then
            proc path req
          else
            p path req)
    | [] -> finish in
  mk fail list;;

let path_element ?(fail = fail_with_code_num 404) proc path req =
  match path with
  | [] -> fail [] req
  | element :: path -> proc element path req;;

let path_element_int ?(fail = fail_with_code_num 404) proc path req =
  let proc element path req =
    match int_of_string_opt element with
    | Some x -> proc x path req
    | None -> fail (element :: path) req in
  path_element proc ~fail path req;;

let dir ?(fail = fail_with_code_num 404) ?(root = fail_with_code_num 404)
        list =
  let tbl = Hashtbl.create (List.length list) in
  let next element path req =
    match Hashtbl.find_opt tbl element with
    | Some proc -> proc path req 
    | None -> fail (element :: path) req in
  List.iter (fun (name, proc) -> Hashtbl.add tbl name proc) list;
  path_element next ~fail:root;;

let with_settings proc next path req =
  next path {req with Request.settings = (proc req)};;
