(* Pluk-http
  Copyright 2018 Ramil Farkhshatov
  All rights reserved. This file is distributed under the terms of the
  GNU Lesser General Public License version 3 with OCaml linking exception *)

open Base

let content_types = Hashtbl.create 16
let content_type_octet_stream = Content_type.create "application/octet-stream"
                                                    ~charset:(Some "binary")
                                                    ()
let invalid_content_type = Content_type.create "" ()

let () =
  let charset = Some "UTF-8" in
  List.iter (fun (ext, ctype) -> Hashtbl.add content_types ext ctype) [
    (".txt", Content_type.create "text/plain" ~charset ());
    (".css", Content_type.create "text/css" ~charset ());
    (".js", Content_type.create "text/javascript" ~charset ());
    (".json", Content_type.create "application/json" ~charset ());
    (".html", Content_type.create "text/html" ~charset ());
    (".png", Content_type.create "image/png" ());
    (".jpg", Content_type.create "image/jpeg" ());
    (".jpeg", Content_type.create "image/jpeg" ());
    (".gif", Content_type.create "image/gif" ());
  ]

let content_type_of_filename path =
  match Hashtbl.find_opt content_types (Filename.extension path) with
  | Some content_type -> content_type
  | None -> content_type_octet_stream

let add_content_header (resp: Response.t) content_type mtime =
  resp.content_type <- Some content_type;
  Response.add_header resp "Date" (Time.to_string (Unix.time ()));
  Response.add_header resp "Last-Modified" (Time.to_string mtime);
  Response.add_header resp "Cache-Control" "private"

let string_content ?(content_type = content_type_octet_stream)
                   ?(header = []) str =
  let mtime = Unix.time () in
  let len = String.length str in
  let write output = Lwt_io.write output str in
  fun _ _ ->
    let resp = Response.create ~header ~body:(Proc (Some len, write)) () in
    add_content_header resp content_type mtime;
    Lwt.return resp

let not_found () =
  Lwt.return (Response.(create ~code: (int_of_code Not_found) ()))

let file_content ?(content_type = invalid_content_type) ?(header = [])
                 ?(buf_size = (1 lsl 20)) fs_path =

  let content_type = (if content_type <> invalid_content_type
                      then content_type
                      else content_type_of_filename fs_path) in

  let rec write_blocks input output bytes =
    let%lwt n = Lwt_io.read_into input bytes 0 (Bytes.length bytes) in
    if n <> 0 then
      let%lwt () = Lwt_io.write_from_exactly output bytes 0 n in
      write_blocks input output bytes
    else
      Lwt.return () in

  let write' output input =
    let bytes = Bytes.create buf_size in
    write_blocks input output bytes in

  let write output =
    let flags = [Unix.O_RDONLY; Unix.O_NONBLOCK] in
    Lwt_io.with_file ~flags ~mode:Input fs_path (write' output) in

  let serve_file _ =
    let stats = Unix.stat fs_path in
    let body = Response.Proc (Some stats.st_size, write) in
    let response = Response.create ~body ~header () in
    add_content_header response content_type stats.st_mtime;
    Lwt.return response in

  fun path req ->
    if (path = []) && (Sys.file_exists fs_path) then
      serve_file req
    else
      not_found ()

let normalize_path_within path =
  let rec loop acc = function
    | ".." :: path ->
        if acc <> [] then
          loop (List.tl acc) path
        else
          None
    | "." :: path -> loop acc path
    | element :: path -> loop (element :: acc) path
    | [] -> Some (List.rev acc) in
  loop [] path

let path_append_join root path =
  let rec loop sep acc = function
    | element :: path -> loop "/" (acc ^ sep ^ element) path
    | [] -> acc in
  loop (if root = "" then "" else "/") root path

let dummy_dir_proc _ _ _ = not_found ()

let dir_content ?(buf_size = (1 lsl 20)) ?(dir_proc = dummy_dir_proc)
                ?(header = []) dir_path path req =
  match normalize_path_within path with
  | Some path ->
      let fs_path = path_append_join dir_path path in
      if Sys.file_exists fs_path then begin
        if Sys.is_directory fs_path then
          dir_proc fs_path path req
        else
          file_content ~header ~buf_size fs_path [] req
      end else
        not_found ()
  | None -> not_found ()
