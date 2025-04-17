(* Pluk-http
  Copyright 2018 Ramil Farkhshatov
  All rights reserved. This file is distributed under the terms of the
  GNU Lesser General Public License version 3 with OCaml linking exception *)

open Base
module P = Pluk_parser

let file_prefix = "pluk-http-upload-"

let index_from_to_opt bytes start finish char =
  let rec loop i =
    if i < finish then begin
      if (Bytes.get bytes i) = char then
        Some i
      else
        loop (i + 1)
    end else
      None in
  loop start

let limited_reader input len =
  let read_limited len block start finish =
    let e1 = start + !len in
    let n = ((min e1 finish) - start) in
    let%lwt n = Lwt_io.read_into input block start n in
    len := !len - n;
    Lwt.return n in

  let read_unlimited block start finish =
    Lwt_io.read_into input block start (finish - start) in

  match len with
  | Some n -> read_limited (ref n)
  | None -> read_unlimited

let equals_from_to a a_pos b b_pos n =
  let rec loop a_i b_i i =
    if i < n then begin
      if (Bytes.get a a_i) = (Bytes.get b b_i) then
        loop (a_i + 1) (b_i + 1) (i + 1)
      else
        false
    end else
      true in
  loop a_pos b_pos 0

let header_end = Bytes.of_string "\r\n\r\n"

let bytes_starts_with bytes off a b =
  ((Bytes.get bytes off) = a) && ((Bytes.get bytes (off + 1)) = b)

let open_receiving_file =
  let perm = 0o660 in
  fun dir ->
    let prefix = file_prefix in
    match dir with
    | Some temp_dir -> Lwt_io.open_temp_file ~temp_dir ~perm ~prefix ()
    | None -> Lwt_io.open_temp_file ~perm ~prefix ()

let fold_input_till reader block boundary proc acc =
  let rec search bytes off start finish acc =
    let boundary_len = Bytes.length boundary in
    match index_from_to_opt bytes off finish (Bytes.get boundary 0) with
    | Some pos when (pos + boundary_len + 2) >= finish ->
        let%lwt acc = proc bytes start pos acc in
        Bytes.blit bytes pos bytes 0 (finish - pos);
        fill_buf bytes 0 (finish - pos) acc
    | Some pos when equals_from_to boundary 0 bytes pos boundary_len ->
        let%lwt acc = proc bytes start pos acc in
        Lwt.return ((bytes, pos + boundary_len, finish), acc)
    | Some pos -> search bytes (pos + 1) start finish acc
    | None -> 
        let%lwt acc = proc bytes start finish acc in
        fill_buf bytes 0 0 acc

  and fill_buf bytes start finish acc =
    match%lwt reader bytes finish (Bytes.length bytes) with
    | 0 when start = finish -> Lwt.fail (Parse.Parse_error "Stream depleted")
    | 0 ->
        let%lwt acc = proc bytes start finish acc in
        Lwt.return ((bytes, finish, finish), acc)
    | n -> search bytes start start (finish + n) acc in

  let bytes, start, finish = block in
  search bytes start start finish acc

type f = {
  fold_input: 'a. bytes * int * int -> bytes ->
              (bytes -> int -> int -> 'a -> 'a Lwt.t) ->
              'a -> ((bytes * int * int) * 'a) Lwt.t
}

let multi {fold_input} boundary bytes dir test_file tbl =
  let boundary = Bytes.cat (Bytes.of_string "\r\n--") boundary in

  let string_parameter name content_type buf = 
    {Parameter.name; content_type; value = String (Buffer.contents buf)} in

  let file_parameter name content_type filename path = 
    {Parameter.name; content_type; value = File {filename; path}} in

  let add_parameter = function
    | {Parameter.name; _} as p -> Hashtbl.add tbl name p in

  let is_end_of_part bytes start = bytes_starts_with bytes start '\r' '\n' in

  let is_end_of_req bytes start = bytes_starts_with bytes start '-' '-' in

  let to_null _ _ _ () = Lwt.return () in

  let collect_buf bytes start finish buf =
    let () = Buffer.add_subbytes buf bytes start (finish - start) in
    Lwt.return buf in

  let collect_file bytes start finish out =
    let%lwt () = Lwt_io.write_from_exactly out bytes start (finish - start) in
    Lwt.return out in

  let read_header buf =
    Buffer.add_string buf "\r\n\r\n";
    let stream = P.Stream.of_bytes (Buffer.to_bytes buf) in
    match Parse.http_header_fields stream with
    | Ok (fields, _) -> fields
    | Error (msg, _) ->
        let str = msg
                  |> P.string_of_msg
                  |> Option.value ~default: "Unknown" in
        raise (Parse.Parse_error str) in

  let read_part_string name block =
    let buf = Buffer.create 16 in
    let ctype = Content_type.create "text/plain" in
    let%lwt block, buf = fold_input block boundary collect_buf buf in
    let () = add_parameter (string_parameter name ctype buf) in
    Lwt.return block in

  let read_part_file ctype cdisp block =
    match cdisp with
    | {Content_disposition.name = Some name; filename = Some filename; _} ->
        if test_file filename then begin
          let%lwt path, out = open_receiving_file dir in
          let%lwt block, out = fold_input block boundary collect_file out in
          let%lwt () = Lwt_io.close out in
          let () = add_parameter (file_parameter name ctype filename path) in
          Lwt.return block
        end else
          Lwt.fail (Parse.Parse_error "read_part_file : test_file => false")
    | _ -> Lwt.fail (Parse.Parse_error "read_part_file") in

  let read_part (fields: Parse.header_fields) block =
    match fields.content_type, fields.content_disposition with
    | None, Some {Content_disposition.name = Some name; _} ->
        read_part_string name block
    | Some ctype, Some cdisp -> read_part_file ctype cdisp block
    | _, _ -> Lwt.fail (Parse.Parse_error "read_part") in

  let rec loop block =
    match block with
    | bytes, start, finish when is_end_of_part bytes start ->
        let buf = Buffer.create 16 in
        let block = bytes, start + 2, finish in
        let%lwt block, buf = fold_input block header_end collect_buf buf in
        let hdr = read_header buf in
        let%lwt block = read_part hdr block in
        loop block
    | bytes, start, _ when is_end_of_req bytes start ->
        Lwt.return tbl
    | _ -> Lwt.fail (Parse.Parse_error "loop") in
  Bytes.set bytes 0 '\r';
  Bytes.set bytes 1 '\n';
  let%lwt block, () = fold_input (bytes, 0, 2) boundary to_null () in
  loop block

let parse input length boundary bytes dir test_file tbl =
  let boundary = Bytes.of_string boundary in
  let fold_input block boundary proc acc =
    fold_input_till (limited_reader input length) block boundary proc acc in
  multi {fold_input} boundary bytes dir test_file tbl
