(* Pluk-http
  Copyright 2018 Ramil Farkhshatov
  All rights reserved. This file is distributed under the terms of the
  GNU Lesser General Public License version 3 with OCaml linking exception *)

open Base
module P = Pluk_parser

exception Parse_error of string

type header_fields = {
  generic: (string, string) Hashtbl.t;
  cookies: (string, string) Hashtbl.t;
  mutable content_length: int option;
  mutable content_type: Content_type.t option;
  mutable content_disposition: Content_disposition.t option;
}

let str_percent_parser test =
  P.(string_of (some_of [next (satisfies ((=) '%')) (fun _ ->
                           next (number_n 16 2) (fun x s ->
                             Ok (Char.chr x, s)));
                         satisfies test]))

let word = P.string_if ((<) ' ')
let ws = P.(satisfy zero_or_many ((>=) ' '))

let space = P.exact_item ' '

let spaces =
  let is_space c = (c = ' ') || (c = '\t') in
  P.(satisfy zero_or_many is_space)

let crlf = P.(some_of [exact_string "\n"; exact_string "\r\n"])

let either element default = P.some_of [element; P.const default]

let delimited delim element =
  P.(some_of [(convert
               (fun x -> List.rev x)
               (next element (fun x ->
                 zero_or_many [x] (fun acc ->
                   next delim (fun _ ->
                     next element (fun x s ->
                       Ok (x :: acc, s)))))));
              const []])

let spaced_item item =
  P.(next spaces (fun _ ->
       next item (fun _ ->
         next spaces (fun _ s ->
           Ok ((), s)))))

let content_type =
  let delim = spaced_item (P.exact_item ';') in
  let element break c = (c > ' ') && (c <> break) in
  let entry =
    P.(next (string_if (element '=')) (fun key ->
         next (exact_item '=') (fun _ ->
           next (string_if (element ';')) (fun value s ->
             Ok ((key, value), s))))) in
  let variants = P.next delim (fun _ -> delimited delim entry) in
  P.(next (string_if (element ';')) (fun media_type ->
       next (either variants []) (fun alist s ->
        let charset = (List.assoc_opt "charset" alist) in
        let boundary = (List.assoc_opt "boundary" alist) in
        Ok (Content_type.create media_type ~charset ~boundary (),
                 s))))

let content_disposition =
  let delim = spaced_item (P.exact_item ';') in
  let element break c = (c > ' ') && (c <> break) in
  let quoted_value parser =
    P.(next (exact_item '"') (fun _ ->
         next parser (fun x ->
           next (exact_item '"') (fun _ s ->
             Ok (x, s))))) in
  let entry_value = P.(some_of [quoted_value (string_if ((<>) '"'));
                                string_if (element ';')]) in
  let entry =
    P.(next (string_if (element '=')) (fun key ->
         next (exact_item '=') (fun _ ->
           next entry_value (fun value s ->
             Ok ((key, value), s))))) in
  let variants = P.next delim (fun _ -> delimited delim entry) in
  let all_true _ = true in
  let str_pc str =
    match (str_percent_parser all_true (P.Stream.of_string str)) with
    | Ok (str, _) -> Some str
    | _ -> None in
  let decode = function
    | Some x -> (str_pc x)
    | None -> None in
  let choose a b =
    match a with
    | Some x -> Some x
    | None -> b in
  let mode =
    P.(some_of [convert (fun _ -> Content_disposition.Inline)
                        (exact_string "inline");
                convert (fun _ -> Content_disposition.Attachment)
                        (exact_string "attachment");
                convert (fun _ -> Content_disposition.Form_data)
                        (exact_string "form-data")]) in
  P.(next mode (fun mode ->
       next (either variants []) (fun alist s ->
         let name = (List.assoc_opt "name" alist) in
         let filename = choose (List.assoc_opt "filename" alist)
                               (decode (List.assoc_opt "filename*" alist)) in
         Ok ({Content_disposition.mode; name; filename}, s))))

let query =
  let item_test c = (c > ' ') && (c <> '=') && (c <> '&') && (c <> '#') in
  let item =
    P.(next (str_percent_parser item_test) (fun key ->
         next (exact_item '=') (fun _ ->
           next (str_percent_parser item_test) (fun value s ->
             Ok ((key, value), s))))) in
  delimited (P.exact_item '&') item

let uri =
  let scheme =
    P.(next (string_if (fun c -> (c > ' ') && (c <> ':'))) (fun scheme ->
         next (exact_item ':') (fun _ s ->
           Ok (scheme, s)))) in

  let userinfo_pass =
    P.(next (exact_item ':') (fun _ ->
         next (str_percent_parser ((<>) '@')) (fun pass s ->
           Ok (pass, s)))) in

  let userinfo =
    P.(next (str_percent_parser (fun x -> (x <> ':') && (x <> '@')))
            (fun user ->
              next (either userinfo_pass "") (fun pass ->
                next (exact_item '@') (fun _ s ->
                  Ok ((user, pass), s))))) in

  let host = P.string_if (fun c -> (c > ' ') && (c <> ':') && (c <> '/')) in

  let port = P.(next (exact_item ':') (fun _ ->
    next (number 10) (fun port s ->
      Ok (Some port, s)))) in

  let path_test c = (c > ' ') && (c <> '/') && (c <> '?') && (c <> '#') in

  let path =
    P.(next (delimited (exact_item '/') (string_if path_test))
            (fun path s -> Ok (path, s))) in

  let authority =
    P.(next (either userinfo ("", "")) (fun (user, pass) ->
      next host (fun host ->
        next (either port None) (fun port s ->
          Ok ((user, pass, host, port), s))))) in

  let hier_auth_path_abempty =
    P.(next (exact_string "//") (fun _ ->
      next authority (fun (user, pass, host, port) ->
        next (exact_item '/') (fun _ ->
          next (either path []) (fun path s ->
            Ok ((user, pass, host, port, path), s)))))) in

  let hier_path_abs =
    P.(next (exact_item '/') (fun _ ->
         next (either path []) (fun path s ->
           Ok (("", "", "", None, path), s)))) in

  let hier_path_rootless =
    P.(next (either path []) (fun path s ->
         Ok (("", "", "", None, path), s))) in
  let hier_path_empty = P.const ("", "", "", None, []) in

  let hier = P.some_of [hier_auth_path_abempty;
                        hier_path_abs;
                        hier_path_rootless;
                        hier_path_empty] in

  let query_start =
    P.(next (exact_item '?') (fun _ ->
      next (either (exact_item '&') '&') (fun _ s ->
        Ok ((), s)))) in

  let query =
    P.(next query_start (fun () ->
        next query (fun query s ->
          Ok (query, s)))) in

  let fragment =
    P.(next (exact_item '#') (fun _ ->
         next (string_if ((<) ' ')) (fun fragment s ->
           Ok (fragment, s)))) in

  P.(next (either scheme "") (fun proto ->
      next (either hier ("", "", "", None, []))
           (fun (user, password, host, port, path) ->
              next (either query []) (fun query ->
                next (either fragment "") (fun fragment s ->
                  Ok ({Uri.protocol = proto; user; password; host; port;
                             path; query; fragment}, s))))))

let exact_string_ci string =
  let test_ci a b = (Char.lowercase_ascii a) = b in
  P.match_string string test_ci

(*let field key = P.convert (fun x -> (key, x));; FIXME: remove *)

let cookie =
  let not_delim c = (c > ' ') && (c <> ';') in
  let element =
    P.(next (str_percent_parser ((<>) '=')) (fun key ->
         next (exact_item '=') (fun _ ->
           next (str_percent_parser not_delim)
                (fun value s -> Ok ((key, value), s))))) in
  delimited (P.exact_string "; ") element

let http_header_field (cell: header_fields) =
  let field key_parser value_parser proc =
    P.(next key_parser (fun key ->
         next spaces (fun _ ->
           next (exact_item ':') (fun _ ->
             next spaces (fun _ ->
               next value_parser (fun value ->
                 next crlf (fun _ s ->
                   ignore (proc (key, value));
                   Ok (cell, s)))))))) in
  let key = P.string_if (fun c -> (c <> ' ') && (c <> ':')) in
  let value = P.string_if (fun c -> (c <> '\n') && (c <> '\r')) in
  let cookie = field (exact_string_ci "cookie") cookie (fun (_, cs) ->
                 List.iter (fun (k, v) -> Hashtbl.add cell.cookies k v) cs) in
  let ctype = field (exact_string_ci "content-type") content_type
                    (fun (_, v) -> cell.content_type <- Some v) in
  let cdisp = field (exact_string_ci "content-disposition")
                    content_disposition
                    (fun (_, v) -> cell.content_disposition <- Some v) in
  let clen = field (exact_string_ci "content-length")
                   (P.number 10)
                   (fun (_, len) -> cell.content_length <- Some len) in
  let generic = field key value (fun (k, v) ->
                                      let k = String.lowercase_ascii k in
                                      Hashtbl.add cell.generic k v) in
  P.some_of [cookie; ctype; cdisp; clen; generic]

let http_header_fields stream =
  let cell = {generic = Hashtbl.create 1; cookies = Hashtbl.create 1;
              content_type = None; content_disposition = None;
              content_length = None} in
  let proc cell =
    P.next (http_header_field cell) (fun cell s ->
      Ok (cell, s)) in
  P.zero_or_many cell proc stream
