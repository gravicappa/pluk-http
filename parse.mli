(* Pluk-http
  Copyright 2018 Ramil Farkhshatov
  All rights reserved. This file is distributed under the terms of the
  GNU Lesser General Public License version 3 with OCaml linking exception *)

(** Module [pluk-http]: parsing definitions *)

exception Parse_error of string
(** Exception for reporting parsing errors *)

type header_fields = {
  generic: (string, string) Hashtbl.t;
  cookies: (string, string) Hashtbl.t;
  mutable content_length: int option;
  mutable content_type: Base.Content_type.t option;
  mutable content_disposition: Base.Content_disposition.t option;
}
(** record accessing parsed header fields *)

val str_percent_parser:
  (char -> bool) -> char Pluk_parser.stream ->
  (string, char) Pluk_parser.parse_result
(** [str_percent_parse test stream] parses percent encoded string while
    [test] of unencoded char returns true *)

val word: char Pluk_parser.stream -> (string, char) Pluk_parser.parse_result
(** Parses word till whitespace *)

val ws: char Pluk_parser.stream -> (unit, char) Pluk_parser.parse_result
(** Parses whitespace of any length *)

val space: char Pluk_parser.stream -> (char, char) Pluk_parser.parse_result
(** Parses a single space *)

val spaces: char Pluk_parser.stream -> (unit, char) Pluk_parser.parse_result
(** Parses any number of spaces *)

val crlf: char Pluk_parser.stream -> (string, char) Pluk_parser.parse_result
(** Parses "\r\n" sequence *)

val either:
  ('a Pluk_parser.stream -> ('b, 'a) Pluk_parser.parse_result) ->
  'b -> 'a Pluk_parser.stream -> ('b, 'a) Pluk_parser.parse_result
(** [either parser other stream] return successful result of a [parser] on 
    a [stream] or [other] on failure. *)

val delimited:
  ('a Pluk_parser.stream -> ('b, 'a) Pluk_parser.parse_result) ->
  ('a Pluk_parser.stream -> ('c, 'a) Pluk_parser.parse_result) ->
  'a Pluk_parser.stream -> ('c list, 'a) Pluk_parser.parse_result
(** [delimited delimiter_parser item_parser stream] parses a series of
    [item_parsers] delimited by [delimiter_parser]. *)

val spaced_item:
  (char Pluk_parser.stream -> ('a, char) Pluk_parser.parse_result) ->
  char Pluk_parser.stream -> (unit, char) Pluk_parser.parse_result
(** [spaced_item item_parser stream] parses [item_parser] enclosed in
    optional spaces *)

val content_type:
  char Pluk_parser.stream ->
    (Base.Content_type.t, char) Pluk_parser.parse_result
(** Parses [Content_type.t] *)

val content_disposition:
  char Pluk_parser.stream ->
    (Base.Content_disposition.t, char) Pluk_parser.parse_result
(** Parses [Content_disposition.t] *)

val query: char Pluk_parser.stream ->
  ((string * string) list, char) Pluk_parser.parse_result
(** Parses HTTP GET query (["one=1&two=three"]) *)

val uri: char Pluk_parser.stream -> (Base.Uri.t, char) Pluk_parser.parse_result
(** Parses [Base.Uri.t] *)

val exact_string_ci:
  string -> char Pluk_parser.stream ->
    (string, char) Pluk_parser.parse_result
(** Parses exact string case insensitive *)

val cookie: char Pluk_parser.stream ->
  ((string * string) list, char) Pluk_parser.parse_result
(** Parses cookie entry *)

val http_header_field:
  header_fields -> char Pluk_parser.stream ->
    (header_fields, char) Pluk_parser.parse_result
(** [http_header_field fields stream] parses HTTP header field mutating
    [header_fields] record *)

val http_header_fields:
  char Pluk_parser.stream -> (header_fields, char) Pluk_parser.parse_result
(** Parses HTTP header fields *)
