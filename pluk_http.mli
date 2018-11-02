type http_method =
  Base.http_method =
    GET
  | HEAD
  | POST
  | PUT
  | DELETE
  | CONNECT
  | OPTIONS
  | TRACE
  | PATCH
module Uri = Base.Uri
module Content_type = Base.Content_type
module Content_disposition = Base.Content_disposition
val esc_into_buffer: string -> Buffer.t -> unit
val buffer_of_escaped: string -> Buffer.t
val esc: string -> string
module Time = Base.Time
module Cookie = Base.Cookie
module Parameter = Base.Parameter
module Settings = Base.Settings
module Parse = Parse
module Multipart = Multipart
module Request = Request
module Response = Response
module Dispatch = Dispatch
module Static = Static
module Util = Util
