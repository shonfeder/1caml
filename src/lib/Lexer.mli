open Lexing

module type SOURCE = sig
  val on_refill : lexbuf -> unit Lwt.t
end

module type LEXER = sig
  val token : lexbuf -> Token.t Lwt.t
end
type lexer = (module LEXER)

module Make (R : SOURCE) : LEXER

module type STATE = sig
  val chan : Lwt_io.input_channel
  val size : int
end

module LwtSource (S : STATE) : SOURCE

val create : Lwt_io.input_channel -> int -> lexer * lexbuf
val tokens : Lwt_io.input_channel -> Token.t Lwt_stream.t
