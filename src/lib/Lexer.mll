{
[@@@warning "-22"]

open Lexing
open Token

module type SOURCE = sig
  val on_refill : lexbuf -> unit Lwt.t
end

module type LEXER = sig
  val token : lexbuf -> Token.t Lwt.t
end
type lexer = (module LEXER)

module Make (R : SOURCE) : LEXER = struct
  let refill_handler k lexbuf =
    R.on_refill lexbuf >> k lexbuf

  let make_table num elems =
    let table = Caml.Hashtbl.create num in
    Caml.List.iter (fun (k, v) -> Caml.Hashtbl.add table k v) elems;
    table

  let keywords =
    make_table 1 [
      ("def", KEYWORD_DEF);
      ("fun", KEYWORD_FUN);
      ("include", KEYWORD_INCLUDE);
      ("mod", KEYWORD_MOD);
      ("open", KEYWORD_OPEN);
      ("sig", KEYWORD_SIG);
      ("val", KEYWORD_VAL);
      ("type", KEYWORD_TYPE);
      ("with", KEYWORD_WITH);
    ]
}

let line_ending
  = '\r'
  | '\n'
  | "\r\n"
let number =
  ['0'-'9']+
let whitespace =
  [' ' '\t']+
let identifier_initial =
  [^ '0'-'9' '(' ')' '[' ']' '{' '}' '.' '#' '\\' '"' ' ' '\t' '\n' '\r']
let identifier_subsequent =
  [^ '(' ')' '[' ']' '{' '}' '.' '#' '\\' '"' ' ' '\t' '\n' '\r']

refill {refill_handler}

rule token = parse
  | identifier_initial identifier_subsequent*
{
  let input = lexeme lexbuf in
  try
    let kwd = Caml.Hashtbl.find keywords input in
    Lwt.return kwd
  with Not_found ->
    Lwt.return (IDENTIFIER input)
}
  | '('
{ Lwt.return LEFT_PARENTHESIS }
  | '['
{ Lwt.return LEFT_SQUARE_BRACKET }
  | ')'
{ Lwt.return RIGHT_PARENTHESIS }
  | ']'
{ Lwt.return RIGHT_SQUARE_BRACKET }
  | line_ending
{ new_line lexbuf; token lexbuf }
  | whitespace
{ token lexbuf }
  | eof
{ Lwt.return EOF }
  | _
{ Lwt_io.printlf "Unexpected char: %s" (lexeme lexbuf) >> token lexbuf }

{
end (* LEXER *)

module type STATE = sig
  val chan : Lwt_io.input_channel
  val size : int
end

module LwtSource (S : STATE): SOURCE = struct
  let resize b n =
    if (b.lex_buffer_len + n) > (Bytes.length b.lex_buffer) then begin
      let tmp_buf = ref b.lex_buffer in
      if (b.lex_buffer_len - b.lex_start_pos + n) > Bytes.length b.lex_buffer then begin
        let new_len = min (2 * Bytes.length b.lex_buffer) Sys.max_string_length in
        if b.lex_buffer_len - b.lex_start_pos + n > new_len then
          failwith "cannot resize buffer"
        else
          tmp_buf := Bytes.create new_len
      end;
      Bytes.blit b.lex_buffer b.lex_start_pos !tmp_buf 0 (b.lex_buffer_len - b.lex_start_pos);
      b.lex_buffer <- !tmp_buf;
      for i = 0 to Array.length b.lex_mem - 1 do
        if b.lex_mem.(i) >= 0 then
          b.lex_mem.(i) <- b.lex_mem.(i) - b.lex_start_pos
      done;
      b.lex_abs_pos    <- b.lex_abs_pos    + b.lex_start_pos;
      b.lex_curr_pos   <- b.lex_curr_pos   - b.lex_start_pos;
      b.lex_last_pos   <- b.lex_last_pos   - b.lex_start_pos;
      b.lex_buffer_len <- b.lex_buffer_len - b.lex_start_pos;
      b.lex_start_pos  <- 0;
    end

  let on_refill b =
    let aux_buffer = Bytes.create S.size in
    let%lwt n = Lwt_io.read_into S.chan aux_buffer 0 S.size in
    if n = 0 then
      Lwt.return (b.lex_eof_reached <- true)
    else begin
      resize b n;
      Bytes.blit aux_buffer 0 b.lex_buffer b.lex_buffer_len n;
      Lwt.return (b.lex_buffer_len <- b.lex_buffer_len + n)
    end
end

let create chan size =
  let pkg : lexer = (module Make(LwtSource(struct
    let chan = chan
    let size = size
  end))) in
  let zero_pos = {
    pos_fname = "";
    pos_lnum  = 1;
    pos_bol   = 0;
    pos_cnum  = 0;
  } in
  let buf = {
    refill_buff     = begin fun _ -> () end;
    lex_buffer      = Bytes.create size;
    lex_buffer_len  = 0;
    lex_abs_pos     = 0;
    lex_start_pos   = 0;
    lex_curr_pos    = 0;
    lex_last_pos    = 0;
    lex_last_action = 0;
    lex_mem         = [| |];
    lex_eof_reached = false;
    lex_start_p     = zero_pos;
    lex_curr_p      = zero_pos;
  } in (pkg, buf)

let tokens chan =
  let len = 1024 in
  let (pkg, buf) = create chan len in
  let module Lwt_lex = (val pkg : LEXER) in
  let go () = match%lwt Lwt_lex.token buf with
    | tok -> Lwt.return (Some tok)
  in Lwt_stream.from go
}
