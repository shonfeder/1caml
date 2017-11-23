open Format

type t =
  | EOF
  | IDENTIFIER of string
  | KEYWORD_DEF
  | KEYWORD_FUN
  | KEYWORD_INCLUDE
  | KEYWORD_MOD
  | KEYWORD_OPEN
  | KEYWORD_SIG
  | KEYWORD_TYPE
  | KEYWORD_VAL
  | KEYWORD_WITH
  | LEFT_PARENTHESIS
  | LEFT_SQUARE_BRACKET
  | RIGHT_PARENTHESIS
  | RIGHT_SQUARE_BRACKET
[@@deriving (eq, ord, show)]

module Pretty = struct
  let pp fmt = function
    | EOF ->
      fprintf fmt "@\n"
    | IDENTIFIER id ->
      fprintf fmt "%a" Fmt.string id
    | KEYWORD_DEF ->
      fprintf fmt "%a" Fmt.string "def"
    | KEYWORD_FUN ->
      fprintf fmt "%a" Fmt.string "fun"
    | KEYWORD_INCLUDE ->
      fprintf fmt "%a" Fmt.string "include"
    | KEYWORD_MOD ->
      fprintf fmt "%a" Fmt.string "mod"
    | KEYWORD_OPEN ->
      fprintf fmt "%a" Fmt.string "open"
    | KEYWORD_SIG ->
      fprintf fmt "%a" Fmt.string "sig"
    | KEYWORD_TYPE ->
      fprintf fmt "%a" Fmt.string "type"
    | KEYWORD_VAL ->
      fprintf fmt "%a" Fmt.string "val"
    | KEYWORD_WITH ->
      fprintf fmt "%a" Fmt.string "with"
    | LEFT_PARENTHESIS ->
      fprintf fmt "%a" Fmt.string "("
    | LEFT_SQUARE_BRACKET ->
      fprintf fmt "%a" Fmt.string "["
    | RIGHT_PARENTHESIS ->
      fprintf fmt "%a" Fmt.string ")"
    | RIGHT_SQUARE_BRACKET ->
      fprintf fmt "%a" Fmt.string "]"

  let pp_utf_8 fmt = function
    | IDENTIFIER id ->
      fprintf fmt "%a" Uuseg_string.pp_utf_8 id
    | token -> pp fmt token
end
