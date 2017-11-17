open Format

type token =
  | EOF
  | IDENTIFIER of string
  | KEYWORD_TYPE
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
      fprintf fmt "%a" (Fmt.string) id
    | KEYWORD_TYPE ->
      fprintf fmt "%a" (Fmt.string) "type"
    | LEFT_PARENTHESIS ->
      fprintf fmt "%a" (Fmt.string) "("
    | LEFT_SQUARE_BRACKET ->
      fprintf fmt "%a" (Fmt.string) "["
    | RIGHT_PARENTHESIS ->
      fprintf fmt "%a" (Fmt.string) ")"
    | RIGHT_SQUARE_BRACKET ->
      fprintf fmt "%a" (Fmt.string) "]"

  let pp_utf_8 fmt = function
    | EOF ->
      fprintf fmt "@\n"
    | IDENTIFIER id ->
      fprintf fmt "%a" (Uuseg_string.pp_utf_8) id
    | KEYWORD_TYPE ->
      fprintf fmt "%a" (Fmt.string) "type"
    | LEFT_PARENTHESIS ->
      fprintf fmt "%a" (Fmt.string) "("
    | LEFT_SQUARE_BRACKET ->
      fprintf fmt "%a" (Fmt.string) "["
    | RIGHT_PARENTHESIS ->
      fprintf fmt "%a" (Fmt.string) ")"
    | RIGHT_SQUARE_BRACKET ->
      fprintf fmt "%a" (Fmt.string) "]"
end
