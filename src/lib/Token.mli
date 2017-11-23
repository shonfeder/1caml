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

module Pretty : sig
  val pp : t Fmt.t
  val pp_utf_8 : t Fmt.t
end
