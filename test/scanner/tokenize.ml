open Parser
open Ast

type num =
  | Num_int of int
  | Num_float of float

let stringify = function

  (*  Operators - Arithmatic *)
  | PLUS -> "PLUS"     | MINUS -> "MINUS"
  | TIMES -> "TIMES"   | DIVIDE -> "DIVIDE"
  | MOD -> "MOD"       | ASSIGN -> "ASSIGN"

  (*  Operators - Relational *)
  | EQ -> "EQ"    | NEQ -> "NEQ"
  | LT -> "LT"    | GT -> "GT"
  | LTE -> "LTE"  | GTE -> "GTE"

  (* Logical Operators & Keywords *)
  | AND -> "AND"   | OR -> "OR"
  | NOT -> "NOT"   | PARALLEL -> "PARALLEL"

  (* Punctuation *)
  | COLON -> "COLON" | DOT -> "DOT"
  | COMMA -> "COMMA" | RARROW -> "RARROW"

  (* Musical Operators & keywords *)
  | OCTAVE_RAISE -> "OCTAVE_RAISE" | OCTAVE_LOWER -> "OCTAVE_LOWER"
  | SCORE_RESOLUTION -> "SCORE_RESOLUTION" | PITCH -> "PITCH"
  | DURATION -> "DURATION" | NOTE -> "NOTE"
  | CHORD -> "CHORD" | SEQ -> "SEQ"

  (* Scoping  *)
  | LPAREN -> "LPAREN"  | RPAREN -> "RPAREN"
  | LBRACE -> "LBRACE"  | RBRACE -> "RBRACE"
  | LBRACK -> "LBRACK"  | RBRACK -> "RBRACK"

  (* Literals *)
  | LIT_STR(string) -> "LIT_STR" | LIT_INT(num) -> "LIT_INT"

  (* ADD the rest of Keywords`*)
  | LIT_BOOL(true) -> "LIT_BOOL_TRUE" | LIT_BOOL(false) -> "LIT_BOOL_FALSE"
  | LIT_DOUBLE(num) -> "LIT_DOUBLE"
  | ID(lit) -> "ID" | NULL -> "NULL"
  | TYPE_UNIT -> "TYPE_UNIT" | TYPE_BOOL -> "TYPE_BOOL"
  | TYPE_INT -> "TYPE_INT" | TYPE_DOUBLE -> "TYPE_DOUBLE"
  | TYPE_STR -> "TYPE_STR" | TYPE_STRUCT -> "TYPE_STRUCT"
  | TYPE_ENUM -> "TYPE_ENUM" | RETURN -> "RETURN"
  | SEP -> "SEP" | EOF -> "EOF"
  | FUNC -> "FUNC" | USING -> "USING"
  | MODULE -> "MODULE" | MATCH -> "MATCH"
  | MATCHCASE -> "MATCHCASE" | IF -> "IF"
  | ELSE -> "ELSE" | WHILE -> "WHILE"
  | FOR -> "FOR" | IN -> "IN"
  | RANGE -> "RANGE" | BREAK -> "BREAK"
  | CONTINUE -> "CONTINUE" |  ARRAY -> "ARRAY"
let _ =
  let lexbuf = Lexing.from_channel stdin in
  let rec print_tokens = function
    | EOF -> " "
    | token ->
      print_endline (stringify token);
      print_tokens (Scanner.token lexbuf) in
  print_tokens (Scanner.token lexbuf)
