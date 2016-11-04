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


  (* Scoping  *)
  | LPAREN -> "LPAREN"  | RPAREN -> "RPAREN"
  | LBRACE -> "LBRACE"  | RBRACE -> "RBRACE"
  | LBRACK -> "LBRACK"  | RBRACK -> "RBRACK"

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let rec print_tokens = function
    | EOF -> " "
    | token ->
      print_endline (stringify token);
      print_tokens (Scanner.token lexbuf) in
  print_tokens (Scanner.token lexbuf)
