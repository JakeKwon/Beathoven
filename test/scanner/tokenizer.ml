open Parser
open Ast

type num =
  | Num_int of int
  | Num_float of float

let stringify = function

  (* Arithmetic Operators *)
  | PLUS -> "PLUS"     | MINUS -> "MINUS"
  | TIMES -> "TIMES"   | DIVIDE -> "DIVIDE"
  | MOD -> "MOD"       | POWER -> "POWER"

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let rec print_tokens = function
    | EOF -> " "
    | token ->
      print_endline (stringify token);
      print_tokens (Scanner.token lexbuf) in
  print_tokens (Scanner.token lexbuf)
