(*
 * Authors:
 *  - Ruonan Xu
 *)

{
  open Parser
}

let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let letter = lowercase | uppercase
let digit = ['0'-'9']
let newline = ('\n' | '\r' | "\r\n")
let whitespace = [' ' '\t']
let separator = ';'

(* Used for float parsing *)
let hasint = digit+ '.' digit*
let hasfrac = digit* '.' digit+
let hasexp = 'e' ('+'? | '-') digit+

let pitch = ['A'-'G'] (['1'-'7'] ('#'|'b')?)?


(* Regex conflicts are resolved by order *)
rule token = parse
  | newline { token lexbuf }
  | whitespace { token lexbuf }
  | separator { SEP }
  | "/*" { comment lexbuf } (* Comments *)
  | "//" { comment_oneline lexbuf }
(* ------------- Scoping ------------- *)
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '[' { LBRACK }
  | ']' { RBRACK }
  | '{' { LBRACE }
  | '}' { RBRACE }
(* ------------- Operators ------------- *)
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { TIMES }
  | '/' { DIVIDE }
  | '%' { MOD }
  | '=' { ASSIGN }
  | "==" { EQ }
  | "!=" { NEQ }
  | '<' { LT }
  | "<=" { LTE }
  | '>' { GT }
  | ">=" { GTE }
  | ':' { COLON }
	| '.' { DOT }
  | ',' { COMMA }
  | '!' { NOT }
  | "&" { PARALLEL }
  | "and" { AND }
  | "or" { OR }
  | "->" { RARROW }
  | "::" { SCORE_RESOLUTION }
  | '^' { OCTAVE_RAISE }
  | '_' { OCTAVE_LOWER }
  | "=>" { MATCHCASE }
(* ------------- Keywords ------------- *)
  | "unit" { TYPE_UNIT }
  | "bool" { TYPE_BOOL }
  | "int" { TYPE_INT }
  | "double" { TYPE_DOUBLE }
  | "char" { TYPE_CHAR }
  | "string" { TYPE_STR }
  | "Struct" { TYPE_STRUCT }
  | "Enum" { TYPE_ENUM }
  | "if" { IF }
  | "else"{ ELSE }
  | "match" { MATCH }
  | "while" { WHILE }
  | "for" { FOR }
  | "in" { IN }
  | "range" { RANGE }
  | "break" { BREAK }
  | "continue" { CONTINUE }
  | "func" { FUNC }
  | "return" { RETURN }
  | "using" { USING }
  | "module" { MODULE }
(*
  | "null" { NULL }
*)
  | "true" { LIT_BOOL(true) }
  | "false" { LIT_BOOL(false) }
(* ------------- Music Keywords ------------- *)
  | "pitch" { TYPE_PITCH }
  | "duration" { TYPE_DURATION }
  | "Note" { TYPE_NOTE }
  | "Chord" { TYPE_CHORD }
  | "Seq" { TYPE_SEQ }
(* ------------- Literals ------------- *)
  | pitch as lit { LIT_PITCH(lit) }
  | digit+ as lit { LIT_INT(int_of_string lit) }
  | ((hasint | hasfrac) hasexp?) | (digit+ hasexp) as lit { LIT_DOUBLE(float_of_string lit) }
  | '"' (('\\' '"'| [^'"'])* as str) '"' { LIT_STR(Scanf.unescaped str) }
  | (letter | '_') (letter | digit | '_')* as lit { ID(lit) } (* Identifiers *)
  | eof { EOF }
  | _ as c { raise (Exceptions.Lexing_error("Unknown token '" ^ String.make 1 c ^ "'")) }

and comment = parse
    "*/" { token lexbuf }
  | _ { comment lexbuf }

and comment_oneline = parse
    (newline | eof) { token lexbuf }
  | _ { comment_oneline lexbuf }


(*
	TODO:
	space concat
  Note Literals
*)
