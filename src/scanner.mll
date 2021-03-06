(*
 * Authors:
 *  - Ruonan Xu
 *  - Jake Kwon
 *  - Sona Roy
 *)

{
  open Parser

  let get_char str =
    let str = (Scanf.unescaped str) in
    str.[1]
}

let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let letter = lowercase | uppercase
let digit = ['0'-'9']
let newline = ('\n' | '\r' | "\r\n")
let whitespace = [' ' '\t']
let separator = ';'

(* Used for char *)
let escape = '\\' ['\\' ''' '"' 'n' 'r' 't']
let ascii = ([' '-'!' '#'-'[' ']'-'~'])

(* Used for float parsing *)
let hasint = digit+ '.' digit*
let hasfrac = digit* '.' digit+
let hasexp = 'e' ('+'? | '-') digit+

let pitch = ['A'-'G'] ( (['0'-'9'] | "10" ) ('#'|'b')?)?
let pitch_relative = ['1'-'7'] ('^'|'_')


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
  | '\\' { DIVIDE } (* otherwise need to infer what's 1/4, binop or duration *)
  | '%' { MOD }
  | '=' { ASSIGN }
  | "==" { EQ }
  | "!=" { NEQ }
  | '<' { LT }
  | "<=" { LTE }
  | '>' { GT }
  | ">=" { GTE }
  | ',' { COMMA }
  | '!' { NOT }
  | "&" { PARALLEL }
  | "and" { AND }
  | "or" { OR }
  | "->" { RARROW }
  | '/' { SLASH }
  | "::" { SCORE_RESOLUTION }
  | ".." { DOTS }
  | ':' { COLON }
  | '.' { DOT }
  | ''' { APOSTROPHE }
  | '^' { OCTAVE_RAISE }
  | '_' { OCTAVE_LOWER }
  | "=>" { MATCHCASE }
(* ------------- Keywords ------------- *)
  | "unit" { UNIT }
  | "bool" { BOOL }
  | "int" { INT }
  | "double" { DOUBLE }
  | "char" { CHAR }
  | "string" { STR }
  | "Struct" { STRUCT }
  | "Enum" { ENUM }
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
  | "pitch" { PITCH }
  | "duration" { DURATION }
  | "Note" { NOTE }
  | "Chord" { CHORD }
  | "Seq" { SEQ }
(* ------------- Literals ------------- *)
  | digit+ as lit { LIT_INT(int_of_string lit) }
  | digit+ as lit ".."  { LIT_INT_DOTS(int_of_string lit) }
  | pitch as lit { LIT_PITCH(lit) }
  | pitch_relative as lit { LIT_PITCH(
    (Core.Std.Char.to_string (Char.chr (((int_of_char lit.[0] - 48)+1) mod 7 + 65)))
    ^ (if lit.[1] = '^' then "5" else "3") ) }
  | ((hasint | hasfrac) hasexp?) | (digit+ hasexp) as lit { LIT_DOUBLE(float_of_string lit) }
  | ''' ( ascii | digit | escape ) ''' as str { LIT_CHAR(get_char str) }
  | '"' (('\\' '"'| [^'"'])* as str) '"' { LIT_STR(Scanf.unescaped str) }
  | (letter) (letter | digit | '_')* '''? as lit { ID(lit) } (* Identifiers should start with letters *)
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
