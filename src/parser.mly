%{
 open Core.Std
 open Ast
%}

/* token and type specifications, precedence directives, and other output directives */

/* TODO: declare the list of tokens */
%token PLUS MINUS TIMES DIVIDE EOF
%token ASSIGN
%token <int> LIT_INT

/* TODO: Precedence (low to high) and Associativity */
%right ASSIGN
%left PLUS MINUS
%left TIMES DIVIDE

%start program
%type <Ast.program> program

%%

expr:
    literals { $1 }
  | expr PLUS expr { Binop($1, Add, $3) }
  | expr MINUS expr { Binop($1, Sub, $3) }
  | MINUS expr { Unop (Sub, $2) }
  | expr TIMES expr { Binop($1, Mult, $3) }
  | expr DIVIDE expr { Binop($1, Div, $3) }
  | expr MOD expr { Binop($1, Mod, $3)}
  | expr ASSIGN expr { Assign($1, $3) }
  | expr EQ expr { Binop($1, Equal, $3) }
  | expr NEQ expr { Binop($1, Neq, $3) }
  | expr LT expr { Binop($1, Less, $3) }
  | expr LTE expr { Binop($1, Leq, $3) }
  | expr GT expr { Binop($1, Greater, $3) }
  | expr GTE expr { Binop($1, Geq, $3) }
  | NOT expr { Unop (Not, $2) }
  | expr AND expr { Binop($1, And, $3) }
  | expr OR expr { Binop($1, Or, $3) }
  | LPAREN expr RPAREN { $2 }
/*
  | expr bracket_args RBRACKET { ArrayAccess($1, List.rev $2) }
  | ID LPAREN func_args RPAREN { Call($1, $3) }
*/
