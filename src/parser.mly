%{
  open Ast
%}

/* Token and type specifications */
%token <int> LIT_INT
%token <bool> LIT_BOOL
%token <string> LIT_STR
%token <float> LIT_DOUBLE
%token <string> ID
%token NULL TYPE_UNIT TYPE_BOOL TYPE_INT TYPE_DOUBLE TYPE_STR TYPE_STRUCT TYPE_ENUM
%token ASSIGN
%token RETURN SEP EOF
%token LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE
%token PLUS MINUS TIMES DIVIDE MOD
%token EQ NEQ LT LTE GT GTE
%token COLON DOT COMMA
%token NOT PARALLEL AND OR
%token RARROW
%token ARRAY
%token OCTAVE_RAISE OCTAVE_LOWER SCORE_RESOLUTION
%token PITCH DURATION NOTE CHORD SEQ
%token FUNC USING MODULE
%token MATCH MATCHCASE
%token IF ELSE WHILE FOR IN RANGE BREAK CONTINUE

/* Precedence (low to high) and Associativity */
%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left NEQ GTE EQ LTE GT
%left PLUS MINUS
%left TIMES DIVIDE MOD
%left COLON
%left OCTAVE_RAISE OCTAVE_LOWER
%left DOT
%left ARRAY

/* AST program start */
%start program
%type <Ast.program> program

%%

program:
  main_module EOF { $1 }
  /*main_module btmodule_list EOF { $1, $2 }*/ /* rev?? */


btmodule_list:
    /* nothing */ { [] }
  | btmodule btmodule_list { $1::$2 } /* rev?? */

btmodule:
  MODULE ID LBRACE mbody RBRACE
  {
    { mname = $2; funcs = $4 }
  }

main_module:
  mbody
  {
    { mname = "~beathoven"; funcs = $1 }
  }

mbody:
  main_func { [$1] }
  /*main_func func_list { $1, $2 }*/

main_func:
  stmt_list
  {
    { fname = "main"; formals = []; returnType = Primitive(Unit); body = List.rev $1 }
  }
  /* TODO: codegen, rename mname+fname */


var_decl:
    typ ID SEP { VarDecl(Primitive($1), $2, Noexpr) }  /* ?? */
  | typ ID ASSIGN expr SEP { VarDecl(Primitive($1), $2, $4) }

/****
formals, parameters, variables, actuals (reference Dice)
****/

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    typ ID                   { [( Primitive($1),$2)] }
  | formal_list COMMA typ ID { (Primitive($3),$4) :: $1 }

actuals_opt:
    /* nothing */ { [] }
  |   actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  |   actuals_list COMMA expr { $3 :: $1 }

/***
expressions
***/
stmt_list:
    /* nothing */ { [] }
  | stmt_list stmt { $2::$1 }

typ:
    TYPE_UNIT { Unit }
  | TYPE_INT { Int }
  | TYPE_DOUBLE { Double }
  | TYPE_STR { String }
  | TYPE_BOOL { Bool }

literals:
    ID { Id($1) }
  | NULL { Null }
  | LIT_BOOL { LitBool($1) }
  | LIT_INT { LitInt($1) }
  | LIT_DOUBLE { LitDouble($1) }
  | LIT_STR { LitStr($1) }
  /* | lit_array        { $1 } */
  /*
  lit_array:
  | LBRACE stmt_list_plus RBRACE { Arr((List.rev $2), None) }
  | LBRACK stmt_list_plus RBRACK { ArrMusic((List.rev $2)) }
  | typename   BRACES  { Arr([], Some($1)) }
  */

stmt:
    expr SEP { Expr($1) }
  | var_decl { $1 }
  | RETURN expr SEP { Return($2) }
  | RETURN SEP { Return(Noexpr) }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([Expr(Noexpr)])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  /*
  | FOR LPAREN expr_opt SEP expr_opt SEP expr_opt RPAREN stmt
   { For($3, $5, $7, $9) }
  */
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }
  | BREAK SEP { Break }
  | CONTINUE SEP { Continue }

expr:
    literals            { $1 }
  | MINUS expr { Uniop (Neg, $2) }
  | expr PLUS   expr { Binop($1, Add, $3) }
  | expr MINUS  expr { Binop($1, Sub, $3) }
  | expr TIMES  expr { Binop($1, Mult, $3) }
  | expr DIVIDE expr { Binop($1, Div, $3) }
  | expr MOD    expr { Binop($1, Mod, $3) }
  | expr ASSIGN expr { Assign($1, $3) }
  | ID LPAREN actuals_opt RPAREN { FuncCall($1, $3)}
  /*
  | expr EQ expr { Binop($1, Equal, $3) }
  | expr NEQ expr { Binop($1, Neq, $3) }
  | expr LT expr { Binop($1, Less, $3) }
  | expr LTE expr { Binop($1, Leq, $3) }
  | expr GT expr { Binop($1, Greater, $3) }
  | expr GTE expr { Binop($1, Geq, $3) }
  | NOT expr { Unop (Not, $2) }
  | expr AND expr { Binop($1, And, $3) }
  | expr OR expr { Binop($1, Or, $3) }
  | LPAREN expr RPAREN { $2 }*/
/*
  | expr bracket_args RBRACKET { ArrayAccess($1, List.rev $2) }
  | ID LPAREN func_args RPAREN { Call($1, $3) }
*/

/*----------------------------------------*/


fdecl:
   typ ID LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
     { { returnType = Primitive($1);
   fname = $2;
   formals = $4;
   body = List.rev $7 } }

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list var_decl { $2 :: $1 }
