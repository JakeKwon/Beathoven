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

primitive:
    TYPE_UNIT { Unit }
  | TYPE_INT { Int }
  | TYPE_DOUBLE { Double }
  | TYPE_STR { String }
  | TYPE_BOOL { Bool }

datatype:
    primitive { Datatype($1) }

/* ------------------- Expressions ------------------- */

expr:
    literals            { $1 }
  | MINUS expr { Uniop (Neg, $2) }
  | expr PLUS   expr { Binop($1, Add, $3) }
  | expr MINUS  expr { Binop($1, Sub, $3) }
  | expr TIMES  expr { Binop($1, Mult, $3) }
  | expr DIVIDE expr { Binop($1, Div, $3) }
  | expr MOD    expr { Binop($1, Mod, $3) }
  | expr ASSIGN expr { Assign($1, $3) }
  | ID LPAREN expr_list RPAREN { FuncCall($1, $3)}
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

formal_list: /* bind list */
    /* nothing */ { [] }
  | formal_rev_list { List.rev $1 }

formal_rev_list:
    datatype ID { [($1, $2)] }
  | formal_rev_list COMMA datatype ID { ($3, $4) :: $1 }

expr_list:
    /* nothing */ { [] }
  | expr_rev_list { List.rev $1 }

expr_rev_list:
    expr { [$1] }
  | expr_rev_list COMMA expr { $3 :: $1 }

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
  | LBRACE stmt_list_plus RBRACE { Arr(($2), None) }
  | LBRACK stmt_list_plus RBRACK { ArrMusic(($2)) }
  | typename   BRACES  { Arr([], Some($1)) }
  */

/* ------------------- Statements ------------------- */

stmt:
    expr SEP { Expr($1) }
  | var_decl { $1 }
  | RETURN expr SEP { Return($2) }
  | RETURN SEP { Return(Noexpr) }
  | LBRACE stmt_list RBRACE { Block($2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([Expr(Noexpr)])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  /*
  | FOR LPAREN expr_opt SEP expr_opt SEP expr_opt RPAREN stmt
   { For($3, $5, $7, $9) }
  */
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }
  | BREAK SEP { Break }
  | CONTINUE SEP { Continue }

stmt_list:
  | stmt_rev_list { List.rev $1 }

stmt_rev_list:
    /* nothing */ { [] }
  | stmt_rev_list stmt { $2 :: $1 }
  /*| stmt stmt_rev_list { $1 :: $2 }*/

var_decl:
    datatype ID SEP { VarDecl($1, $2, Noexpr) }
  | datatype ID ASSIGN expr SEP { VarDecl($1, $2, $4) }

/* ------------------- Functions ------------------- */

func_decl:
  FUNC ID LPAREN formal_list RPAREN RARROW datatype LBRACE stmt_list RBRACE
  {
    { fname = $2; formals = $4; returnType = $7; body = $9 }
  }

mbody: /* stmt_rev_list (main_func), func_decl_rev_list */
    /* nothing */ { [], [] }
  | mbody stmt { ($2 :: fst $1), snd $1 }
  | mbody func_decl { fst $1, ($2 :: snd $1) }

mfuncs:
    mbody
  {
    { fname = default_fname; formals = [];
      returnType = Datatype(Unit); body = List.rev (fst $1) } :: (List.rev (snd $1))
  }

/* ------------------- Modules ------------------- */

main_module:
  mfuncs
  {
    { mname = default_mname; funcs = $1 }
  }

btmodule:
  MODULE ID LBRACE mfuncs RBRACE
  {
    { mname = $2; funcs = $4 }
  }

btmodule_list:
    /* nothing */ { [] }
  | btmodule btmodule_list { $1::$2 } /* rev?? */


program:
  main_module EOF { [$1] }
  /*main_module btmodule_list EOF { $1, $2 }*/ /* rev?? */

/*type fname = Constructor | FName of string*/
