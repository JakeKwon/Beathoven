/*
 * Authors:
 *  - Ruonan Xu
 *  - Jake Kwon
 *  - Sona Roy
 */

%{
  open Ast
%}

/* Token and type specifications */
%token <int> LIT_INT
%token <bool> LIT_BOOL
%token <string> LIT_STR
%token <float> LIT_DOUBLE
%token <string> ID
%token <string> LIT_PITCH
%token <char> LIT_CHAR
%token UNIT BOOL INT CHAR DOUBLE STR
%token STRUCT ENUM
%token PITCH DURATION NOTE CHORD SEQ
%token ASSIGN
%token RETURN SEP EOF
%token LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE
%token PLUS MINUS TIMES DIVIDE MOD
%token EQ NEQ LT LTE GT GTE
%token COLON DOT COMMA
%token NOT AND OR
%token RARROW
%token SLASH PARALLEL
%token OCTAVE_RAISE OCTAVE_LOWER SCORE_RESOLUTION
%token FUNC USING MODULE
%token MATCH MATCHCASE
%token IF ELSE WHILE FOR IN RANGE BREAK CONTINUE

/* Precedence (low to high) and Associativity */
%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left COLON
%left OR
%left AND
%left EQ NEQ LT GT LTE GTE
%left PLUS MINUS
%left TIMES DIVIDE MOD
%nonassoc SLASH
%right NOT
%right RBRACK
%left LBRACK
%left DOT /* right?? */
/*
%left OCTAVE_RAISE OCTAVE_LOWER
*/


/* AST program start */
%start program
%type <Ast.program> program

%%

literal_duration:
  | LIT_INT SLASH LIT_INT { LitDuration($1, $3) }

literal_pitch:
  | LIT_PITCH { LitPitch($1.[0],
    (if (String.length $1 <= 1) then 4 else (int_of_char $1.[1] - int_of_char '0')),
    (if (String.length $1 <= 2) then 0 else if $1.[2] = '#' then 1 else -1) ) }

literal_note:
  /* TODO: LitInt() is not yet supported for pitch */
  | LIT_INT COLON literal_duration { LitNote(LitInt($1), $3) }
  | literal_pitch COLON literal_duration { LitNote($1, $3) }
  | LIT_INT COLON { LitNote(LitInt($1), LitDuration(1, 4)) } /* don't have this in LRM!! */
  | literal_pitch COLON { LitNote($1, LitDuration(1, 4)) } /* don't have this in LRM!! */
  | COLON literal_duration { LitNote(LitPitch('C', 4, 0), $2) }

literal:
  /*| NULL { Null }*/
  | LIT_BOOL { LitBool($1) }
  | LIT_INT { LitInt($1) }
  | LIT_DOUBLE { LitDouble($1) }
  | LIT_STR { LitStr($1) }
  | LIT_CHAR { LitChar($1) }
  /* these are still Primitive() */
  | literal_pitch { $1 }
  | literal_duration { $1 }


primitive:
    UNIT { Unit }
  | INT { Int }
  | DOUBLE { Double }
  | STR { String }
  | BOOL { Bool }
  | CHAR { Char }
  /* primitive music types */
  | PITCH { Pitch }
  | DURATION { Duration }

datatype_nonarray:
    primitive { Primitive($1) }
  | NOTE { Musictype(Note) }
  /*| musictype { Musictype($1) }*/
  | STRUCT ID { Structtype($2) }

datatype:
    datatype_nonarray { $1 }
  | datatype_nonarray LBRACK RBRACK { Arraytype($1) }

/* ------------------- Expressions ------------------- */

ids:
    ID { Id($1) }
  | expr DOT ID { StructField($1, $3) } /* how about struct.struct.f?? */

index_range: /* Python-like array access */
    expr COLON expr { ($1, $3) }
  | COLON expr { (LitInt(0), $2) }
  | expr COLON { ($1, Noexpr) }
  | COLON { (LitInt(0), Noexpr) }

expr_array:
  | LBRACK expr_list RBRACK { LitArray($2) }
  | expr LBRACK index_range RBRACK { ArraySub($1, fst $3, snd $3) }

expr:
  | literal { $1 }
  /* Note that ID can still have whatever type, such as Arraytype and Musictype  */
  | ids { $1 }
  | MINUS expr { Uniop (Neg, $2) }
  | expr PLUS expr { Binop($1, Add, $3) }
  | expr MINUS expr { Binop($1, Sub, $3) }
  | expr TIMES expr { Binop($1, Mult, $3) }
  | expr DIVIDE expr { Binop($1, Div, $3) }
  | expr MOD expr { Binop($1, Mod, $3) }
  | expr EQ expr { Binop($1, Equal, $3) }
  | expr NEQ expr { Binop($1, Neq, $3) }
  | expr LT expr { Binop($1, Less, $3) }
  | expr LTE expr { Binop($1, Leq, $3) }
  | expr GT expr { Binop($1, Greater, $3) }
  | expr GTE expr { Binop($1, Geq, $3) }
  | ID LPAREN expr_with_note_list RPAREN { FuncCall($1, $3)}
  /*
  | NOT expr { Unop (Not, $2) }
  | expr AND expr { Binop($1, And, $3) }
  | expr OR expr { Binop($1, Or, $3) }
*/
  | expr LBRACK expr RBRACK { ArrayIdx($1, $3) } /* ids?? */
  | LPAREN expr RPAREN { $2 }
  | expr ASSIGN expr_array { Assign($1, $3) }
  /* | expr_array { $1 } This has shift/reduce error. Why?? My mind stucks now */

/*TODO: expr_with_array: */

expr_with_note:
  | expr { $1 }
  | literal_note { $1 }
  | expr ASSIGN expr_with_note { Assign($1, $3) }

/* ------------------- Expressions List ------------------- */

expr_with_note_list:
  | { [] }
  | expr_with_note_rev_list { List.rev $1 }

expr_with_note_rev_list:
    expr_with_note { [$1] }
  | expr_with_note_rev_list COMMA expr_with_note { $3 :: $1 }

expr_list:
    /* nothing */ { [] }
  | expr_rev_list { List.rev $1 }

expr_rev_list:
    expr { [$1] }
  | expr_rev_list COMMA expr { $3 :: $1 }


/* ------------------- Statements ------------------- */

stmt:
    expr_with_note SEP { Expr($1) }
  | var_decl { $1 }
  | RETURN expr_with_note SEP { Return($2) }
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
  | datatype ID ASSIGN expr_with_note SEP { VarDecl($1, $2, $4) }
  | datatype ID ASSIGN expr_array SEP { VarDecl($1, $2, $4) }

/* ------------------- Structs ------------------- */

field_list:
  | field_rev_list { List.rev $1 }

field_rev_list:
    datatype ID SEP { [($1, $2)] }
  | field_rev_list datatype ID SEP { ($2, $3) :: $1 }

struct_decl:
  STRUCT ID LBRACE field_list RBRACE
  {
    { sname = $2; fields = $4; }
  }

struct_and_stmt:
    stmt { $1 }
  | struct_decl { Struct($1) }

/* ------------------- Functions ------------------- */

formal_list: /* bind list */
    /* nothing */ { [] }
  | formal_rev_list { List.rev $1 }

formal_rev_list:
    datatype ID { [($1, $2)] }
  | formal_rev_list COMMA datatype ID { ($3, $4) :: $1 }

func_decl:
  FUNC ID LPAREN formal_list RPAREN RARROW datatype LBRACE stmt_list RBRACE
  {
    { fname = $2; formals = $4; returnType = $7; body = $9 }
  }

mfuncs: /* struct_and_stmt_rev_list (main_func), func_decl_rev_list */
    /* nothing */ { [], [] }
  | mfuncs struct_and_stmt { ($2 :: fst $1), snd $1 }
  | mfuncs func_decl { fst $1, ($2 :: snd $1) }

mbody:
    mfuncs
  {
    { fname = default_fname; formals = [];
      returnType = Primitive(Unit); body = List.rev (fst $1) } :: (List.rev (snd $1))
  }

/* ------------------- Modules ------------------- */

include_decl:
  | USING ID SEP { ($2, true) }
  | MODULE ID SEP { ($2, false) }

include_rev_list:
    include_decl { [$1] }
  | include_rev_list include_decl { $2::$1 }

btmodule:
  mbody
  {
    { mname = default_mname; funcs = $1 }
  }

  include_list:
    | include_rev_list  { (beathoven_lib, true) :: (List.rev $1)}

program:
  | btmodule EOF { [$1] }
  | include_list btmodule EOF { [$2] }

/* TODO: right now include_list must be on the top */

/*
p.s. parser is still clean in this version without Musictype(Note).
https://github.com/JakeKwon/Beathoven/blob/68ec6cee97ef888fca6f21b298821769553c513c/src/parser.mly
*/
