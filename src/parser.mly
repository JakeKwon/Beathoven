%{
  open Core.Std
  open Ast
%}

/* token and type specifications, precedence directives, and other output directives */

/* Token List */
%token <int> LIT_INT
%token <bool> LIT_BOOL
%token <string> LIT_STRING
%token <float> LIT_DOUBLE
%token ASSIGN
%token SEP
%token LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE
%token PLUS MINUS TIMES DIVIDE MOD ASSIGN
%token EQ NEQ LT LTE GT GTE
%token COLON DOT COMMA
%token NOT PARALLEL AND OR 
%token RARROW 
%token ARRAY
%token OCTAVE_RAISE OCTAVE_LOWER SCORE_RESOLUTION
%token MATCHCASE

/* Precedence (low to high) and Associativity */
%right ASSIGN
%left NEQ GTE EQ LTE GT GT
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

literals:
  INT_LITERAL      		{ Int_Lit($1) }
| FLOAT_LITERAL    		{ Float_Lit($1) }
| TRUE			   		{ Boolean_Lit(true) }
| FALSE			   		{ Boolean_Lit(false) }
| STRING_LITERAL   		{ String_Lit($1) }  
| CHAR_LITERAL			{ Char_Lit($1) }
| THIS 			   		{ This }
| ID 			   		{ Id($1) }	
| NULL				    { Null }
| BAR array_prim BAR 	{ ArrayPrimitive($2) }
| LIT_BOOL         { LitBool($1) }
| LIT_INT { LitInt($1) }
| LIT_DOUBLE { LitDouble($1) }
| LIT_FLOAT        { LitFloat($1) }
| LIT_STR          { LitStr($1) }
| TILDE            { StructInit("chord", []) }
| lit_array        { $1 }

lit_array:
| LBRACE stmt_list_plus RBRACE { Arr((List.rev $2), None) }
| LBRACK stmt_list_plus RBRACK { ArrMusic((List.rev $2)) }
| typename   BRACES  { Arr([], Some($1)) }

stmt:
	expr SEP { Expr($1) }
| 	RETURN expr SEP { Return($2) }
|	RETURN SEP		 { Return(Noexpr) }
| 	LBRACE stmt_list RBRACE { Block(List.rev $2) }
| 	IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([Expr(Noexpr)])) }
| 	IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
| 	FOR LPAREN expr_opt SEP expr_opt SEP expr_opt RPAREN stmt
	 { For($3, $5, $7, $9) }
| 	WHILE LPAREN expr RPAREN stmt 	{ While($3, $5) }
|	BREAK SEP					 	{ Break }
|	CONTINUE SEP				 	{ Continue }
|   datatype ID SEP 			 	{ Local($1, $2, Noexpr) }
| 	datatype ID ASSIGN expr SEP 	{ Local($1, $2, $4) }


