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
	expr SEMI { Expr($1) }
| 	RETURN expr SEMI { Return($2) }
|	RETURN SEMI		 { Return(Noexpr) }
| 	LBRACE stmt_list RBRACE { Block(List.rev $2) }
| 	IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([Expr(Noexpr)])) }
| 	IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
| 	FOR LPAREN expr_opt SEMI expr_opt SEMI expr_opt RPAREN stmt
	 { For($3, $5, $7, $9) }
| 	WHILE LPAREN expr RPAREN stmt 	{ While($3, $5) }
|	BREAK SEMI					 	{ Break }
|	CONTINUE SEMI				 	{ Continue }
|   datatype ID SEMI 			 	{ Local($1, $2, Noexpr) }
| 	datatype ID ASSIGN expr SEMI 	{ Local($1, $2, $4) }


