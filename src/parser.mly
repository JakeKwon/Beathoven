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
