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
