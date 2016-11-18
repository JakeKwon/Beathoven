open Ast

type sexpr =
	SId of string * datatype
	| SLitBool of bool
	| SLitInt of int
	| SLitDouble of float
	| SLitStr of string
	| SNull
	| SBinop of sexpr * binary_operator * sexpr * datatype
	| SUniop of unary_operator * sexpr * datatype
	| SAssign of sexpr * sexpr * datatype
	| SFuncCall of string * sexpr list (* datatype at end? *)
	| SNoexpr

	(* 
	| 	SInt_Lit of int
	| 	SBoolean_Lit of bool
	| 	SFloat_Lit of float
	| 	SString_Lit of string
	| 	SChar_Lit of char
	| 	SBinop of sexpr * op * sexpr * datatype
	| 	SAssign of sexpr * sexpr * datatype
	| 	SNoexpr
	| 	SArrayCreate of datatype * sexpr list * datatype
	| 	SArrayAccess of sexpr * sexpr list * datatype
	| 	SObjAccess of sexpr * sexpr * datatype
	| 	SCall of string * sexpr list * datatype * int
	|   SObjectCreate of string * sexpr list * datatype
	| 	SArrayPrimitive of sexpr list * datatype
	|  	SUnop of op * sexpr * datatype
	| 	SNull
	| 	SDelete of sexpr *)

type sstmt =
	SBlock of sstmt list
	| SExpr of expr * datatype
	| SIf of sexpr * sstmt * sstmt
	| SWhile of sexpr * sstmt
	| SReturn of sexpr * datatype
	| SBreak
	| SContinue
	| SVarDecl of datatype * string * sexpr

(* 
	SBlock of sstmt list
	| 	SExpr of sexpr * datatype
	| 	SReturn of sexpr  * datatype
	| 	SIf of sexpr * sstmt * sstmt
	| 	SFor of sexpr * sexpr * sexpr * sstmt
	| 	SWhile of sexpr * sstmt
	|  	SBreak
	|   SContinue
	|   SLocal of datatype * string * sexpr *)

(* corresponds to ast lines 46-60*)
type sfunc_decl = {
	sfname : string;
	sformals : bind list;
	sreturnType : datatype;
	sbody : sstmt list;
}

(* 
type sfunc_decl = {
	sfname : fname;
	sreturnType : datatype;
	sformals : formal list;
	sbody : sstmt list;
	func_type : func_type;
	source : string;
} *)