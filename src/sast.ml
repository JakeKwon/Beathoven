module A = Ast

type expr =
	Id of string * A.primi
	| LitBool of bool
	| LitInt of int
	| LitDouble of float
	| LitStr of string
	| Null
	| Binop of A.expr * A.binary_operator * A.expr * A.primi
	| Uniop of A.unary_operator * A.expr * A.primi
	| Assign of A.expr * A.expr * A.primi
	| FuncCall of string * A.expr list * A.primi (* datatype at end? *)
	| Noexpr

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

type stmt =
	Block of stmt list
	| Expr of expr * A.datatype
	| If of expr * stmt * stmt
	| While of expr * stmt
	| Return of expr * A.datatype
	| Break
	| Continue
	| VarDecl of A.datatype * string * expr

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


type bind = A.datatype * string


(* corresponds to ast lines 46-60*)
type func_decl = {
	sfname : string;
	sformals : bind list;
	sreturnType : A.datatype;
	sbody : stmt list;
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

type btmodule = {
	mname : string;
	funcs : func_decl list;
}