module A = Ast

type expr =
	Id of string * A.datatype
	| LitBool of bool
	| LitInt of int
	| LitDouble of float
	| LitStr of string
	| Null
	| Binop of expr * A.binary_operator * expr * A.datatype
	| Uniop of A.unary_operator * expr * A.datatype
	| Assign of expr * expr * A.datatype
	| FuncCall of string * expr list (* datatype at end? *)
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

(* corresponds to ast lines 46-60*)
type func_decl = {
	sfname : string;
	sformals : bind list;
	sreturnType : datatype;
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