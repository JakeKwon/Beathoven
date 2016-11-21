(* open Ast *)
module A = Ast

type expr =
    Id of string * A.datatype
  | LitBool of bool
  | LitInt of int
  | LitDouble of float
  | LitStr of string
  | Null
  | Binop of expr * A.binary_operator * expr * A.primi
  | Uniop of A.unary_operator * expr * A.primi
  | Assign of expr * expr * A.primi
  | FuncCall of string * expr list * A.primi (* datatype at end? *)
  | Noexpr

 (*
	| 	SChar_Lit of char
	| 	SBinop of sexpr * op * sexpr * datatype
	| 	SAssign of sexpr * sexpr * datatype
	| 	SCall of string * sexpr list * datatype * int
	|  	SUnop of op * sexpr * datatype
*)

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
	| 	SFor of sexpr * sexpr * sexpr * sstmt
	|   SLocal of datatype * string * sexpr *)


type bind = A.datatype * string


(* corresponds to ast lines 46-60*)
type func_decl = {
  sfname : string;
  sformals : bind list;
  returnType : A.datatype;
  body : stmt list;
}

type btmodule = {
  mname : string;
  funcs : func_decl list;
}


(* Class Declarations | All method declarations | Main entry method *)
(*
type sprogram =  {
	classes : sclass_decl list;
  (* main module *)
	functions : sfunc_decl list;
	main : sfunc_decl;
  (* user_type ?? *)
}
 *)
