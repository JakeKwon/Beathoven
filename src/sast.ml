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


type func_decl = {
  fname : string;
  formals : A.bind list;
  returnType : A.datatype;
  body : stmt list;
  (* btmodule  *)
  (* functype *)
  (* TODO?: separate vars from stmt list *)
}

type btmodule = {
  mname : string;
  main_func : func_decl;
  funcs : func_decl list;
}

type program = {
  main_module : btmodule;
  btmodules : btmodule list;
  (* functions : sfunc_decl list; (* All method declarations *) *)
  (* user_type ?? *)
}

(* Class Declarations |  | Main entry method *)
