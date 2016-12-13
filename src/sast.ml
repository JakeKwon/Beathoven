module A = Ast

type expr =
    Id of string * A.datatype
  | LitBool of bool
  | LitInt of int
  | LitDouble of float
  | LitStr of string
  | LitPitch of char * int * int
  | Null
  | Binop of expr * A.binary_operator * expr * A.datatype
  | Uniop of A.unary_operator * expr * A.datatype
  | Assign of expr * expr * A.datatype
  | FuncCall of string * expr list * A.datatype
  | Noexpr

type stmt =
    Block of stmt list
  | Expr of expr * A.datatype
  | If of expr * stmt * stmt
  | While of expr * stmt
  | Return of expr * A.datatype
  | Break
  | Continue
  | VarDecl of A.datatype * string * expr
(*| SFor of sexpr * sexpr * sexpr * sstmt
 *)


type func_decl = {
  fname : string; (* global name *)
  formals : A.bind list;
  returnType : A.datatype;
  body : stmt list;
  (* btmodule  *)
  (* functype *)
  (* TODO?: separate vars from stmt list in analyzer *)
}

type btmodule = {
  mname : string;
  structs: A.struct_decl list; (* global name *)
  funcs : func_decl list;
}

type program = {
  main_module : btmodule; (* still not sure if need main_module *)
  btmodules : btmodule list;
  (* functions : sfunc_decl list; (* All method declarations *) *)
  (* user_type ?? *)
}
