(*
 * Authors:
 *  - Ruonan Xu
 *)

module A = Ast

type expr =
    Id of string * A.datatype
  | StructField of expr * expr * A.datatype (* Id * Id * datatype *)
  | LitBool of bool
  | LitInt of int
  | LitDouble of float
  | LitStr of string
  | LitPitch of char * int * int
  | LitDuration of int * int
  | Null
  | Binop of expr * A.binary_operator * expr * A.datatype
  | Uniop of A.unary_operator * expr * A.datatype
  | Assign of expr * expr * A.datatype
  | FuncCall of string * expr list * A.datatype
  | Noexpr
  | LitArray of expr list * A.datatype (* element type *)
  | ArrayIdx of expr * expr * A.datatype
  | ArraySub of expr * expr * expr * A.datatype

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
  fname : string;
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
  funcs : func_decl list; (* global name *)
}

type program = {
  btmodules : btmodule list;
}
