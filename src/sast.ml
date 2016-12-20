(*
 * Authors:
 *  - Ruonan Xu
 *  - Jake Kwon
 *  - Sona Roy
 *  - Eunice Kokor
 *)

module A = Ast

type expr =
    Id of string * A.datatype
  | StructField of expr * string * A.datatype (* Id * Id * datatype *)
  | LitBool of bool
  | LitInt of int
  | LitChar of char
  | LitDouble of float
  | LitStr of string
  | LitPitch of char * int * int
  | LitDuration of int * int
  | LitNote of expr * expr
  | Null
  | Binop of expr * A.binary_operator * expr * A.datatype
  | Uniop of A.unary_operator * expr * A.datatype
  | Assign of expr * expr * A.datatype
  | FuncCall of string * expr list * A.datatype
  | Noexpr
  | LitArray of expr list * A.datatype (* element type *)
  | ArrayConcat of expr list * A.datatype (* type of array *)
  | ArrayIdx of expr * expr * A.datatype
  | ArraySub of expr * expr * expr * A.datatype

type stmt =
    Block of stmt list
  | Expr of expr * A.datatype
  | If of expr * stmt * stmt
  | While of expr * stmt
  | For of expr * expr * expr * stmt
  | Return of expr * A.datatype
  | Break
  | Continue
  | VarDecl of A.datatype * string * expr


type func_decl = {
  fname : string;
  formals : A.bind list;
  returnType : A.datatype;
  body : stmt list;
  (* TODO: separate vars from stmt list in analyzer or parser ?? *)
}

type btmodule = {
  mname : string;
  structs: A.struct_decl list; (* global name *)
  funcs : func_decl list; (* global name *)
}

type program = {
  btmodules : btmodule list;
}
