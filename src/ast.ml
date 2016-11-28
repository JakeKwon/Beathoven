let default_mname = "~beathoven"
let default_fname = "~main"

type primi =
    Unit
  | Int
  | Double
  | String
  | Bool

type binary_operator =
    Add | Sub | Mult | Div | Equal | Neq
  | Less | Leq | Greater | Geq | And | Mod | Or

type unary_operator = Neg | Not

(* Primitive(primi) *)
type datatype = Datatype of primi

type bind = datatype * string
(* type formal = Formal of bind | Many of datatype *)


type expr =
    Id of string
  | LitBool of bool
  | LitInt of int
  | LitDouble of float
  | LitStr of string
  | Null
  | Binop of expr * binary_operator * expr
  | Uniop of unary_operator * expr
  | Assign of expr * expr
  | FuncCall of string * expr list
  | Noexpr
  (* LitChar, Array, ... *)

type stmt =
    Block of stmt list
  | Expr of expr
  | If of expr * stmt * stmt
  | While of expr * stmt
  | Return of expr
  | Break
  | Continue
  | VarDecl of datatype * string * expr


type func_decl = {
  fname : string;
  formals : bind list;
  returnType : datatype;
  body : stmt list;
}

type btmodule = {
  mname : string;
  (* TODO: usr_type Struct, Enum *)
  funcs : func_decl list;
}

type program = btmodule list
