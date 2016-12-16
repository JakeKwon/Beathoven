(*
 * Authors:
 *  - Ruonan Xu
 *)

let default_mname = ".beathoven"
let default_fname = ".main"

type primitive =
    Unit
  | Bool
  | Int
  | Double
  | String

type musictype =
    Pitch

type datatype = Primitive of primitive | Musictype of musictype
              | Structtype of string | Arraytype of datatype

type binary_operator =
    Add | Sub | Mult | Div | Mod | Equal | Neq
  | Less | Leq | Greater | Geq | And | Or

type unary_operator = Neg | Not

type bind = datatype * string
(* type formal = Formal of bind | Many of datatype *)

type struct_decl = {
  sname : string;
  fields : bind list;
}

type expr =
    Id of string
  | StructField of expr * string
  | LitBool of bool
  | LitInt of int
  | LitDouble of float
  | LitStr of string
  | LitPitch of char * int * int (* step * octave * alter *)
  | Null
  | Binop of expr * binary_operator * expr
  | Uniop of unary_operator * expr
  | Assign of expr * expr
  | FuncCall of string * expr list
  | Noexpr
  | LitArray of expr list
  | ArrayIdx of expr * expr
  | ArraySub of expr * expr * expr

type stmt =
    Block of stmt list
  | Expr of expr
  | VarDecl of datatype * string * expr
  | If of expr * stmt * stmt
  | While of expr * stmt
  | Return of expr
  | Break
  | Continue
  | Struct of struct_decl


type func_decl = {
  fname : string;
  formals : bind list;
  returnType : datatype;
  body : stmt list;
}

type btmodule = {
  mname : string;
  (* TODO: usr_type Enum *)
  funcs : func_decl list;
}

type program = btmodule list
