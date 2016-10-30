type typ =
  | Unit
  | Int
  | Float
  | String
  | Bool

type bind = typ * string

(* Reference:
https://github.com/el2724/note-hashtag/blob/master/src/ast.ml
https://github.com/DavidWatkins/Dice/blob/master/src/ast.ml
 *)

type binary_operator =
    Add | Sub | Mult | Div | Equal | Neq
  | Less | Leq | Greater | Geq | And | Mod | Or
type unary_operator = Neg

type datatype = Datatype of typ
type formal = Formal of datatype * string | Many of datatype

type expr =
  | LitBool of bool
  | LitInt of int
  | LitDouble of float
  | LitStr of string
  | Binop of expr * binary_operator * expr
  | Uniop of unary_operator * expr
  | Assign of string * expr
  | FuncCall of string * expr list
  | Noexpr 
  | Local of datatype * string * expr

type stmt =
    Block of stmt list
  | Expr of expr
  | If of expr * stmt * stmt
  | While of expr * stmt
  | Return of expr

type func_decl = {
  fname : string;
  formals : bind list;
  returnType : typ;
  locals : bind list;
  body : stmt list;
}

type program = bind list * func_decl list * stmt list
