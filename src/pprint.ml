(* Pretty Print  *)
open Sast
(* Ast is opened as module A in Sast. Items in Ast can be accessed with A.* here.  *)

(* let string_of_op = function
Add -> "+"
| Sub -> "-"
| Mult -> "*"
| Div -> "/"
| Equal -> "=="
| Neq -> "!="
| Less -> "<"
| Leq -> "<="
| Greater -> ">"
| Geq -> ">="
| And -> "&&"
| Mod -> "%"
| Or -> "||"

let string_of_typ = function
Datatype(Int) -> "int"
| Datatype(Bool) -> "bool"
| Datatype(Unit) -> "void"
(* | Primitive(Double) -> "double"
| Primitive(String) -> "string" *)

let string_of_uop = function
Neg -> "-"
| Not -> "!"

let rec string_of_expr = function
LitInt(l) -> string_of_int l
| LitBool(true) -> "true"
| LitBool(false) -> "false"
| Id(s,_) -> s
| Binop(e1, o, e2,_) ->
string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
| Uniop(o, e, _) -> string_of_uop o ^ string_of_expr e
| Assign(v, e,_) -> string_of_expr v ^ " = " ^ string_of_expr e
| FuncCall(f, el, _) ->
f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
| Noexpr -> ""

let rec string_of_stmt = function
Block(stmts) ->
"{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
| Expr(expr) -> string_of_expr expr ^ ";\n";
| Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
| If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
| If(e, s1, s2) -> "if (" ^ string_of_expr e ^ ")\n" ^
string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
| While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl =
string_of_typ fdecl.returnType ^ " " ^
fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
")\n{\n" ^
String.concat "" (List.map string_of_stmt fdecl.body) ^
"}\n"

let string_of_program (vars, funcs) =
String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
String.concat "\n" (List.map string_of_fdecl funcs) *)
