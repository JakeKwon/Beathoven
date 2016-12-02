(* Pretty Print *)

open Sast
(* Ast is opened as module A in Sast. Items in Ast can be accessed with A.* here. *)
open Yojson
(* Ref: https://realworldocaml.org/v1/en/html/handling-json-data.html *)



let string_of_datatype (t : A.datatype) =
  match t with
    Datatype(Int) -> "int"
  | Datatype(Bool) -> "bool"
  | Datatype(Unit) -> "unit"
(* | Primitive(Double) -> "double"
   | Primitive(String) -> "string" *)

let string_of_op (op : A.binary_operator) =
  match op with
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

let string_of_uop (uop : A.unary_operator) =
  match uop with
    Neg -> "-"
  | Not -> "!"

(* let string_of_expr = function
  Id (_,d) -> d
| LitBool(_) -> A.Datatype(Bool)
| LitInt(_) -> A.Datatype(Int)
| LitDouble(_) -> A.Datatype(Double)
| LitStr(_) -> A.Datatype(String)
| Null -> A.Datatype(Unit)
| Binop (_,_,_,d) -> d
| Uniop (_,_,d) -> d
| Assign (_,_,d) -> d
| FuncCall (_,_,d)-> d
| Noexpr -> A.Datatype(Unit)

    Int_Lit(i) -> string_of_int i
  | Boolean_Lit(b) -> if b then "true" else "false"
  | Float_Lit(f) -> string_of_float f
  | String_Lit(s) -> "\"" ^ (String.escaped s) ^ "\""
  | Char_Lit(c) -> Char.escaped c
  | This -> "this"
  | Id(s) -> s
  | Binop(e1, o, e2) -> (string_of_expr e1) ^ " " ^ (string_of_op o) ^ " " ^ (string_of_expr e2)
  | Assign(e1, e2) -> (string_of_expr e1) ^ " = " ^ (string_of_expr e2)
  | Noexpr -> ""
  | ObjAccess(e1, e2) -> (string_of_expr e1) ^ "." ^ (string_of_expr e2)
  | Call(f, el) -> f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | ArrayPrimitive(el) -> "|" ^ (string_of_array_primitive el) ^ "|"
  |   Unop(op, e) -> (string_of_op op) ^ "(" ^ string_of_expr e ^ ")"
  | Null -> "null" *)


(* Print SAST tree representation *)

let tuple_of_datatype d = ("datatype", `String (string_of_datatype d))

let rec json_of_expr expr =
  let (expr_json : Yojson.Basic.json)  =
    match expr with
      Id(s, d) -> `Assoc [("id", `Assoc [("name", `String s); tuple_of_datatype d])]
    | LitBool(b) -> `Assoc [("bool", `Assoc [("val", `Bool b);])]
    | LitInt(i) -> `Assoc [("int", `Assoc [("val", `Int i);])]
    | LitDouble(d) -> `Assoc [("double", `Assoc [("val", `Float d);])]
    | LitStr(s) -> `Assoc [("string", `Assoc [("val", `String s);])]
    | Binop(e1, op, e2, d) -> `Assoc [("binop",
                                       `Assoc [
                                         ("lhs", (json_of_expr e1));
                                         ("op", `String (string_of_op op));
                                         ("rhs", (json_of_expr e2));
                                         tuple_of_datatype d;
                                       ]);]
    | Uniop(op, e, d) -> `Assoc [("uniop",
                                  `Assoc [
                                    ("op", `String (string_of_uop op));
                                    ("operand", (json_of_expr e));
                                    tuple_of_datatype d
                                  ])]
    | Assign(e1, e2, d)  -> `Assoc [("assign",
                                     `Assoc [
                                       ("lhs", (json_of_expr e1));
                                       ("rhs", (json_of_expr e2));
                                       tuple_of_datatype d
                                     ])]
    | FuncCall(f, el, d)-> `Assoc [("funccall",
                                    `Assoc [
                                      ("name", `String f);
                                      ("params", `List (List.map json_of_expr el));
                                      tuple_of_datatype d
                                    ])]
    | Noexpr -> `String "noexpr"
    | Null -> `String "null"
  in expr_json

let rec json_of_stmt stmt =
  let (stmt_json : Yojson.Basic.json) = (* OCaml cannot infer data type as I wish *)
    match stmt with
      Block sl -> `Assoc [("block", `List (List.map json_of_stmt sl))]
    | Expr(e, d) -> `Assoc [("expr", `Assoc [("expr", json_of_expr e); tuple_of_datatype d])]
    | Return(e, d) -> `Assoc [("return", `Assoc [("expr", json_of_expr e); tuple_of_datatype d])]

    | If (e, s1, s2) -> `Assoc [("sif", `Assoc [("cond", json_of_expr e); ("ifbody", json_of_stmt s1)]); ("selse", json_of_stmt s2)]
    (* | For (e1, e2, e3, s) -> `Assoc [("sfor", `Assoc [("init", map_sexpr_to_json e1); ("cond", map_sexpr_to_json e2); ("inc", map_sexpr_to_json e3); ("body", map_sstmt_to_json s)])] *)
    (* | While (e, s) -> `Assoc [("swhile", `Assoc [("cond", map_sexpr_to_json e); ("body", map_sstmt_to_json s)])] *)
    | Break -> `String "sbreak"
    | Continue -> `String "scontinue"

    | VarDecl(d, s, e) -> `Assoc [("vardecl",
                                   `Assoc [tuple_of_datatype d; ("name", `String s); ("val", json_of_expr e)])]
  in stmt_json


let json_of_func (func : func_decl) =
  `Assoc[("func_decl",
          `Assoc[
            ("fname", `String func.fname);
            ("returnType", `String (string_of_datatype func.returnType) );
            (* ("formals", map_formals_to_json sfdecl.sformals); *)
            ("body", `List (List.map json_of_stmt func.body));
          ])]

let json_of_funcs funcs =
  `List(List.map json_of_func funcs)

let json_of_module btmodule =
  `Assoc [("btmodule",
           `Assoc[
             ("mname", `String btmodule.mname);
             ("funcs", json_of_funcs btmodule.funcs);
           ])]

let json_of_module_list btmodules =
  `List(List.map json_of_module btmodules)

let json_of_program program =
  `Assoc [("program",
           `Assoc [
             ("main_module", json_of_module program.main_module);
             ("btmodules", json_of_module_list program.btmodules);
           ])]



(*


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
