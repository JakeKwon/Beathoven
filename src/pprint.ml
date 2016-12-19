(*
 * Authors:
 *  - Ruonan Xu
 *  - Jake Kwon
 *)

(*
Pretty Print
*)

open Sast
(* Ast is opened as module A in Sast. Items in Ast can be accessed with A.* here. *)
open Yojson
(* Ref: https://realworldocaml.org/v1/en/html/handling-json-data.html *)

let rec string_of_datatype (t : A.datatype) =
  match t with
  | Primitive(Unit) -> "unit"
  | Primitive(Bool) -> "bool"
  | Primitive(Int) -> "int"
  | Primitive(Double) -> "double"
  | Primitive(String) -> "string"
  | Primitive(Char) -> "char"
  | Primitive(Pitch) -> "pitch"
  | Primitive(Duration) -> "duration"
  | Musictype(Note) -> "Note"
  | Musictype(Seq) -> "Seq"
  | Structtype(s) -> "Struct_" ^ s
  | Arraytype(d) -> "Array_" ^ (string_of_datatype d)
(* TODO J: other datatypes  *)

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


(* Print SAST tree representation *)

let tuple_of_datatype d = ("datatype", `String (string_of_datatype d))

let rec json_of_expr expr =
  let (expr_json : Yojson.Basic.json)  =
    match expr with
      Id(s, d) -> `Assoc [("id", `Assoc [("name", `String s); tuple_of_datatype d])]
    | StructField(e1, e2, d) -> `Assoc [("StructField",
                                         `Assoc [("struct", (json_of_expr e1));
                                                 ("field", `String e2);
                                                 tuple_of_datatype d;
                                                ])]
    | LitBool(b) -> `Assoc [("bool", `Bool b)]
    | LitInt(i) -> `Assoc [("int", `Int i)]
    | LitDouble(d) -> `Assoc [("double", `Float d)]
    | LitStr(s) -> `Assoc [("string", `String s)]
    | LitChar(c) -> `Assoc [("char", `String (Core.Std.Char.to_string c))]
    | LitPitch(k, o, a) ->
      let p = (Core.Std.Char.to_string k) ^ (string_of_int o) ^ "_" ^ (string_of_int a) in
      `Assoc [("pitch", `String p)]
    | LitDuration(a, b) ->
      `Assoc [("duration", `String ((string_of_int a) ^ "/" ^ (string_of_int b)))]
    | LitNote(p, d) -> `Assoc [("Note",
                                `Assoc [("pitch", (json_of_expr p));
                                        ("duration", (json_of_expr d));
                                       ])]
    | Binop(e1, op, e2, d) -> `Assoc [("binop",
                                       `Assoc [("lhs", (json_of_expr e1));
                                               ("op", `String (string_of_op op));
                                               ("rhs", (json_of_expr e2));
                                               tuple_of_datatype d;
                                              ]);]
    | Uniop(op, e, d) -> `Assoc [("uniop",
                                  `Assoc [("op", `String (string_of_uop op));
                                          ("operand", (json_of_expr e));
                                          tuple_of_datatype d
                                         ])]
    | Assign(e1, e2, d)  -> `Assoc [("assign",
                                     `Assoc [("lhs", (json_of_expr e1));
                                             ("rhs", (json_of_expr e2));
                                             tuple_of_datatype d
                                            ])]
    | FuncCall(f, el, d)-> `Assoc [("funccall",
                                    `Assoc [("name", `String f);
                                            ("params", `List (List.map json_of_expr el));
                                            tuple_of_datatype d
                                           ])]
    | Noexpr -> `String "noexpr"
    | Null -> `String "null"
    | LitSeq(el) -> `Assoc [("Seq", `List (List.map json_of_expr el))]
    | LitArray(el, d) -> `Assoc [("Array",
                                  `Assoc [("elements", `List (List.map json_of_expr el));
                                          tuple_of_datatype d
                                         ])]
    | ArrayIdx(a, idx, d) -> `Assoc [("ArrayEle",
                                      `Assoc [("Array", json_of_expr a);
                                              ("Idx", json_of_expr idx);
                                              tuple_of_datatype d
                                             ])]
    | ArraySub(a, idx1, idx2, d) -> `Assoc [("ArraySub",
                                             `Assoc [("Array", json_of_expr a);
                                                     ("Idx1", json_of_expr idx1);
                                                     ("Idx2", json_of_expr idx2);
                                                     tuple_of_datatype d
                                                    ])]
  in expr_json

let rec json_of_stmt stmt =
  let (stmt_json : Yojson.Basic.json) = (* OCaml cannot infer data type as I wish *)
    match stmt with
      Block sl -> `Assoc [("block", `List (List.map json_of_stmt sl))]
    | Expr(e, d) -> `Assoc [("stmt_expr", `Assoc [("expr", json_of_expr e); tuple_of_datatype d])]
    | Return(e, d) -> `Assoc [("return", `Assoc [("expr", json_of_expr e); tuple_of_datatype d])]
    | If (e, s1, s2) -> `Assoc [("if", `Assoc [("cond", json_of_expr e); ("then", json_of_stmt s1)]); ("else", json_of_stmt s2)]
    | For (e1, e2, e3, s) -> `Assoc [("for",
                                      `Assoc [("init", json_of_expr e1);
                                              ("cond", json_of_expr e2);
                                              ("next", json_of_expr e3);
                                              ("body", json_of_stmt s)])]
    | While (e, s) -> `Assoc [("while", `Assoc [("cond", json_of_expr e); ("body", json_of_stmt s)])]
    | Break -> `String "break"
    | Continue -> `String "continue"
    | VarDecl(d, s, e) -> `Assoc [("vardecl",
                                   `Assoc [tuple_of_datatype d; ("name", `String s); ("val", json_of_expr e)])]
  in stmt_json

let json_of_bind_list bind_list =
  `List (List.map
           (function (d, s) -> `Assoc [("name", `String s); tuple_of_datatype d;])
           bind_list)

let json_of_func (func : func_decl) =
  `Assoc[("func_decl",
          `Assoc[("fname", `String func.fname);
                 ("returnType", `String (string_of_datatype func.returnType) );
                 ("formals", json_of_bind_list func.formals);
                 ("body", `List (List.map json_of_stmt func.body));
                ])]

let json_of_funcs funcs =
  `List(List.map json_of_func funcs)

let json_of_struct (s : A.struct_decl) =
  `Assoc[("struct_decl",
          `Assoc[("sname", `String s.sname);
                 ("fields", json_of_bind_list s.fields);
                ])]

let json_of_structs structs =
  `List(List.map json_of_struct structs)

let json_of_module btmodule =
  `Assoc [("btmodule",
           `Assoc[("mname", `String btmodule.mname);
                  ("structs", json_of_structs btmodule.structs);
                  ("funcs", json_of_funcs btmodule.funcs);
                 ])]

let json_of_module_list btmodules =
  `List(List.map json_of_module btmodules)

let json_of_program program =
  `Assoc [("program",
           `Assoc [("btmodules", json_of_module_list program.btmodules);])]


(*
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
