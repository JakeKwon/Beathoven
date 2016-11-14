(* open Analyzer
open Hashtbl
open Llvm.MemoryBuffer
open Llvm_bitreader *)
module L = Llvm (* LLVM VMCore interface library *)
open Ast
(* open Log *)

module StringMap = Map.Make(String)

exception Error of string

let context = L.global_context () (* global data container *)
let the_module = L.create_module context "Beathoven Codegen" (* container *)
(* let builder = L.builder context *)
let double_t = L.double_type context
and i64_t = L.i64_type context
and i32_t = L.i32_type context
and i8_t = L.i8_type context
and i1_t = L.i1_type context
and unit_t = L.void_type context
let str_t = L.pointer_type i8_t
(* let p_str_t = L.pointer_type str_t  *)
let global_vars:(string, L.llvalue) Hashtbl.t = Hashtbl.create 50
(* cannot use map since StringMap.add returns a new map.  *)

let lltype_of_datatype = function
    Primitive(Unit) -> unit_t
  | Primitive(Int) -> i32_t
  | Primitive(Double) -> double_t
  | Primitive(String) -> str_t
  | Primitive(Bool) -> i1_t

(* Declare variable; remember its llvalue in a map; returns () *)
let allocate typ var_name builder =
  let alloca = L.build_alloca (lltype_of_datatype typ) var_name builder in
  Hashtbl.add global_vars var_name alloca

(* Return the value for a variable or formal argument *)
let lookup s = try Hashtbl.find global_vars s
  with Not_found -> raise (Exceptions.VariableNotDefined s)

and lookup_func fname =
  match (L.lookup_function fname the_module) with
    None -> raise (Exceptions.LLVMFunctionNotFound fname)
  | Some f -> f


let rec codegen_print expr_list builder =
  let lit_list = List.map (codegen_expr builder) expr_list in
  let printtype_of_expr = function
    (* str_t -> "%s" *)
  (* | double_t -> "%lf" *)
  | i32_t -> "%d"
  | i1_t -> "%s" (* ?? *)
  (* TODO: other types *)
  in
  let type_list = List.map printtype_of_expr lit_list in
  let fmt_str = String.concat " " type_list in
  let format_str = L.build_global_stringptr fmt_str "fmt" builder in
  let actuals = Array.of_list (format_str :: lit_list) in
  L.build_call (lookup_func "printf") actuals "tmp" builder

and codegen_assign lhs rhs builder =
  let lhs = match lhs with Id s -> lookup s in
  let rhs = codegen_expr builder rhs in
  ignore(L.build_store rhs lhs builder); rhs

(* Construct code for an expression; return its llvalue *)
and codegen_expr builder = function
    Id s -> L.build_load (lookup s) s builder
  | LitBool b -> L.const_int i1_t (if b then 1 else 0)
  | LitInt i -> L.const_int i32_t i
  | LitDouble d -> L.const_float double_t d
  | LitStr s -> L.build_global_stringptr s "tmp" builder
  | Noexpr -> L.const_int i32_t 0
  | Null -> L.const_null i32_t
  | Assign (e1, e2) -> codegen_assign e1 e2 builder
  | FuncCall (f, el) ->
    (match f with
      "print" -> codegen_print el builder
    | _ -> raise (Exceptions.LLVMFunctionNotFound f)) (* not implemented *)
  | Binop (e1, op, e2) ->
    let e1' = codegen_expr builder e1
    and e2' = codegen_expr builder e2 in
    (match op with
      Add -> L.build_add
    | Sub -> L.build_sub
    | Mult -> L.build_mul
    | Div -> L.build_sdiv
    | Equal -> L.build_icmp L.Icmp.Eq
    | Neq -> L.build_icmp L.Icmp.Ne
    | Less -> L.build_icmp L.Icmp.Slt
    | Leq -> L.build_icmp L.Icmp.Sle
    | Greater -> L.build_icmp L.Icmp.Sgt
    | Geq -> L.build_icmp L.Icmp.Sge
    | And -> L.build_and
    | Mod  -> L.build_srem
    | Or -> L.build_or
    ) e1' e2' "tmp" builder

let rec codegen_stmt builder = function (*rec??*)
    Block sl -> List.fold_left codegen_stmt builder sl
  | Expr e -> ignore(codegen_expr builder e); builder
  | VarDecl (t, s, e) ->
    allocate t s builder;
    if e <> Noexpr then ignore(codegen_assign (Id(s)) e builder);
    builder

let codegen_builtin_funcs () =
  (* Declare printf(), which the print built-in function will call *)
  let printf_t = L.var_arg_function_type i32_t [| str_t |] in
  let _ = L.declare_function "printf" printf_t the_module in
  ()

let codegen_def_func func =
  let formals = List.map (fun (t, _) -> lltype_of_datatype t) func.formals in
  let func_t = L.function_type (lltype_of_datatype func.returnType) (Array.of_list formals) in
  ignore(L.define_function func.fname func_t the_module) (* llfunc *)

let codegen_func func =
  (* Hashtbl.clear named_values;
  Hashtbl.clear named_params;
  let _ = init_params f func.formals in *)
  let llfunc = lookup_func func.fname in
  (* An instance of the IRBuilder class used in generating LLVM instructions *)
  let llbuilder = L.builder_at_end context (L.entry_block llfunc) in
  let _ = codegen_stmt llbuilder (Block(func.body)) in
  (* Finish off the function. *)
  (* L.build_ret (L.const_int i32_t 0) llbuilder;  *)
  if func.returnType = Primitive(Unit) then ignore(L.build_ret_void llbuilder)
  else ()

let codegen_main (btmodule) =
  codegen_builtin_funcs ();
  List.iter codegen_def_func btmodule.funcs;
  List.iter codegen_func btmodule.funcs;

  (* Declare each global variable; remember its value in map global_vars *)
  (* let _ =
    let add_global_var (datatype, var_name) =
      ignore (allocate datatype var_name llbuilder) in
    List.iter add_global_var globals
  in *)
  the_module


(* Batteries  *)

(* Invoke "f builder" if the current block doesn't already
   have a terminal (e.g., a branch). *)
(* let add_terminal builder f =
  match L.block_terminator (L.insertion_block builder) with
    Some _ -> ()
  | None -> ignore (f builder) in *)

(*
Code generation: translate takes a semantically checked AST and produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial
  http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:
  http://llvm.moe/
  http://llvm.moe/ocaml/
*)
