(*
open Analyzer
open Llvm.MemoryBuffer
open Llvm_bitreader
open Log
*)
module L = Llvm (* LLVM VMCore interface library *)
open Sast

module StringMap = Map.Make(String)

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
let local_tbl:(string, L.llvalue) Hashtbl.t = Hashtbl.create 50
let formal_tbl:(string, L.llvalue) Hashtbl.t = Hashtbl.create 10


(* ------------------- Utils ------------------- *)

let lltype_of_datatype (t : A.datatype) =
  match t with
    Datatype(Unit) -> unit_t
  | Datatype(Int) -> i32_t
  | Datatype(Double) -> double_t
  | Datatype(String) -> str_t
  | Datatype(Bool) -> i1_t

(* Declare variable and remember its llvalue in local_tbl *)
let allocate typ var_name builder = (* -> () *)
  let alloca = L.build_alloca (lltype_of_datatype typ) var_name builder in
  Hashtbl.add local_tbl var_name alloca

(* Return the value for a variable or formal argument *)
let lookup s = try Hashtbl.find local_tbl s
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
  let lhs = match lhs with Id(s, _) -> lookup s in
  let rhs = codegen_expr builder rhs in
  ignore(L.build_store rhs lhs builder); rhs

and codegen_binop e1 (op : A.binary_operator) e2 builder =
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

(* Construct code for an expression; return its llvalue *)
and codegen_expr builder = function
    Id(s, _) -> L.build_load (lookup s) s builder
  | LitBool b -> L.const_int i1_t (if b then 1 else 0)
  | LitInt i -> L.const_int i32_t i
  | LitDouble d -> L.const_float double_t d
  | LitStr s -> L.build_global_stringptr s "tmp" builder
  | Noexpr -> L.const_int i32_t 0
  | Null -> L.const_null i32_t
  | Assign(e1, e2, _) -> codegen_assign e1 e2 builder
  | FuncCall(f, el, _) ->
    (match f with
       "printf" -> codegen_print el builder
     | _ -> raise (Exceptions.LLVMFunctionNotFound f)) (* not implemented *)
  | Binop(e1, op, e2, _) -> codegen_binop e1 op e2 builder


let rec codegen_stmt builder = function
    Block sl -> List.fold_left codegen_stmt builder sl
  | Expr(e, _) -> ignore(codegen_expr builder e); builder
  | VarDecl(t, s, e) ->
    allocate t s builder;
    if e <> Noexpr then ignore(codegen_assign (Id(s, t)) e builder);
    builder


let codegen_builtin_funcs () =
  (* Declare printf(), which the print built-in function will call *)
  let printf_t = L.var_arg_function_type i32_t [| str_t |] in
  let _ = L.declare_function "printf" printf_t the_module in
  ()

let codegen_def_func func =
  let formals_lltype = List.map (fun (t, _) -> lltype_of_datatype t) func.formals in
  let func_t = L.function_type (lltype_of_datatype func.returnType) (Array.of_list formals_lltype) in
  ignore(L.define_function func.fname func_t the_module) (* llfunc *)

let codegen_func func =
(*
  Hashtbl.clear named_values;
  Hashtbl.clear named_params;
  let _ = init_params f func.formals in
 *)
  let llfunc = lookup_func func.fname in
  (* An instance of the IRBuilder class used in generating LLVM instructions *)
  let llbuilder = L.builder_at_end context (L.entry_block llfunc) in
  let _ = codegen_stmt llbuilder (Block(func.body)) in
  (* Finish off the function. *)
  if func.returnType = Datatype(Unit)
  then ignore(L.build_ret_void llbuilder)
  else ()
(* L.build_ret (L.const_int i32_t 0) llbuilder;  *)


let codegen_program program =
  (* maybe we don't need a separate main_module *)
  let btmodules = program.main_module :: program.btmodules in
  let helper_def_func btmodule =
    List.iter codegen_def_func btmodule.funcs
  in
  let helper_func btmodule =
    List.iter codegen_func btmodule.funcs
  in
  codegen_builtin_funcs ();
  List.iter helper_def_func btmodules;
  List.iter helper_func btmodules; (* main ?? *)
  the_module



(* Declare each global variable; remember its value in map local_tbl *)
(* let _ =
   let add_global_var (datatype, var_name) =
    ignore (allocate datatype var_name llbuilder) in
   List.iter add_global_var globals
   in *)


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
