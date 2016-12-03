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

(*
let struct_types:(string, lltype) Hashtbl.t = Hashtbl.create 10
let struct_field_indexes:(string, int) Hashtbl.t = Hashtbl.create 50
 *)

(* ------------------- Utils ------------------- *)

let lltype_of_datatype (t : A.datatype) =
  match t with
    Datatype(Unit) -> unit_t
  | Datatype(Int) -> i32_t
  | Datatype(Double) -> double_t
  | Datatype(String) -> str_t
  | Datatype(Bool) -> i1_t
  | Musictype(Pitch) ->
    let lltype = L.named_struct_type context "struct.mypitch" in
    let llar = [| i32_t; i1_t
               (* array_type i8_type 10; vector_type i64_type 10  *)
               |] in
    L.struct_set_body lltype llar false; lltype

(* Declare variable and remember its llvalue in local_tbl *)
let allocate typ var_name builder = (* -> () *)
  let alloca = L.build_alloca (lltype_of_datatype typ) var_name builder in
  Hashtbl.add local_tbl var_name alloca;
  alloca

(* Return the value for a variable or formal argument *)
(* duplicate name in formal will be overwritten by local *)
let load_id s builder =
  try
    let alloca = Hashtbl.find local_tbl s in
    L.build_load alloca s builder
  with | Not_found ->
  try Hashtbl.find formal_tbl s
  with Not_found -> raise (Exceptions.VariableNotDefined s)

let lookup_id s t builder =
  try Hashtbl.find local_tbl s
  with | Not_found ->
  try
    let v = Hashtbl.find formal_tbl s in (* formal s found *)
    (* Make a copy of the formal in the local_tbl  *)
    let alloca = allocate t s builder in
    ignore (L.build_store v alloca builder);
    alloca
  with Not_found -> raise (Exceptions.VariableNotDefined s)


(*
  let tmp =
  in
  print_endline ("--------" ^ (L.string_of_llvalue tmp));
  tmp
 *)


let lookup_func fname =
  match (L.lookup_function fname the_module) with
    None -> raise (Exceptions.LLVMFunctionNotFound fname)
  | Some f -> f


let rec codegen_print expr_list builder =
  let (llval_expr_list : L.llvalue list) = List.map (codegen_expr builder) expr_list in
  let llstrfmt =
    let idx = ref (-1) in
    let llval_and_fmt_of_expr expr = (* -> fmt : string *)
      incr idx;
      let print_fmt_of_datatype (t : A.datatype) =
        match t with
          Datatype(Int) -> "%d"
        | Datatype(String) -> "%s"
        | Datatype(Bool) ->
          (* print_endline (L.string_of_llvalue (List.nth llval_expr_list !idx)); *)
          "%d" (* TODO: print "true" or "false" *)
        | Datatype(Double) -> "%lf"
        | _ -> raise (Exceptions.InvalidTypePassedToPrintf)
      in
      print_fmt_of_datatype (Analyzer.get_type_from_expr expr)
    in
    let fmt_list = List.map llval_and_fmt_of_expr expr_list in
    let fmt_str = String.concat " " fmt_list in
    L.build_global_stringptr fmt_str "fmt" builder
  in
  let actuals = Array.of_list (llstrfmt :: llval_expr_list) in
  (* actuals.(0) <- llstrfmt; *)
  L.build_call (lookup_func "printf") actuals "tmp" builder
(*
let zero = const_int i32_t 0 in
let s = build_in_bounds_gep llstrfmt [| zero |] "tmp" llbuilder in
build_call printf (Array.of_list (s :: params)) "tmp" llbuilder
 *)

and codegen_funccall fname el d builder =
  let f = lookup_func fname in
  let (actuals : L.llvalue array) = Array.of_list (List.map (codegen_expr builder) el) in
  match d with
    A.Datatype(A.Unit) -> L.build_call f actuals "" builder
  | _ -> L.build_call f actuals "tmp" builder


and codegen_assign lhs rhs builder =
  let lhs = match lhs with Id(s, t) -> lookup_id s t builder in
  let rhs = codegen_expr builder rhs in
  ignore(L.build_store rhs lhs builder); rhs

and codegen_binop e1 (op : Sast.A.binary_operator) e2 builder =
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

and codegen_unop (op : Sast.A.unary_operator) e1 builder =
  let e1' = codegen_expr builder e1 in
  (match op with
    | Neg -> L.build_neg
    | Not     -> L.build_not) e1' "tmp" builder

(* Construct code for an expression; return its llvalue *)
and codegen_expr builder = function
    Id(s, _) -> load_id s builder
  | LitBool b -> L.const_int i1_t (if b then 1 else 0)
  | LitInt i -> L.const_int i32_t i
  | LitDouble d -> L.const_float double_t d
  | LitStr s -> L.build_global_stringptr s "tmp" builder
(*
  | LitPitch(s,o,a) -> L.build_struct_gep parent_expr field_index field llbuilder in
   llvalue -> int -> string -> llbuilder -> llvalue
 *)
  | Noexpr -> L.const_int i32_t 0
  | Null -> L.const_null i32_t
  | Assign(e1, e2, _) -> codegen_assign e1 e2 builder
  | FuncCall(fname, el, d) ->
    (match fname with
       "printf" -> codegen_print el builder
     | _ -> codegen_funccall fname el d builder )
  | Binop(e1, op, e2, _) -> codegen_binop e1 op e2 builder
  | Uniop(op, e1, _) -> codegen_unop op e1 builder


let rec codegen_stmt builder = function
    Block sl -> List.fold_left codegen_stmt builder sl
  | Expr(e, _) -> ignore(codegen_expr builder e); builder
  | VarDecl(t, s, e) ->
    ignore(allocate t s builder);
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
  let init_params llfunc formals =
    List.iteri ( fun i formal ->
        let n = snd formal in
        let v = L.param llfunc i in
        L.set_value_name n v;
        Hashtbl.add formal_tbl n v
      ) formals
  in
  let llfunc = lookup_func func.fname in
  (* An instance of the IRBuilder class used in generating LLVM instructions *)
  let llbuilder = L.builder_at_end context (L.entry_block llfunc) in
  Hashtbl.clear formal_tbl;
  Hashtbl.clear local_tbl;
  init_params llfunc func.formals;
  ignore (codegen_stmt llbuilder (Block(func.body)));
  (* Finish off the function. *)
  if func.returnType = A.Datatype(A.Unit)
  then ignore(L.build_ret_void llbuilder)
  else ()
(* L.build_ret (L.const_int i32_t 0) llbuilder;  *)


let linker filename =
  (* let llctx = L.global_context () in *)
  let llmem = L.MemoryBuffer.of_file filename in
  let llm = Llvm_bitreader.parse_bitcode context llmem in
  Llvm_linker.link_modules' the_module llm

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
  linker "stdlib.bc";
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
