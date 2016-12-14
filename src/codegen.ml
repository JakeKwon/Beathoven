
(*
Code generation: translate takes a semantically checked AST and produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial
  http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:
  http://llvm.moe/
  http://llvm.moe/ocaml/
*)

(*
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

let local_tbl:(string, L.llvalue) Hashtbl.t = Hashtbl.create 50
let formal_tbl:(string, L.llvalue) Hashtbl.t = Hashtbl.create 10

let struct_tbl:(string, L.lltype) Hashtbl.t = Hashtbl.create 10
let struct_field_indexes:(string, int) Hashtbl.t = Hashtbl.create 50
let is_struct_packed = false

let literal_tbl:(string, L.llvalue) Hashtbl.t = Hashtbl.create 50

(*
let global_vars:(string, L.llvalue) Hashtbl.t = Hashtbl.create 50
*)

(* ------------------- Utils ------------------- *)

let lookup_struct sname =
  try Hashtbl.find struct_tbl sname
  with | Not_found -> raise(Exceptions.UndefinedStructType sname)

let lookup_func fname =
  match (L.lookup_function fname the_module) with
    None -> raise (Exceptions.LLVMFunctionNotFound fname)
  | Some f -> f

let lltype_of_datatype (t : A.datatype) =
  match t with
    Datatype(Unit) -> unit_t
  | Datatype(Int) -> i32_t
  | Datatype(Double) -> double_t
  | Datatype(String) -> str_t
  | Datatype(Bool) -> i1_t
  | Structtype(s) -> L.pointer_type (lookup_struct s)
  | Musictype(Pitch) -> L.pointer_type (lookup_struct "pitch")
    (*
    | 	Arraytype(t, i) -> get_ptr_type (Arraytype(t, (i)))
    | 	d -> raise(Exceptions.InvalidStructType (Utils.string_of_datatype d))
 *)

let lltype_of_bind_list bind_list =
  List.map (fun (t, _) -> lltype_of_datatype t) bind_list

(* Declare variable and remember its llvalue in local_tbl *)
let codegen_allocate (typ : A.datatype) var_name builder = (* -> () *)
  let t = match typ with
      Structtype(s) -> lookup_struct s
    | Musictype(Pitch) -> lookup_struct "pitch"
    | _ -> lltype_of_datatype typ
  in
  let alloca = L.build_alloca t var_name builder in
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

let lookup_id id builder =
  match id with Id(s, d) ->
  try Hashtbl.find local_tbl s
  with | Not_found ->
  try
    let v = Hashtbl.find formal_tbl s in (* formal s found *)
    (* Make a copy of the formal in the local_tbl  *)
    let alloca = codegen_allocate d s builder in
    ignore (L.build_store v alloca builder);
    alloca
  with Not_found -> raise (Exceptions.VariableNotDefined s)

let codegen_sizeof = ()


(* -------------------------------------------- *)

let codegen_structfield sid fid builder isref =
  let struct_ll = lookup_id sid builder in
  let f = match fid with Id(f, _) -> f in
  let field_index =
    let field =
      let struct_name =
        match sid with Id(_, d) -> (
            match d with Structtype(s) -> s
          )
      in
      struct_name ^ "." ^ f  (* how about changing sast field name to be global ?? *)
    in
    Hashtbl.find struct_field_indexes field
  in
  let p = L.build_struct_gep struct_ll field_index f builder in
  if isref then p
  else L.build_load p f builder

let codegen_pitch k o a builder =
  let p = (Core.Std.Char.to_string k) ^ (string_of_int o) ^ "_" ^ (string_of_int a) in
  let pitch_ll =
    try Hashtbl.find literal_tbl p
    with | Not_found ->
      codegen_allocate (A.Musictype(Pitch)) p builder
  in
  let octave = L.build_struct_gep pitch_ll 1 (p ^ ".octave") builder in
  let alter = L.build_struct_gep pitch_ll 2 (p ^ ".alter") builder in
  ignore(L.build_store (L.const_int i32_t o) octave builder);
  ignore(L.build_store (L.const_int i32_t a) alter builder);
  pitch_ll
(* L.const_named_struct (lookup_struct "pitch")
   ([|L.const_null str_t; L.const_int i32_t o; L.const_int i32_t a|]) *)

let rec codegen_print expr_list builder =
  let (llval_expr_list : L.llvalue list) = List.map (codegen_expr builder) expr_list in
  let llstrfmt =
    (* let idx = ref (-1) in *)
    let llval_and_fmt_of_expr expr = (* -> fmt : string *)
      (* incr idx; *)
      let print_fmt_of_datatype (t : A.datatype) =
        match t with
          Datatype(Int) -> "%d"
        | Datatype(String) -> "%s"
        | Datatype(Bool) ->
          (* print_endline (L.string_of_llvalue (List.nth llval_expr_list !idx)); *)
          "%d" (* TODO: print "true" or "false" *)
        | Datatype(Double) -> "%lf"
        | _ -> raise (Exceptions.InvalidTypePassedToPrint)
      in
      print_fmt_of_datatype (Analyzer.get_type_from_expr expr)
    in
    let fmt_list = List.map llval_and_fmt_of_expr expr_list in
    let fmt_str = String.concat " " fmt_list in
    (* let ll = L.const_stringz context "%d" in
       L.set_value_name "fmmt" ll; print_endline(L.value_name ll); *)
    L.build_global_stringptr fmt_str "fmt" builder
  in
  let actuals = Array.of_list (llstrfmt :: llval_expr_list) in
  (* actuals.(0) <- llstrfmt; *)
  L.build_call (lookup_func "printf") actuals "tmp" builder
  (*
  Dice:
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


and codegen_assign lhs_expr rhs_expr builder =
  let lhs =
    match lhs_expr with
      Id(_, _) -> lookup_id lhs_expr builder
    | StructField(s, f, _) -> codegen_structfield s f builder true
    | _ -> raise Exceptions.AssignLHSMustBeAssignable
    (*  | 	SArrayAccess(se, sel, d) -> codegen_array_access true se sel d llbuilder, true
    *)
  in
  let rhs =
    let store e =
      let rhs = codegen_expr builder e in
      ignore(L.build_store rhs lhs builder); rhs
    in
    match rhs_expr with
      Id(_, d) -> (
        match d with
          Datatype(_) -> store rhs_expr
        | _ -> lookup_id rhs_expr builder)
    | _ -> store rhs_expr
  in
  (*
  (**debug**) print_endline (L.string_of_llvalue lhs);
  (**debug**) print_endline (L.string_of_llvalue rhs); *)
  rhs
(*
  let lhs, isObjAccess = match lhs with
  in
  (* Codegen the rhs. *)
  let rhs = match rhs with
    | 	Sast.SId(id, d) -> codegen_id false false id d llbuilder
    |  	SObjAccess(e1, e2, d) -> codegen_obj_access true e1 e2 d llbuilder
    | _ -> codegen_sexpr llbuilder rhs
 *)


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
   | Not -> L.build_not) e1' "tmp" builder

(* Construct code for an expression; return its llvalue *)
and codegen_expr builder = function
    Id(s, _) -> load_id s builder
  | StructField(s, f, _) -> codegen_structfield s f builder false (* load *)
  | LitBool b -> L.const_int i1_t (if b then 1 else 0)
  | LitInt i -> L.const_int i32_t i
  | LitDouble d -> L.const_float double_t d
  | LitStr s -> L.build_global_stringptr s "tmp" builder
  | LitPitch(k, o, a) -> codegen_pitch k o a builder
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
  | VarDecl(d, s, e) ->
    ignore(codegen_allocate d s builder);
    if e <> Noexpr then ignore(codegen_assign (Id(s, d)) e builder);
    builder
  | If (e, s1, s2) -> codegen_if_stmt e s1 s2 builder

and codegen_if_stmt exp then_ (else_:stmt) builder =
  let cond_val = codegen_expr builder exp in

  (* Grab the first block so that we might later add the conditional branch
   * to it at the end of the function. *)
  let start_bb = L.insertion_block builder in
  let the_function = L.block_parent start_bb in

  let then_bb = L.append_block context "then" the_function in

  (* Emit 'then' value. *)
  L.position_at_end then_bb builder;
  let _(* then_val *) = codegen_stmt builder then_ in

  (* Codegen of 'then' can change the current block, update then_bb for the
   * phi. We create a new name because one is used for the phi node, and the
   * other is used for the conditional branch. *)
  let new_then_bb = L.insertion_block builder in

  (* Emit 'else' value. *)
  let else_bb = L.append_block context "else" the_function in
  L.position_at_end else_bb builder;
  let _ (* else_val *) = codegen_stmt builder else_ in

  (* Codegen of 'else' can change the current block, update else_bb for the
   * phi. *)
  let new_else_bb = L.insertion_block builder in


  let merge_bb = L.append_block context "ifcont" the_function in
  L.position_at_end merge_bb builder;
  (* let then_bb_val = value_of_block new_then_bb in *)
  let else_bb_val = L.value_of_block new_else_bb in
  (* let incoming = [(then_bb_val, new_then_bb); (else_bb_val, new_else_bb)] in *)
  (* let phi = build_phi incoming "iftmp" llbuilder in *)

  (* Return to the start block to add the conditional branch. *)
  L.position_at_end start_bb builder;
  ignore (L.build_cond_br cond_val then_bb else_bb builder);

  (* Set a unconditional branch at the end of the 'then' block and the
   * 'else' block to the 'merge' block. *)
  L.position_at_end new_then_bb builder; ignore (L.build_br merge_bb builder);
  L.position_at_end new_else_bb builder; ignore (L.build_br merge_bb builder);

  (* Finally, set the builder to the end of the merge block. *)
  L.position_at_end merge_bb builder;

  (* else_bb_val *) (* phi *)
  builder

let codegen_builtin_funcs () =
  (* Declare printf(), which the print built-in function will call *)
  let printf_t = L.var_arg_function_type i32_t [| str_t |] in
  let _ = L.declare_function "printf" printf_t the_module in
  ()

let codegen_def_func func =
  let formals_lltype = lltype_of_bind_list func.formals in
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

let codegen_def_struct (s : A.struct_decl) =
  let struct_t = L.named_struct_type context s.sname in
  Hashtbl.add struct_tbl s.sname struct_t

let codegen_struct (s : A.struct_decl) =
  List.iteri (fun i field ->
      let n = s.sname ^ "." ^ (snd field) in
      Hashtbl.add struct_field_indexes n i;
    ) s.fields;
  let struct_t = lookup_struct s.sname in
  let type_list = lltype_of_bind_list s.fields in
  L.struct_set_body struct_t (Array.of_list type_list) is_struct_packed
(* TODO: test forward declaration  *)
(*
  let llar = [| i32_t; i1_t
             (* array_type i8_type 10; vector_type i64_type 10  *)
             |] in
 *)


let linker filename =
  (* let llctx = L.global_context () in *)
  let llmem = L.MemoryBuffer.of_file filename in
  let llm = Llvm_bitreader.parse_bitcode context llmem in
  Llvm_linker.link_modules' the_module llm

let codegen_program program =
  (* TODO: maybe we don't need a separate main_module *)
  let btmodules = program.main_module :: program.btmodules in
  let def_funcs_and_structs btmodule =
    List.iter codegen_def_struct btmodule.structs;
    List.iter codegen_def_func btmodule.funcs
  in
  let build_funcs_and_structs btmodule =
    List.iter codegen_struct btmodule.structs;
    List.iter codegen_func btmodule.funcs
  in
  codegen_builtin_funcs ();
  List.iter def_funcs_and_structs btmodules;
  List.iter build_funcs_and_structs btmodules; (* main ?? *)
  linker "stdlib.bc";
  the_module



(* Batteries  *)

(* Invoke "f builder" if the current block doesn't already
   have a terminal (e.g., a branch). *)
(* let add_terminal builder f =
   match L.block_terminator (L.insertion_block builder) with
    Some _ -> ()
   | None -> ignore (f builder) in *)


(* print_endline (L.string_of_llvalue alloca); *)
