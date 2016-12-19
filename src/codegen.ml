(*
 * Authors:
 *  - Ruonan Xu
 *  - Sona Roy
 *  - Jake Kwon
 *  - Eunice Kokor
 *)

(*
Code generation: translate takes a semantically checked AST and produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial
  http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:
  http://llvm.moe/
  http://llvm.moe/ocaml/
*)

module L = Llvm (* LLVM VMCore interface library *)
open Sast

module StringMap = Map.Make(String)

let _debug = true

let context = L.global_context () (* global data container *)
let the_module = L.create_module context "Beathoven Codegen" (* container *)
(* let builder = L.builder context *)
let double_t = L.double_type context
and i64_t = L.i64_type context
and i32_t = L.i32_type context
and i8_t = L.i8_type context
and i1_t = L.i1_type context
and void_t = L.void_type context
let str_t = L.pointer_type i8_t
let ptr_t = str_t
let size_t = L.type_of (L.size_of i8_t)
let null_ll = L.const_null i32_t
and null_str = L.const_null str_t

let is_main = ref false
(* All vardecls in main adopt global name and
   are defined as global variables in codegen. *)
let global_tbl:(string, L.llvalue) Hashtbl.t = Hashtbl.create 100
(* Use our own literal lookup instead of L.lookup_global  *)
let literal_tbl:(string, L.llvalue) Hashtbl.t = Hashtbl.create 100

let local_tbl:(string, L.llvalue) Hashtbl.t = Hashtbl.create 50
(* In formal_tbl are the actual values of parameters. If need to modify
   primitives in it, should create a copy variable with the same name
   in local_tbl.
*)
let formal_tbl:(string, L.llvalue) Hashtbl.t = Hashtbl.create 10

let array_tbl:(A.datatype, L.lltype) Hashtbl.t = Hashtbl.create 10
let struct_tbl:(string, L.lltype) Hashtbl.t = Hashtbl.create 10
let struct_field_indexes:(string, int) Hashtbl.t = Hashtbl.create 50
let is_struct_packed = false


(* ------------------- Utils ------------------- *)

let lookup_struct sname =
  try Hashtbl.find struct_tbl sname
  with | Not_found -> raise(Exceptions.UndefinedStructType sname)

let lookup_func fname =
  match (L.lookup_function fname the_module) with
  | None -> raise (Exceptions.Impossible "Analyzer should catch undefined funcs")
  | Some f -> f

let rec lltype_of_datatype (d : A.datatype) =
  match d with
  | Primitive(Unit) -> void_t
  | Primitive(Int) -> i32_t
  | Primitive(Double) -> double_t
  | Primitive(String) -> str_t
  | Primitive(Bool) -> i1_t
  | Primitive(Char) -> i8_t
  | Primitive(Duration) -> L.pointer_type (lookup_struct "_duration")
  | Primitive(Pitch) -> L.pointer_type (lookup_struct "_pitch")
  | Musictype(Note) -> lookup_struct "Note"
  | Musictype(Seq) -> lookup_array (A.Musictype(Note)) (* Future: note/chord *)
  | Structtype(s) -> lookup_struct s
  | Arraytype(d) -> lookup_array d
  | _ -> raise(Exceptions.Impossible("lltype_of_datatype"))

(* Create the struct for Arraytype(d), {int size; d* ptr; } *)
and lookup_array (d : A.datatype) =
  try Hashtbl.find array_tbl d
  with | Not_found ->
    let struct_t = L.named_struct_type context ("Arr_" ^ (Pprint.string_of_datatype d)) in
    let type_array = [|i32_t; L.pointer_type (lltype_of_datatype d)|] in
    L.struct_set_body struct_t type_array is_struct_packed;
    Hashtbl.add array_tbl d struct_t;
    struct_t

let get_bind_type d =
  let lltype = lltype_of_datatype d in
  match d with
  | Primitive(_) -> lltype
  | _ -> L.pointer_type lltype

let lltype_of_bind_list (bind_list : A.bind list) =
  List.map (fun (d, _) -> get_bind_type d) bind_list


(* Declare local variable and remember its llvalue in local_tbl *)
let codegen_local_allocate (typ : A.datatype) var_name builder =
  if _debug then Log.debug ("codegen_local_allocate: " ^ var_name);
  let t = lltype_of_datatype typ in
  let alloca = L.build_alloca t var_name builder in
  Hashtbl.add local_tbl var_name alloca;
  alloca

(* Declare global variable and remember its llvalue in global_tbl *)
let codegen_global_allocate (typ : A.datatype) var_name builder =
  if _debug then Log.debug ("codegen_global_allocate: " ^ var_name);
  let zeroinitializer = L.const_null (lltype_of_datatype typ) in
  let alloca = L.define_global var_name zeroinitializer the_module in
  Hashtbl.add global_tbl var_name alloca;
  alloca

let codegen_allocate (typ : A.datatype) var_name builder =
  if _debug then Log.debug ("codegen_allocate: " ^ var_name);
  if !is_main then codegen_global_allocate typ var_name builder
  else codegen_local_allocate typ var_name builder


(* Return the value for a variable or formal argument *)
(* duplicate name in formal will be overwritten by local *)
let load_id id builder =
  match id with
  | Id(s, d) -> (
      let isloaded = ref false in
      let v =
        try Hashtbl.find local_tbl s
        with | Not_found ->
        try
          let v = Hashtbl.find formal_tbl s in
          isloaded := true; v
        with | Not_found ->
        try Hashtbl.find global_tbl s
        with Not_found -> raise (Exceptions.Impossible
                                   ("Undefined var not caught in Analyzer unless there is bug in Codegen"))
      in
      match d with (* Only load primitives *)
      | A.Primitive(_) -> if !isloaded then v else L.build_load v s builder
      | _ -> v )
  | _ -> raise (Exceptions.Impossible("load_id"))

let lookup_id id builder =
  match id with
  | Id(s, d) -> (
      try Hashtbl.find local_tbl s
      with | Not_found ->
      try
        let v = Hashtbl.find formal_tbl s in
        let alloca =
          if _debug then Log.debug ("lookup_id (formal_tbl): " ^ s);
          codegen_allocate d s builder
        in
        ignore (L.build_store v alloca builder);
        alloca
      with | Not_found ->
      try Hashtbl.find global_tbl s
      with Not_found -> raise (Exceptions.VariableNotDefined s))
  | _ -> raise (Exceptions.Impossible("lookup_id"))


let codegen_lit_alloca isPermanent (typ : A.datatype) var_name builder =
  let lltype = (* Actual type of literals *)
    match typ with
    | Primitive(Duration) -> lookup_struct "_duration"
    | Primitive(Pitch) -> lookup_struct "_pitch"
    | _ -> lltype_of_datatype typ
  in
  let zeroinitializer = L.const_null lltype in
  let alloca = L.define_global var_name zeroinitializer the_module in
  if isPermanent then Hashtbl.add literal_tbl var_name alloca;
  (* TODO: temp using L.build_alloca *)
  alloca (* ref lltype *)

let get_lit_alloca isPermanent name d (l : (string * L.llvalue) list) builder =
  let alloca = codegen_lit_alloca isPermanent d name builder in
  let set_struct_field i (field, llvalue) =
    let field' = L.build_struct_gep alloca i (name ^ "." ^ field) builder in
    ignore(L.build_store llvalue field' builder)
  in
  List.iteri set_struct_field l;
  alloca

(* These literals have unique id and will be stored in literal_tbl *)
let get_literal_alloca name d (l : (string * L.llvalue) list) builder =
  try Hashtbl.find literal_tbl name
  with | Not_found ->
    get_lit_alloca true name d l builder
(* An alternative is to use initializer (but need a table for initializer, )
   L.const_named_struct (lookup_struct "pitch")
   ([|L.const_null str_t; L.const_int i32_t o; L.const_int i32_t a|]) *)


(* -------------------------------------------- *)

let codegen_pitch k o a builder =
  let pitch = (Core.Std.Char.to_string k) ^ (string_of_int o) ^ "_" ^ (string_of_int a) in
  let ptr_lit = get_literal_alloca pitch (A.Primitive(Pitch))
      [("key", L.const_int i8_t (Char.code k)); ("octave", L.const_int i32_t o);
       ("alter", L.const_int i32_t a)] builder in
  ptr_lit (* primitive: _pitch* *)

let codegen_duration a b builder =
  let gcd' =
    let rec gcd a b = if b = 0 then a else gcd b (a mod b) in
    gcd a b
  in
  let a = a / gcd' and b = b / gcd' in
  let duration = (string_of_int a) ^ "/" ^ (string_of_int b) in
  let ptr_lit = get_literal_alloca duration (A.Primitive(Duration))
      [("a", L.const_int i32_t a); ("b", L.const_int i32_t b)] builder
  in
  ptr_lit (* primitive: _duration* *)
(* Seems there is no need to cast, since when assign we simply store it. *)
(* cast_literal_alloca duration (A.Primitive(Duration)) ptr_lit builder *)

(* ----- Functions ----- *)

let rec codegen_print expr_list builder =
  let (llval_expr_list : L.llvalue list) = List.map (codegen_expr builder) expr_list in
  let printfmt =
    let llval_and_fmt_of_expr expr = (* -> fmt : string *)
      let print_fmt_of_datatype (t : A.datatype) =
        match t with
        | Primitive(Int) -> "%d"
        | Primitive(String) -> "%s"
        | Primitive(Char) -> "%c"
        | Primitive(Bool) ->
          (* print_endline (L.string_of_llvalue (List.nth llval_expr_list !idx)); *)
          "%d" (* TODO: print "true" or "false" *)
        | Primitive(Double) -> "%lf"
        | _ -> raise (Exceptions.InvalidTypePassedToPrint)
      in
      print_fmt_of_datatype (Analyzer.get_type_from_expr expr)
    in
    let fmt_list = List.map llval_and_fmt_of_expr expr_list in
    let fmt_str = String.concat "" fmt_list in
    L.build_global_stringptr fmt_str "fmt" builder
  in
  let actuals = Array.of_list (printfmt :: llval_expr_list) in
  L.build_call (lookup_func "printf") actuals "tmp" builder
  (*
  ref Dice:
  let zero = const_int i32_t 0 in
  let s = build_in_bounds_gep llstrfmt [| zero |] "tmp" llbuilder in
  build_call printf (Array.of_list (s :: params)) "tmp" llbuilder
   *)

and codegen_funccall fname el d builder =
  let f = lookup_func fname in
  let (actuals : L.llvalue array) = Array.of_list (List.map (codegen_expr builder) el) in
  (if _debug then
     Log.debug ("codegen_funccall(" ^ fname ^ "): ");
   let helper ll = Log.debug (L.string_of_llvalue ll) in
   Array.iter helper actuals);
  match d with
  | A.Primitive(A.Unit) -> L.build_call f actuals "" builder
  | _ -> L.build_call f actuals "tmp" builder

(* ----- Assignment ----- *)

and codegen_assign_with_lhs lhs rhs_expr builder =
  let store rhs =
    ignore(L.build_store rhs lhs builder);
    rhs
  in
  let memcpy rhs = (* rhs is non-primitive, so rhs is ref *)
    let size_ll = (* the size of the type which rhs_p points to *)
      let codegen_sizeof e builder =
        let lltype = lltype_of_datatype (Analyzer.get_type_from_expr e) in
        let size_ll = L.size_of lltype in
        Log.debug ("rhs_size: " ^ (L.string_of_llvalue size_ll));
        (* L.build_bitcast size_ll size_t "size" builder *)
        size_ll
      in
      codegen_sizeof rhs_expr builder
    in
    let lhs_p = L.build_bitcast lhs ptr_t "lhs_p" builder in
    let rhs_p = L.build_bitcast rhs ptr_t "rhs_p" builder in
    (* set the value of what lhs_p points_to *)
    ignore(L.build_call (lookup_func "memcpy") [|lhs_p; rhs_p; size_ll |] "" builder);
    rhs_p
  in
  let d = Analyzer.get_type_from_expr rhs_expr in
  let rhs = codegen_expr builder rhs_expr in
  Log.debug ("lhs: " ^ (L.string_of_llvalue lhs) ^ "\n rhs: " ^ (L.string_of_llvalue rhs));
  match d with
  | Primitive(_) -> store rhs
  | _ -> memcpy rhs

and codegen_assign lhs_expr rhs_expr builder =
  let lhs = codegen_expr_ref builder lhs_expr in
  codegen_assign_with_lhs lhs rhs_expr builder

(* ----- Struct ----- *)

and codegen_note pitch duration builder =
  let p = codegen_expr builder pitch and d = codegen_expr builder duration in
  get_lit_alloca false ".litNote" (A.Musictype(Note)) [("p", p); ("d", d)] builder

and codegen_structfield struct_expr fid isref builder =
  let struct_ll = codegen_expr builder struct_expr in
  (* let struct_ll = lookup_id sid builder in *)
  let f = match fid with Id(f, _) -> f in
  let field_index =
    let field =
      let global_field_name = function
        | A.Structtype(s) -> s ^ "." ^ f
        | _ -> raise (Exceptions.Impossible("Must be structtype unless Analyzer fails"))
      in
      global_field_name (Analyzer.get_type_from_expr struct_expr)
    in
    Hashtbl.find struct_field_indexes field
  in
  let p = L.build_struct_gep struct_ll field_index f builder in
  if isref then p
  else L.build_load p f builder

(* ----- Array ----- *)

and codegen_array el d builder =
  if d = A.Primitive(Unit) then null_ll (* TODO: null!! skip unknown empty array [] *)
  else
    let len, arr = (* codegen_raw_array el element_type builder *)
      (* no GC *)
      let len =
        let length =
          List.fold_left (fun count expr ->
              match Analyzer.get_type_from_expr expr with
              | Arraytype(_) ->
                count + 1 (* TODO: codegen_expr expr *)
              | _ -> count + 1
            ) 0 el
        in
        L.const_int i32_t length
      in
      let arr = L.build_array_malloc (lltype_of_datatype d) len ".arr" builder in
      let i = ref 0 in
      List.iter (fun e ->
          let ptr = L.build_gep arr [| (L.const_int i32_t !i) |] ".idx" builder in
          ignore(codegen_assign_with_lhs ptr e builder);
          incr i;
        ) el;
      len, arr (* return llvalue of ptr of element_type *)
    in
    let lit_name = ".litarr_" ^ (Pprint.string_of_datatype d) in
    let alloca = codegen_global_allocate (A.Arraytype(d)) lit_name builder in
    let arr_len = L.build_struct_gep alloca 0 (lit_name ^ ".len") builder in
    let arr_p = L.build_struct_gep alloca 1 (lit_name ^ ".p") builder in
    ignore(L.build_store len arr_len builder);
    ignore(L.build_store arr arr_p builder);
    alloca

and codegen_arrayidx a idx d isref builder =
  let idx_ll = codegen_expr builder idx in
  let arr_s = codegen_expr builder a in
  (* TODO: check idx in range  *)
  let arr_p =
    let ptr_p = L.build_struct_gep arr_s 1 (".ptr_p") builder in
    L.build_load ptr_p ".arr_p" builder
  in
  let p = L.build_gep arr_p [| idx_ll |] ".arridx" builder in
  if isref then p
  else
    match d with
    | A.Primitive(_) -> L.build_load p ".val" builder
    | _ -> p


(* ----- Operators ----- *)

and codegen_binop e1 (op : A.binary_operator) e2 builder =
  let e1' = codegen_expr builder e1
  and e2' = codegen_expr builder e2 in
  (match op with
     Add -> L.build_add
   | Sub -> L.build_sub
   | Mult -> L.build_mul
   | Div -> L.build_sdiv
   | Equal -> L.build_icmp L.Icmp.Eq
   (* TODO: string type equality *)
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

(* ----- Expressions ----- *)

(* Construct code for an expression; return its llvalue.
   For non-primitive type, the returned llvalue is ref.
*)
and codegen_expr builder = function
  | Id(_, _) as id -> load_id id builder
  | StructField(e, f, _) -> codegen_structfield e f false builder (* load *)
  | LitBool b -> L.const_int i1_t (if b then 1 else 0)
  | LitInt i -> L.const_int i32_t i
  | LitDouble d -> L.const_float double_t d
  | LitStr s -> L.build_global_stringptr s "tmp" builder
  | LitChar c -> L.const_int i8_t (Char.code c)
  | LitPitch(k, o, a) -> codegen_pitch k o a builder (* load *)
  | LitDuration(a, b) -> codegen_duration a b builder (* load *)
  | LitNote(p, d) -> codegen_note p d builder (* ref  *)
  | Noexpr -> null_ll
  | Null -> null_ll
  | Assign(e1, e2, _) -> codegen_assign e1 e2 builder
  | FuncCall(fname, el, d) ->
    (match fname with
       "printf" -> codegen_print el builder
     | _ -> codegen_funccall fname el d builder )
  | Binop(e1, op, e2, _) -> codegen_binop e1 op e2 builder
  | Uniop(op, e1, _) -> codegen_unop op e1 builder
  | LitSeq(el) -> codegen_array el (A.Musictype(Note)) builder (* ref *)
  | LitArray(el, d) -> codegen_array el d builder (* ref *)
  | ArrayIdx(a, idx, d) -> codegen_arrayidx a idx d false builder (* load *)
  | ArraySub(a, idx1, idx2, d) -> L.const_null i32_t (* TODO *)

and codegen_expr_ref builder expr =
  match expr with
  (* Structtype, Arraytype, pitch, duration *)
  | Id(_, _) -> lookup_id expr builder
  | StructField(e, f, _) -> codegen_structfield e f true builder
  | ArrayIdx(a, idx, d) -> codegen_arrayidx a idx d true builder
  | _ -> raise (Exceptions.ExpressionNotAssignable(Pprint.string_of_expr expr))


(* ----- Statements ----- *)

let rec codegen_stmt builder = function
  | Block sl -> List.fold_left codegen_stmt builder sl
  | Expr(e, _) -> ignore(codegen_expr builder e); builder
  | Return(e, d) -> ignore(codegen_ret d e builder); builder
  | VarDecl(d, s, e) ->
    ignore(codegen_allocate d s builder);
    if e <> Noexpr then ignore(codegen_assign (Id(s, d)) e builder);
    builder
  | Return(e, d) -> ignore(codegen_ret d e builder); builder
  | If(e, s1, s2) -> codegen_if e s1 s2 builder
  | While(pred, body) -> codegen_while pred body builder
  | For(e1, e2, e3, body) -> codegen_stmt builder (* this way not works well with Continue *)
                               (Block [
                                   Expr(e1, Analyzer.get_type_from_expr e1);
                                   While(e2,
                                         Block [body;
                                                Expr(e3, Analyzer.get_type_from_expr e1)]) ] )
  | Break -> builder (*TODO*)
  | Continue -> builder (*TODO*)
  | _ -> Core.Std.failwith "[Impossible] Struct declaration are skiped in analyzer"

and codegen_for null_expr1 cond_ null_expr2 body_ builder =
(*
init_ == null_expr1
inc_ ==  null_expr2
*)
  let br_block = ref (L.block_of_value (L.const_int i32_t 0)) in
  let (cont_block) = ref (L.block_of_value (L.const_int i32_t 0)) in
  let is_loop = ref false in

  let old_val = !is_loop in
  is_loop := true;

  let the_function = L.block_parent (L.insertion_block builder) in

    let _ = codegen_expr builder null_expr1 in

    let loop_bb = L.append_block context "loop" the_function in
    let inc_bb = L.append_block context "inc" the_function in
    let cond_bb = L.append_block context "cond" the_function in
    let after_bb = L.append_block context "afterloop" the_function in

    let _ = if not old_val then
      cont_block := inc_bb;
      br_block := after_bb;
    in

    ignore (L.build_br cond_bb builder);

  L.position_at_end loop_bb builder;

  ignore (codegen_stmt builder body_);

  let bb = L.insertion_block builder in
  L.move_block_after bb inc_bb;
  L.move_block_after inc_bb cond_bb;
  L.move_block_after cond_bb after_bb;
  ignore(L.build_br inc_bb builder);

  L.position_at_end inc_bb builder;

  let _ = codegen_expr builder null_expr2 in
  ignore(L.build_br cond_bb builder);

  L.position_at_end cond_bb builder;

  let cond_val = codegen_expr builder cond_ in
  ignore (L.build_cond_br cond_val loop_bb after_bb builder);

  L.position_at_end after_bb builder;

  is_loop := old_val;


and codegen_ret d expr builder =
  match expr with
    Noexpr -> L.build_ret_void builder
  | _ -> L.build_ret (codegen_expr builder expr) builder

and codegen_if exp then_ (else_:stmt) builder =
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

and codegen_while condition body builder =
  let the_function = L.block_parent (L.insertion_block builder) in
  let add_terminal builder' f =
    match L.block_terminator (L.insertion_block builder') with
    | Some ll -> Log.debug ("codegen_while: " ^ (L.string_of_llvalue ll))
    | None -> ignore (f builder') (* Add a terminal, a branch *)
  in
  (* Insert condition block *)
  let cond_bb = L.append_block context "loop_cond" the_function in
  let body_bb = L.append_block context "loop_body" the_function in
  let merge_bb = L.append_block context "loop_merge" the_function in
  (* br label %loop_cond *)
  let _ = L.build_br cond_bb builder in
  (* Build loop_cond block *)
  let cond_builder = L.builder_at_end context cond_bb in
  let cond_val = codegen_expr cond_builder condition in
  let _ = L.build_cond_br cond_val body_bb merge_bb cond_builder in
  (* Build loop_body block *)
  let body_builder = codegen_stmt (L.builder_at_end context body_bb) body in
  add_terminal body_builder (L.build_br cond_bb);
  L.builder_at_end context merge_bb

let codegen_builtin_funcs () =
  (* Declare printf(), which the print built-in function will call *)
  let printf_t = L.var_arg_function_type i32_t [| str_t |] in
  let _ = L.declare_function "printf" printf_t the_module in
  let memcpy_t = L.function_type void_t [| ptr_t; ptr_t; size_t |] in
  let _ = L.declare_function "memcpy" memcpy_t the_module in
  (* Functions defined in stdlib.bc *)
  let render_as_midi_t = L.function_type void_t [| get_bind_type (A.Arraytype(A.Musictype(Note))) |] in (* TODO: add param *)
  let _ = L.declare_function "render_as_midi" render_as_midi_t the_module in
  let _str_of_pitch_t = L.function_type str_t [| get_bind_type (A.Primitive(Pitch)) |] in
  let _ = L.declare_function "_str_of_pitch" _str_of_pitch_t the_module in
  let _str_of_duration_t = L.function_type str_t [| get_bind_type (A.Primitive(Duration)) |] in
  let _ = L.declare_function "_str_of_duration" _str_of_duration_t the_module in
  let _str_of_Note_t = L.function_type str_t [| get_bind_type (A.Musictype(Note)) |] in
  let _ = L.declare_function "_str_of_Note" _str_of_Note_t the_module in
  ()

let codegen_def_func func =
  let formals_lltype = lltype_of_bind_list func.formals in
  let func_t = L.function_type (get_bind_type func.returnType) (Array.of_list formals_lltype) in
  ignore(L.define_function func.fname func_t the_module) (* llfunc *)

let codegen_func func =
  Log.debug ("codegen_func: " ^ func.fname);
  Hashtbl.clear formal_tbl;
  Hashtbl.clear local_tbl;
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
  let _ = init_params llfunc func.formals in
  let llbuilder = codegen_stmt llbuilder (Block(func.body)) in
  (* Finish off the function. *)
  if func.returnType = A.Primitive(A.Unit)
  then ignore(L.build_ret_void llbuilder)
  else ()
(* TODO: return 0 for main.  *)
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
  let btmodules = program.btmodules in
  let def_funcs_and_structs btmodule =
    List.iter codegen_def_struct btmodule.structs;
    List.iter codegen_def_func btmodule.funcs
  in
  let build_funcs_and_structs btmodule =
    List.iter codegen_struct btmodule.structs;
    match btmodule.funcs with
    | [] -> raise (Exceptions.Impossible "Each module has at least one func (main)")
    | hd :: tl ->
      (* main of modules *)
      is_main := true; codegen_func hd;
      (* functions in modules *)
      is_main := false; List.iter codegen_func tl
  in
  List.iter def_funcs_and_structs btmodules; (* define language structs first *)
  codegen_builtin_funcs ();
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
