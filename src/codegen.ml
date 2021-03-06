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
let void_p = L.pointer_type size_t
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
  (* | Musictype(Note) -> lookup_struct "Note" *)
  | Structtype(s) -> lookup_struct s
  | Arraytype(d) -> lookup_array d
  | _ -> raise(Exceptions.Impossible("lltype_of_datatype"))

(* Create the struct for Arraytype(d), {int size; d* ptr; } *)
and lookup_array (d : A.datatype) =
  try Hashtbl.find array_tbl d
  with | Not_found ->
    let struct_t = L.named_struct_type context ("Arr_" ^ (Pprint.string_of_datatype d)) in
    let type_array = [|size_t; L.pointer_type (lltype_of_datatype d)|] in
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
(* TODO: L.build_array_alloca *)

(* Declare global variable and remember its llvalue
   in global_tbl if it's not a temporary variable *)
let codegen_global_allocate (typ : A.datatype) var_name builder isPermanent =
  Log.debug ("codegen_global_allocate: " ^ var_name);
  let zeroinitializer = L.const_null (lltype_of_datatype typ) in
  let alloca = L.define_global var_name zeroinitializer the_module in
  if isPermanent then Hashtbl.add global_tbl var_name alloca;
  alloca

let codegen_allocate (typ : A.datatype) var_name builder =
  if _debug then Log.debug ("codegen_allocate: " ^ var_name);
  if !is_main then codegen_global_allocate typ var_name builder true
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
(* Such as @p = global %struct._pitch { i8 67, i32 1, i32 1 }, align 4 *)

let get_ids_tmp = function
  | Id(s, _) -> "." ^ s
  | StructField(_, s, _) -> ".struct." ^ s


(* ------------------- LLVM Utils ------------------- *)

let break_block = ref (null_ll)
(* let continue_block = ref (null_ll) *)
let in_loop = ref false

let add_terminal builder' f =
  match L.block_terminator (L.insertion_block builder') with
  | Some ll -> Log.debug ("add_terminal: " ^ (L.string_of_llvalue ll))
  | None -> ignore (f builder') (* Add a terminal, i.e. a branch *)

let memcpy (lhs : L.llvalue) (rhs : L.llvalue) (size : L.llvalue) builder =
  let lhs_p = L.build_bitcast lhs ptr_t "lhs_p" builder in
  let rhs_p = L.build_bitcast rhs ptr_t "rhs_p" builder in
  Log.debug ("memcpy(): \n" ^ (L.string_of_llvalue lhs_p) ^ "\n" ^
             (L.string_of_llvalue rhs_p) ^ "\n" ^ (L.string_of_llvalue size));
  ignore(L.build_call (lookup_func "memcpy") [|lhs_p; rhs_p; size |] "" builder);
  lhs_p

let codegen_arraycopy lhs rhs d len builder = (* L.llvalue *)
  let len_cast = L.build_intcast len size_t ".lencast" builder in
  let size_ele = L.size_of d in
  let size = L.build_mul len_cast size_ele ".size" builder in
  memcpy lhs rhs size builder

let get_array_ptr arr_s idx_ll builder =
  let arr_p =
    let ptr_p = L.build_struct_gep arr_s 1 (".arrp_p") builder in
    L.build_load ptr_p ".arrp" builder
  in
  L.build_gep arr_p [| idx_ll |] ".rawidx" builder

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


and codegen_len el builder =
  if List.length el <> 1 then (Log.error "[ParamNumberNotMatch]"; null_ll)
  else (
    let arr_struct_p = codegen_expr builder (List.hd el) in
    let arr_struct_p = L.build_pointercast arr_struct_p void_p ".void_p" builder in
    L.build_call (lookup_func "len") [| arr_struct_p |] ".arrlen" builder)

and codegen_funccall fname el d builder =
  let f = lookup_func fname in
  let (actuals : L.llvalue array) = Array.of_list (List.map (codegen_expr builder) el) in
  (if _debug then
     Log.debug ("codegen_funccall(" ^ fname ^ "): ");
   let helper ll = Log.debug ("- " ^ L.string_of_llvalue ll) in
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
  let copy rhs = (* rhs is non-primitive, so rhs is ref *)
    let size_ll = (* the size of the type which rhs_p points to *)
      let codegen_sizeof e builder =
        let lltype = lltype_of_datatype (Analyzer.get_type_from_expr e) in
        let size_ll = L.size_of lltype in
        Log.debug ("rhs_size: " ^ (L.string_of_llvalue size_ll));
        size_ll
      in
      codegen_sizeof rhs_expr builder
    in
    (* set the value of what lhs_p points_to *)
    memcpy lhs rhs size_ll builder (* rhs_p *)
  in
  let d = Analyzer.get_type_from_expr rhs_expr in
  let rhs = codegen_expr builder rhs_expr in
  Log.debug ("lhs: " ^ (L.string_of_llvalue lhs) ^ "\n rhs: " ^ (L.string_of_llvalue rhs));
  match d with
  | Primitive(_) -> store rhs
  | _ -> copy rhs

and codegen_assign lhs_expr rhs_expr builder =
  let lhs = codegen_expr_ref builder lhs_expr in
  codegen_assign_with_lhs lhs rhs_expr builder

(* ----- Struct ----- *)

and codegen_note pitch duration builder =
  let p = codegen_expr builder pitch and d = codegen_expr builder duration in
  get_lit_alloca false ".litNote" (A.Structtype("Note")) [("p", p); ("d", d)] builder

and codegen_structfield struct_expr fid isref builder =
  let struct_ll = codegen_expr builder struct_expr in
  let tmp_name = fid in (* TODO: get_ids_tmp *)
  let field_index = (* TODO: separate *)
    let field =
      let global_field_name = function
        | A.Structtype(s) -> s ^ "." ^ tmp_name
        | _ -> raise (Exceptions.Impossible("Must be structtype unless Analyzer fails"))
      in
      global_field_name (Analyzer.get_type_from_expr struct_expr)
    in
    Hashtbl.find struct_field_indexes field
  in
  let p = L.build_struct_gep struct_ll field_index tmp_name builder in
  if isref then p
  else L.build_load p tmp_name builder

(* ----- Array ----- *)

and codegen_raw_array el d builder = (* d is element_type *)
  let len = L.const_int size_t (List.length el) in
  (* garbage!! no GC *)
  let arr = L.build_array_malloc (lltype_of_datatype d) len ".arr" builder in
  List.iteri (fun i e ->
      let ptr = L.build_gep arr [| (L.const_int i32_t i) |] ".idx" builder in
      ignore(codegen_assign_with_lhs ptr e builder);
    ) el;
  len, arr (* return llvalue of ptr of element_type *)

and codegen_set_array_struct d name (len : L.llvalue) (arr : L.llvalue) builder =
  let alloca = codegen_global_allocate d name builder false in
  Log.debug ("codegen_set_array_struct: " ^ (L.string_of_llvalue alloca));
  let arr_len = L.build_struct_gep alloca 0 (name ^ ".len") builder in
  let arr_p = L.build_struct_gep alloca 1 (name ^ ".p") builder in
  let len_cast = L.build_intcast len size_t ".lencast" builder in
  ignore(L.build_store len_cast arr_len builder);
  ignore(L.build_store arr arr_p builder);
  alloca

and codegen_array el d builder =
  if d = A.Primitive(Unit) then null_ll (* TODO: null!! skip unknown empty array [] *)
  else
    let len, arr = codegen_raw_array el d builder in
    let lit_name = ".litarr_" ^ (Pprint.string_of_datatype d) in
    codegen_set_array_struct (A.Arraytype(d)) lit_name len arr builder

and codegen_arrayidx a idx d isref builder =
  let idx_ll = codegen_expr builder idx in
  let arr_s = codegen_expr builder a in
  (* TODO: check idx in range  *)
  let p = get_array_ptr arr_s idx_ll builder in
  if isref then p
  else
    match d with
    | A.Primitive(_) -> L.build_load p ".val" builder
    | _ -> p

and codegen_arraysub a idx1 idx2 d builder =
  (* TODO: [:], [1:], [:-1] *)
  let ele_type =
    match d with
    | A.Arraytype(d) -> d
  in
  let ele_lltype = lltype_of_datatype ele_type in
  let arr_idx1 = codegen_arrayidx a idx1 ele_type true builder in
  (* TODO: check idx1 < idx2  *)
  let new_len = codegen_binop idx2 A.Sub idx1 builder in
  let new_arr = L.build_array_malloc ele_lltype new_len ".arrsub_p" builder in
  (* copy original array elements to new array *)
  let _ = codegen_arraycopy new_arr arr_idx1 ele_lltype new_len builder in
  codegen_set_array_struct d ".arrsub" new_len new_arr builder

and codegen_concat_array el d builder =
  (* garbage!! no GC *)
  let len_list = (* len of each array *)
    let get_len expr =
      let len =
        match expr with
        | LitArray(el, _) -> L.const_int i32_t (List.length el)
        | _ -> codegen_len [expr] builder
        (* L.build_bitcast () size_t ".cast" builder *)
      in
      Log.debug ("codegen_concat_array: " ^ (L.string_of_llvalue len)); len
    in
    List.map get_len el
  in
  let acc = ref (L.const_int i32_t 0) in
  let acc_len_array = (* start position of each array in the new array *)
    let acc_len len =
      let old_acc = !acc in
      Log.debug (L.string_of_llvalue len);
      acc := L.build_add !acc len ".add" builder;
      old_acc
    in
    Array.of_list (List.map acc_len len_list)
  in
  let ele_lltype =
    let ele_type = match d with
      | A.Arraytype(d) -> d
      | _ -> raise (Exceptions.Impossible "Analyzer assures that this is Arraytype" )
    in
    lltype_of_datatype ele_type
  in
  let new_arr = L.build_array_malloc ele_lltype !acc ".arrconcat_p" builder in
  let expr_array = Array.of_list (List.map (codegen_expr builder) el) in
  Log.debug ("codegen_concat_array: expr " ^ (L.string_of_llvalue (expr_array.(0))));
  (List.iteri (fun i len ->
       let arr_p = get_array_ptr (expr_array.(i)) (L.const_int i32_t 0) builder in
       let ptr = L.build_gep new_arr [| acc_len_array.(i) |] ".concatidx" builder in
       Log.debug ("codegen_concat_array: ptr " ^ (L.string_of_llvalue ptr));
       ignore(codegen_arraycopy ptr arr_p ele_lltype len builder)
     ) len_list);
  codegen_set_array_struct d ".arrconcat" !acc new_arr builder

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
     | "printf" -> codegen_print el builder
     | "len" -> codegen_len el builder
     | _ -> codegen_funccall fname el d builder )
  | Binop(e1, op, e2, _) -> codegen_binop e1 op e2 builder
  | Uniop(op, e1, _) -> codegen_unop op e1 builder
  (* Note that in Analyzer all legal types will be converted to Note types in a LitSeq  *)
  | LitArray(el, d) -> codegen_array el d builder (* ref *)
  | ArrayIdx(a, idx, d) -> codegen_arrayidx a idx d false builder (* load *)
  | ArraySub(a, idx1, idx2, d) -> codegen_arraysub a idx1 idx2 d builder (* ref *)
  | ArrayConcat(el, d) -> codegen_concat_array el d builder (* ref *)

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
  | Break -> ignore(L.build_br (L.block_of_value !break_block) builder); builder
  (* | Continue -> ignore(L.build_br (L.block_of_value !continue_block) builder); builder *)
  | _ -> Core.Std.failwith "[Impossible] Struct declaration are skiped in analyzer"

and codegen_ret d expr builder =
  match expr with
    Noexpr -> L.build_ret_void builder
  | _ -> L.build_ret (codegen_expr builder expr) builder

and codegen_if condition sast_then (sast_else : stmt) builder =
  let cond_val = codegen_expr builder condition in
  let start_bb = L.insertion_block builder in
  let the_function = L.block_parent start_bb in
  (* Insert blocks *)
  let then_bb = L.append_block context "if_then" the_function in
  let else_bb = L.append_block context "if_else" the_function in
  let merge_bb = L.append_block context "if_merge" the_function in
  (* Build if_cond block *)
  let cond_builder = L.builder_at_end context start_bb in
  let _ = L.build_cond_br cond_val then_bb else_bb cond_builder in
  (* Build if_then block *)
  let then_builder = codegen_stmt (L.builder_at_end context then_bb) sast_then in
  let _ = add_terminal then_builder (L.build_br merge_bb) in
  (* Build if_else block *)
  let else_builder = codegen_stmt (L.builder_at_end context else_bb) sast_else in
  let _ = add_terminal else_builder (L.build_br merge_bb) in
  L.builder_at_end context merge_bb

and codegen_while condition body builder =
  let the_function = L.block_parent (L.insertion_block builder) in
  (* Insert blocks *)
  let cond_bb = L.append_block context "loop_cond" the_function in
  let body_bb = L.append_block context "loop_body" the_function in
  let merge_bb = L.append_block context "loop_merge" the_function in
  (* br label %loop_cond *)
  let _ = L.build_br cond_bb builder in
  (* let old_val = !in_loop in
  let _ = if not old_val then break_block := L.value_of_block merge_bb in (* break outmost loop?? *)
  let _ = in_loop := true in *)
  let _ = break_block := L.value_of_block merge_bb in
  (* Build loop_cond block *)
  let cond_builder = L.builder_at_end context cond_bb in
  let cond_val = codegen_expr cond_builder condition in
  let _ = L.build_cond_br cond_val body_bb merge_bb cond_builder in
  (* Build loop_body block *)
  let body_builder = codegen_stmt (L.builder_at_end context body_bb) body in
  add_terminal body_builder (L.build_br cond_bb);
  (* in_loop := old_val; *)
  L.builder_at_end context merge_bb

let codegen_builtin_funcs () =
  (* Declare printf(), which the print built-in function will call *)
  let printf_t = L.var_arg_function_type i32_t [| str_t |] in
  let _ = L.declare_function "printf" printf_t the_module in
  let memcpy_t = L.function_type void_t [| ptr_t; ptr_t; size_t |] in
  let _ = L.declare_function "memcpy" memcpy_t the_module in
  (* Functions defined in stdlib.bc *)
  let render_seqs_t = L.var_arg_function_type void_t [| i32_t |] in
  let _ = L.declare_function "render_seqs_as_midi" render_seqs_t the_module in
  let len_t = L.function_type i32_t [| void_p |] in
  let _ = L.declare_function "len" len_t the_module in
  let render_as_midi_t = L.function_type void_t [| get_bind_type (A.Arraytype(A.seq_ele_type)) |] in (* TODO: add param *)
  let _ = L.declare_function "render_as_midi" render_as_midi_t the_module in
  let _str_of_pitch_t = L.function_type str_t [| get_bind_type (A.Primitive(Pitch)) |] in
  let _ = L.declare_function "_str_of_pitch" _str_of_pitch_t the_module in
  let _str_of_duration_t = L.function_type str_t [| get_bind_type (A.Primitive(Duration)) |] in
  let _ = L.declare_function "_str_of_duration" _str_of_duration_t the_module in
  let _str_of_Note_t = L.function_type str_t [| get_bind_type (A.Structtype("Note")) |] in
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
