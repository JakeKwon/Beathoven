(*
 * Authors:
 *  - Ruonan Xu
 *  - Jake Kwon
 *  - Eunice Kokor
 *  - Sona Roy
 *)

open Ast
module A = Ast
module S = Sast

open Environment
open Pprint

module SS = Set.Make(
  struct
    let compare = Pervasives.compare
    type t = datatype
  end )


(* ------------------- SAST Utilities ------------------- *)

let get_var_type env s =
  try StringMap.find s env.var_map
  with | Not_found ->
    (if env.ismain then raise (Exceptions.VariableNotDefined s));
    try
      let (d, _) = StringMap.find s env.formal_map in d
    with | Not_found ->
    (* Note that local variables can overwrite module fields (global variables) *)
    try StringMap.find (get_global_name env.name s) !(env.btmodule).field_map
    with | Not_found -> raise (Exceptions.VariableNotDefined s)

let get_type_from_expr (expr : S.expr) =
  match expr with
    Id (_,d) -> d
  | StructField(_,_,d) -> d
  | LitBool(_) -> A.Primitive(Bool)
  | LitInt(_) -> A.Primitive(Int)
  | LitDouble(_) -> A.Primitive(Double)
  | LitChar(_) -> A.Primitive(Char)
  | LitStr(_) -> A.Primitive(String)
  | LitPitch(_,_,_) -> A.Primitive(Pitch)
  | LitDuration(_,_) -> A.Primitive(Duration)
  | LitNote(_,_) -> A.Structtype("Note")
  | Null -> A.Primitive(Unit) (* Null -> Primitive(Null_t) *)
  | Binop(_,_,_,d) -> d
  | Uniop(_,_,d) -> d
  | Assign(_,_,d) -> d
  | FuncCall(_,_,d)-> d
  | Noexpr -> A.Primitive(Unit)
  | LitArray(_,d) -> Arraytype(d)
  | ArrayConcat(_,d) -> d
  | ArrayIdx(_,_,d) -> d
  | ArraySub(_,_,_,d) -> d

let get_stmt_from_expr e =
  let t = get_type_from_expr e in
  S.Expr(e, t)

let check_condition (e : S.expr) =
  match (get_type_from_expr e) with
  | A.Primitive(Bool) | A.Primitive(Unit) -> ()
  | _ -> raise (Exceptions.InvalidConditionType)

let get_litpitch (sast_expr : S.expr) =
  match get_type_from_expr sast_expr with
  | A.Primitive(Pitch) -> sast_expr
  | A.Primitive(Int) -> (
      match sast_expr with
      | LitInt(d) ->
        if d = 0 then S.LitPitch('H',4,0)
        (* ((d+4) mod 7) + 62) gives right note for each integer input *)
        else if d >= 1 && d <= 7 then S.LitPitch(Char.chr (( (d+1) mod 7 + 65)),4,0)
        else raise (Exceptions.InvalidPitchAssignment "make sure your pitch is with in 0-7")
      | Id(_) -> sast_expr (* TODO: codegen !! *)
      | _ -> raise (Exceptions.Impossible "get_litpitch")
    )
  | _ -> Log.error "[InvalidPitchAssignment]"; sast_expr

(* let get_map_size map =
   StringMap.fold (fun k v i -> i + 1) map 0 *)

(* ------------------- build sast from ast ------------------- *)

let rec build_sast_expr env (expr : A.expr) =
  match expr with
  | Id(s) -> env,
             let s = if env.ismain then (get_global_name env.name s) else s in
             S.Id(s, get_var_type env s)
  | StructField(e, f) -> analyze_struct env e f
  | LitBool(b) -> env, S.LitBool(b)
  | LitInt(i) -> env, S.LitInt(i)
  | LitDouble(f) -> env, S.LitDouble(f)
  | LitChar(c) -> env, S.LitChar(c)
  | LitStr(s) -> env, S.LitStr(s)
  | LitPitch(k, o, a) -> env, S.LitPitch(k, o, a)
  | LitDuration(a, b) -> env, S.LitDuration(a, b)
  | LitNote(p, d) -> analyze_note env p d
  | Binop(e1, op, e2) -> analyze_binop env e1 op e2
  | Uniop(op, e) -> analyze_unop env op e
  | Assign(e1, e2) -> analyze_assign env e1 e2
  | FuncCall(s, el) -> (* TODO: Chord::func() ?? *)
    analyze_funccall env s el (* env, FuncCall (s,el,_) *)
  | Noexpr -> env, S.Noexpr
  | Null -> env, S.Null
  | LitSeq(el) -> analyze_seq env el (* LitArray(el', seq_ele_type) *)
  | LitArray(el) -> analyze_array env el
  | ArrayIdx(a, e) -> analyze_arrayidx env a e
  | ArraySub(a, e1, e2) -> analyze_arraysub env a e1 e2

and build_sast_expr_list env (expr_list:A.expr list) =
  let helper_expr expr = snd (build_sast_expr env expr) in
  let sast_expr_list = List.map helper_expr expr_list in
  (* print_int (get_map_size env.var_map); *)
  env, sast_expr_list

(* ref: Dice/check_obj_access *)
and analyze_struct env e f =
  let _, sast_expr = build_sast_expr env e in
  let field_bind =
    let struct_type = get_type_from_expr sast_expr in
    let struct_decl =
      match struct_type with
        Structtype n -> (
          try StringMap.find n !(env.btmodule).struct_map
          with | Not_found -> raise(Exceptions.Impossible("analyze_struct")))
      | _ as d -> raise (Exceptions.ShouldAccessStructType (string_of_datatype d))
    in
    try List.find (fun field -> (snd field) = f) struct_decl.fields
    with | Not_found -> raise(Exceptions.StructFieldNotFound(
        (string_of_datatype struct_type), f))
  in
  env, S.StructField(sast_expr, snd field_bind, fst field_bind)

and analyze_note env p d =
  let _, pitch = build_sast_expr env p in
  let pitch = get_litpitch pitch in
  let _, duration = build_sast_expr env d in
  env, S.LitNote(pitch, duration)

(* ----- Array ----- *)

and analyze_seq env (expr_list:A.expr list) =
  let _, sast_expr_list = build_sast_expr_list env expr_list in
  if List.length sast_expr_list = 0 then
    env, S.LitArray([], A.seq_ele_type)
  else
    let flattened_sast_expr_list =
      let flatten_seq l (expr : S.expr) =
        (* Cast datatype and flatten Seq *)
        match get_type_from_expr expr with
        | A.Structtype("Note") -> expr :: l
        | A.Primitive(Pitch) -> S.LitNote(expr, LitDuration(1, 4)) :: l
        | A.Primitive(Duration) -> S.LitNote(LitPitch('C', 4, 0), expr) :: l
        | A.Arraytype(seq_ele_type) -> (
            match expr with
            | LitArray(el, _) -> (List.rev el) @ l
            | _ -> expr :: l
          )
        (* Future: Chord *)
        | _ -> Log.error "[TypeNotMatch] Element of Seq should have Note type"; l
      in
      List.rev (List.fold_left flatten_seq [] sast_expr_list)
    in
    env, S.LitArray(flattened_sast_expr_list, seq_ele_type)

(* TODO: update it *)
and analyze_array env (expr_list:A.expr list) =
  let _, sast_expr_list = build_sast_expr_list env expr_list in
  if List.length sast_expr_list = 0 then
    env, S.LitArray([], Primitive(Unit))
  else
    let get_ele_type expr  =
      let ele_type = get_type_from_expr expr  in
      match ele_type with
      | Arraytype(d) -> ele_type, d
      | _ -> ele_type, ele_type
    in
    let ele_type = ref (A.Primitive(Unit)) in
    let sast_expr_list =
      let helper_array l (expr : S.expr) =
        let d, d' = get_ele_type expr in
        Log.debug ((string_of_datatype d) ^ " and " ^ (string_of_datatype d'));
        if d' = Primitive(Unit) then l (* expr is [] *)
        else
          (if !ele_type = Primitive(Unit) then ele_type := d';
           (* Note that d' is not Unit *)
           if d = !ele_type then (
             expr :: (fst l), snd l)
           else if d' = !ele_type then
             (
               match expr with
               | S.LitArray(el, _) -> (el @ (fst l), snd l)
               | _ as e ->
                let arrays =
                  if List.length (fst l) = 0 then snd l
                  else (S.LitArray(fst l, d') :: snd l)
                in
                ([] , e :: arrays)
             )
           else raise (Exceptions.ArrayTypeNotMatch(string_of_datatype d'))
          )
      in
      let l = List.fold_left helper_array ([], []) (List.rev sast_expr_list) in
      if List.length (fst l) = 0 then snd l
      else S.LitArray(fst l, !ele_type) :: snd l
    in
    let sast_litarray =
      if List.length sast_expr_list = 1 then List.hd sast_expr_list
      else S.ArrayConcat(sast_expr_list, Arraytype(!ele_type))
    in
    env, sast_litarray

and analyze_arrayidx env a e =
  let _, sast_arr = build_sast_expr env a in
  let ele_type =
    match get_type_from_expr sast_arr with
    | Arraytype(d) -> d
    | _ as d -> raise (Exceptions.ShouldAccessArray(string_of_datatype d))
  in
  let _, idx = build_sast_expr env e in
  (* TODO J: check idx is int type *)
  env, S.ArrayIdx(sast_arr, idx, ele_type)

and analyze_arraysub env a e1 e2 =
  let _, sast_arr = build_sast_expr env a in
  let d = get_type_from_expr sast_arr in
  let get_sast_index e =
    let _, idx = build_sast_expr env e in
    let t = get_type_from_expr idx in
    if t = Primitive(Int) || t = Primitive(Unit) then idx
    else (Log.error "[IndexTypeMismatch]"; idx)
  in
  match d with
  | Arraytype(_) -> (
      let idx1 = get_sast_index e1 and
      idx2 = get_sast_index e2 in
      env, S.ArraySub(sast_arr, idx1, idx2, d)
    )
  | _ -> raise (Exceptions.ShouldAccessArray(string_of_datatype d))

(* ----- Operators ----- *)

and analyze_binop env e1 op e2 = (* -> env, Binop (e1,op,e2,t) *)
  let _, se1 = build_sast_expr env e1 in
  let _, se2 = build_sast_expr env e2 in
  let t1 = get_type_from_expr se1 in
  let t2 = get_type_from_expr se2 in
  let get_logical_binop_type se1 se2 op = function
      (A.Primitive(Bool), A.Primitive(Bool)) -> S.Binop(se1, op, se2, A.Primitive(Bool))
    | _ -> raise (Exceptions.InvalidBinopExpression "Logical operators only operate on Bool types")
  in
  let get_sast_equality_binop () =
    if t1 = t2 then
      match t1 with
      | Primitive(Bool) | Primitive(Int) | Primitive(String)
        -> S.Binop(se1, op, se2, A.Primitive(Bool))
      (* Equality op not supported for double operands. *)
      | _ -> raise (Exceptions.InvalidBinopExpression "Equality operation is not supported for double type")
    else raise (Exceptions.InvalidBinopExpression "Equality operator can't operate on different types")
  in
  let get_comparison_binop_type type1 type2 se1 se2 op =
    let numerics = SS.of_list [A.Primitive(Int); A.Primitive(Double)]
    in
    if SS.mem type1 numerics && SS.mem type2 numerics
    then S.Binop(se1, op, se2, A.Primitive(Bool))
    else raise (Exceptions.InvalidBinopExpression "Comparison operators operate on numeric types only")
  in
  let get_arithmetic_binop_type se1 se2 op = function
      (A.Primitive(Int), A.Primitive(Double))
    | (A.Primitive(Double), A.Primitive(Int))
    | (A.Primitive(Double), A.Primitive(Double)) -> S.Binop(se1, op, se2, A.Primitive(Double))
    (* | (A.Primitive(Int), A.Primitive(Char_t))
       | (A.Primitive(Char_t), A.Primitive(Int))
       | (A.Primitive(Char_t), A.Primitive(Char_t)) -> S.Binop(se1, op, se2, A.Primitive(Char_t))
    *)
    | (A.Primitive(Int), A.Primitive(Int)) -> S.Binop(se1, op, se2, A.Primitive(Int))

    | _ -> raise (Exceptions.InvalidBinopExpression "Arithmetic operators don't support these types")
  in
  env, (
    match op with
    | And | Or -> get_logical_binop_type se1 se2 op (t1, t2)
    | Equal | Neq -> get_sast_equality_binop ()
    | Less | Leq | Greater | Geq -> get_comparison_binop_type t1 t2 se1 se2 op
    | Add | Mult | Sub | Div | Mod -> get_arithmetic_binop_type se1 se2 op (t1, t2))

and analyze_unop env op e = (* -> env, Uniop (op,e,_) *)
  let check_num_unop t = function
      Neg -> t
    | _ -> raise(Exceptions.InvalidUnaryOperation)
  in
  let check_bool_unop = function
      Not -> A.Primitive(Bool)
    | _ -> raise(Exceptions.InvalidUnaryOperation)
  in
  let _, se = build_sast_expr env e in
  let t = get_type_from_expr se in
  match t with
    A.Primitive(Int)
  | A.Primitive(Double) -> env, S.Uniop(op, se, check_num_unop t op)
  | A.Primitive(Bool) -> env, S.Uniop(op, se, check_bool_unop op)
  | _ -> raise(Exceptions.InvalidUnaryOperation)

and analyze_assign env e1 e2 =
  let _, lhs = build_sast_expr env e1 in
  let _, rhs = build_sast_expr env e2 in
  let t1 = get_type_from_expr lhs in
  let t2 = get_type_from_expr rhs in
  let rhs =
    if t1 = t2 then rhs
    else
      match t1, t2 with
      | Arraytype(d), Arraytype(Primitive(Unit)) -> S.LitArray([], d) (* it means e2 is [] *)
      | Structtype("Note"), _ -> S.LitNote(get_litpitch rhs, S.LitDuration(1, 4))
      | _ ->
        raise (Exceptions.AssignTypeMismatch(string_of_datatype t1, string_of_datatype t2))
  in
  env, S.Assign(lhs, rhs, t1)

and analyze_funccall env s el =
  let _, sast_el = build_sast_expr_list env el in
  try
    let func = StringMap.find s builtin_funcs in
    env, S.FuncCall(func.fname, sast_el, func.returnType)
  (* TODO: check builtin funcs *)
  (* such as, len() only accepts arrays *)
  with | Not_found ->
  try
    let fname = get_global_func_name env.name s in
    let func = StringMap.find fname !(env.btmodule).func_map in (* ast func *)
    let check_params (actuals : S.expr list) (formals : A.bind list) =
      if List.length actuals = List.length formals (* && *)
      then
        let paramList = List.map2 (fun i j -> get_type_from_expr i = fst j) actuals formals in
        if List.mem false paramList
        then
          raise (Exceptions.ParamTypeNotMatch "types of paramater differ")
        else
          true
      else raise (Exceptions.ParamNumberNotMatch "numbers of paramater differ")
    in
    if check_params sast_el func.formals
    then
      env, S.FuncCall(fname, sast_el, func.returnType)
    else
      raise (Exceptions.FuncCallCheckFail "funccall check failed")
  with | Not_found -> raise (Exceptions.FuncNotFound (env.name, s))
  (*
  let actuals = handle_params func.sformals sel in
  let actuals = handle_params f.formals sel in
  SCall(fname, actuals, func.sreturnType, 0)
  SCall(sfname, actuals, f.returnType, index)
  *)

let get_sast_structtype env s =
  let n =
    (* Builtin Struct *)
    if s = "Note" then s
    else get_global_name env.name s 
  in
  if not (StringMap.mem n !(env.btmodule).struct_map)
  then raise (Exceptions.UndefinedStructType n)
  else Structtype(n) (* rename Structtype using sast(global) name *)

let get_sast_arraytype env d =
  match d with
  | Structtype(s) -> Arraytype(get_sast_structtype env s)
  | _ -> Arraytype(d) (* so far there is no arraytype within arraytype !! *)

let build_sast_vardecl env t1 s e =
  let s =
    if env.ismain then get_global_name env.name s else s
  in
  if StringMap.mem s env.var_map || StringMap.mem s env.formal_map
  then raise (Exceptions.DuplicateVariable s)
  else
    let t1 =
      match t1 with
      | Primitive(Unit) -> raise (Exceptions.UnitTypeError)
      | Arraytype(d) -> get_sast_arraytype env d
      | Structtype(s) -> get_sast_structtype env s
      | _ -> t1
    in
    let _, sast_expr = build_sast_expr env e in
    let t2 = get_type_from_expr sast_expr in
    let sast_expr =
      if (t1 = t2) || (sast_expr = S.Noexpr) then sast_expr
      else
        match t1, t2 with
        (* Cast *)
        | Arraytype(d), Arraytype(Primitive(Unit)) ->
          S.LitArray([], d) (* it means e is [] *)
        | Primitive(Pitch), Primitive(Int) -> get_litpitch sast_expr
        | seq_ele_type, _ -> S.LitNote(get_litpitch sast_expr, LitDuration(1, 4))
        | _ ->
          raise (Exceptions.VardeclTypeMismatch(string_of_datatype t1, string_of_datatype t2))
    in
    env.var_map <- StringMap.add s t1 env.var_map;
    if env.ismain then (* add variable to module fields *)
      !(env.btmodule).field_map <- StringMap.add s t1 !(env.btmodule).field_map;
    env, S.VarDecl(t1, s, sast_expr)

let rec build_sast_block env = function
  | [] -> env, S.Block([])
  | _ as l ->
    let _, sl = build_sast_stmt_list env l in env, S.Block(sl)

and build_sast_stmt env (stmt : A.stmt) =
  match stmt with
  | Block sl -> build_sast_block env sl
  | Expr e -> let _, se = build_sast_expr env e in env, get_stmt_from_expr se
  | Return e -> build_sast_return e env
  | If (e, s1, s2) -> build_sast_if e s1 s2 env
  | For(e1, e2, e3, s) -> build_sast_for e1 e2 e3 s env
  | While(e, s) -> build_sast_while e s env
  | Break -> check_break env (* TODO: Need to check if in right context *)
  | Continue -> check_continue env (* TODO: Need to check if in right context *)
  | VarDecl(d, s, e) -> build_sast_vardecl env d s e
  | Struct _ -> env, S.Expr(Noexpr, A.Primitive(Unit)) (* skip structs *)

and build_sast_stmt_list env (stmt_list:A.stmt list) =
  let helper_stmt stmt =
    let sast_stmt = snd (build_sast_stmt env stmt) in
    sast_stmt (* env will be updated *)
  in
  let sast_stmt_list = List.map helper_stmt stmt_list in
  (* print_int (get_map_size env.var_map); *)
  env, sast_stmt_list

and build_sast_return e env =
  let _, se = build_sast_expr env e in
  let t = get_type_from_expr se in
  if t = env.env_returnType
  then env, S.Return(se, t)
  else raise (Exceptions.ReturnTypeMismatch(string_of_datatype t, string_of_datatype env.env_returnType))

and build_sast_if e s1 s2 env =
  let _, se = build_sast_expr env e in
  let _, ifbody = build_sast_stmt env s1 in
  let _, elsebody = build_sast_stmt env s2 in
  if (get_type_from_expr se) = A.Primitive(Bool)
  then env, S.If(se, ifbody, elsebody)
  else raise (Exceptions.InvalidConditionType)

and build_sast_for e1 e2 e3 s env =
  let old_val = env.env_in_for in
  env.env_in_for <- true;
  let _, se1 = build_sast_expr env e1 in
  let _, se2 = build_sast_expr env e2 in
  let _, se3 = build_sast_expr env e3 in
  let _, body = build_sast_stmt env s in
  check_condition se2;
  env.env_in_for <- old_val;
  env, S.For(se1, se2, se3, body)

and build_sast_while e s env =
  let old_val = env.env_in_while in
  env.env_in_while <- true;
  let _, se = build_sast_expr env e in
  let _, body = build_sast_stmt env s in
  check_condition se;
  env.env_in_while <- old_val;
  env, S.While(se, body)

and check_break env =
  if env.env_in_for || env.env_in_while then
    env, S.Break
  else
    raise Exceptions.CannotCallBreakOutsideOfLoop

and check_continue env =
  if env.env_in_for || env.env_in_while then
    env, S.Continue
  else
    raise Exceptions.CannotCallContinueOutsideOfLoop

let check_fbody fbody returnType =
  let len = List.length fbody in
  if len = 0 then true else
    let final_stmt = List.hd (List.rev fbody) in
    match returnType, final_stmt with
      A.Primitive(Unit), _   -> true
    |   _, S.Return(_, _)   -> true
    |   _                   -> false

let build_sast_func_decl btmodule_map mname btmodule_env ismain (func:A.func_decl) =
  let env =
    let formal_map =
      let helper_formal map formal =
        StringMap.add (snd formal) formal map
      in
      List.fold_left helper_formal StringMap.empty func.formals
    in
    {
      btmodule_map = btmodule_map;
      (* immutable in this env  *)
      name = mname; (* current module ?? does it change later ?? *)
      btmodule = btmodule_env; (* current module *)
      ismain = ismain;
      formal_map = formal_map;
      (* mutable in this env  *)
      var_map = StringMap.empty;
      env_returnType = func.returnType;
      env_in_for = false;
      env_in_while = false;
    }
  in
  let _, fbody = build_sast_stmt_list env func.body in
  if check_fbody fbody func.returnType
  then
    {
      S.fname = get_global_func_name mname func.fname;
      S.formals = func.formals;
      S.returnType = func.returnType;
      S.body = fbody;
    }
  else
    raise (Exceptions.CheckFbodyFail "check_fbody fail")

let build_sast_struct_decl mname btmodule_env struct_decl =
  let sname = get_global_name mname struct_decl.sname in
  (* Exceptions.DuplicateFunction *)
  !btmodule_env.struct_map <- (StringMap.add sname struct_decl !btmodule_env.struct_map);
  {
    sname = sname;
    fields = struct_decl.fields; (* TODO: rename struct **bind** with global name !!*)
  }

let build_sast btmodule_map (btmodule_list:A.btmodule list) =
  let build_sast_btmodule btmodule =
    let btmodule_env = ref (StringMap.find btmodule.mname btmodule_map) in
    let sast_structs =
      let sast_structs =
        let helper_struct_decl struct_rev_list = function
            Struct struct_decl ->
            let sast_struct =
              build_sast_struct_decl btmodule.mname btmodule_env struct_decl
            in sast_struct::struct_rev_list
          | _ -> struct_rev_list
        in
        let (main_func : A.func_decl) = List.hd btmodule.funcs in
        List.rev (List.fold_left helper_struct_decl [] main_func.body)
      in
      (* Note that there is only one copy of builtin_types in default module
         of Sast, which will be used in codegen *)
      if btmodule.mname = default_mname then builtin_types_list @ sast_structs
      else sast_structs
    in
    let sast_funcs =
      let helper_func_decl ismain func =
        build_sast_func_decl btmodule_map btmodule.mname btmodule_env ismain func
      in
      match btmodule.funcs with
      | [] ->  raise (Exceptions.Impossible "Each module has at least one func (main)")
      | hd :: tl ->
        (helper_func_decl true hd) :: (List.map (helper_func_decl false) tl)
    in
    {
      S.mname = btmodule.mname;
      S.structs = sast_structs;
      S.funcs = sast_funcs;
    }
  in
  List.map build_sast_btmodule btmodule_list

let build_btmodule_map (btmodule_list : A.btmodule list) =
  let build_btmodule_env map btmodule =
    let helper_func map func =
      let fname = get_global_func_name btmodule.mname func.fname in
      if (StringMap.mem fname map)
      then raise(Exceptions.DuplicateFunction(fname))
      else if (StringMap.mem (func.fname) builtin_funcs)
      then raise(Exceptions.CannotUseBuiltinFuncName(func.fname))
      else StringMap.add fname func map
    in
    StringMap.add btmodule.mname
      {
        func_map = List.fold_left helper_func StringMap.empty btmodule.funcs;
        (* Note that there is only one copy of builtin_types in default module
           of Sast!! *)
        struct_map = if btmodule.mname = default_mname then builtin_types
          else StringMap.empty;
        field_map = StringMap.empty;
      }
      map
  in
  List.fold_left build_btmodule_env StringMap.empty btmodule_list

let analyze_ast (btmodule_list) =
  let btmodule_map = build_btmodule_map btmodule_list in
  let sast_btmodule_list = build_sast btmodule_map btmodule_list in
  {
    S.btmodules = sast_btmodule_list;
  }
