(*
 * Authors:
 *  - Ruonan Xu
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


(* BINARY TYPES *)

let get_equality_binop_type type1 type2 se1 se2 op =
  (* Equality op not supported for float operands. The correct way to test floats
     for equality is to check the difference between the operands in question *)
  if (type1 = A.Datatype(Double) || type2 = A.Datatype(Double)) then raise (Exceptions.InvalidBinopExpression "Equality operation is not supported for Double types")
  else
    match type1, type2 with(*
      A.Datatype(Char_t), Datatype(Int)
    | Datatype(Int), Datatype(Char_t) -> env, S.Binop(se1, op, se2, A.Datatype(Bool)) *)
    | _ ->
      if type1 = type2 then S.Binop(se1, op, se2, A.Datatype(Bool))
      else raise (Exceptions.InvalidBinopExpression "Equality operator can't operate on different types")

let get_logical_binop_type se1 se2 op = function
    (A.Datatype(Bool), A.Datatype(Bool)) -> S.Binop(se1, op, se2, A.Datatype(Bool))
  | _ -> raise (Exceptions.InvalidBinopExpression "Logical operators only operate on Bool types")

let get_comparison_binop_type type1 type2 se1 se2 op =
  let numerics = SS.of_list [A.Datatype(Int); A.Datatype(Double)]
  in
  if SS.mem type1 numerics && SS.mem type2 numerics
  then S.Binop(se1, op, se2, A.Datatype(Bool))
  else raise (Exceptions.InvalidBinopExpression "Comparison operators operate on numeric types only")

let get_arithmetic_binop_type se1 se2 op = function
    (A.Datatype(Int), A.Datatype(Double))
  | (A.Datatype(Double), A.Datatype(Int))
  | (A.Datatype(Double), A.Datatype(Double)) -> S.Binop(se1, op, se2, A.Datatype(Double))
  (* | (A.Datatype(Int), A.Datatype(Char_t))
     | (A.Datatype(Char_t), A.Datatype(Int))
     | (A.Datatype(Char_t), A.Datatype(Char_t)) -> S.Binop(se1, op, se2, A.Datatype(Char_t))
  *)
  | (A.Datatype(Int), A.Datatype(Int)) -> S.Binop(se1, op, se2, A.Datatype(Int))

  | _ -> raise (Exceptions.InvalidBinopExpression "Arithmetic operators don't support these types")


(* ------------------- SAST Utilities ------------------- *)
let get_type_from_expr (expr : S.expr) =
  match expr with
    Id (_,d) -> d
  | StructField(_,_,d) -> d
  | LitBool(_) -> A.Datatype(Bool)
  | LitInt(_) -> A.Datatype(Int)
  | LitDouble(_) -> A.Datatype(Double)
  | LitStr(_) -> A.Datatype(String)
  | LitPitch(_,_,_) -> A.Musictype(Pitch)
  | Null -> A.Datatype(Unit) (* Null -> Datatype(Null_t) *)
  | Binop(_,_,_,d) -> d
  | Uniop(_,_,d) -> d
  | Assign(_,_,d) -> d
  | FuncCall(_,_,d)-> d
  | Noexpr -> A.Datatype(Unit)
  | LitArray(_,d) -> Arraytype(d)
  | ArrayIdx(_,_,d) -> d
  | ArraySub(_,_,_,d) -> d

let get_stmt_from_expr e =
  let t = get_type_from_expr e in
  S.Expr(e, t)

(* ------------------- check sast ------------------- *)
let check_vardecl_type d sast_expr =
  let t = get_type_from_expr sast_expr in
  d = t

(* ------------------- debug ------------------- *)


let get_map_size map =
  StringMap.fold (fun k v i -> i + 1) map 0

(* ------------------- build sast from ast ------------------- *)

let get_global_func_name mname (func:A.func_decl) =
  if mname = A.default_mname && func.fname = A.default_fname
  then "main"
  else mname ^ "." ^ func.fname (* module.main *)
(* We use '.' to separate types so llvm will recognize the function name and it won't conflict *)

let get_global_name mname n =
  if mname = default_mname then n
  else mname ^ "." ^ n

(* Initialize builtin_types *)
let (builtin_types_list : A.struct_decl list) =
  [{
    A.sname = "pitch";
    A.fields = [(A.Datatype(String), "key"); (A.Datatype(Int), "octave"); (A.Datatype(Int), "alter");];
  };]

let builtin_types =
  let add_to_map builtin_type map =
    StringMap.add builtin_type.sname builtin_type map
  in
  List.fold_right add_to_map builtin_types_list StringMap.empty

(* Initialize builtin_funcs *)
let builtin_funcs =
  let map = StringMap.empty in
  let map = StringMap.add "print"
      {
        S.fname = "printf";
        S.formals = [];
        S.returnType = A.Datatype(A.Int);
        S.body = [];
      }
      map in
  map
(*
(* these are builtin_funcs  *)
let add_reserved_functions =
  let reserved_stub name return_type formals =
    {
      formals    = formals;
    }
  in
    (* reserved_stub "print"   (Unit)  ([Many(Any)]); *)
    (* reserved_stub "sizeof"  (i32_t)   ([mf Any "in"]); *)
    (* reserved_stub "open"  (i32_t)   ([mf str_t "path"; mf i32_t "flags"]); *)
    (* reserved_stub "input"   (str_t)   ([]); *)
  ] in
  reserved
*)

(* ref: Dice/check_obj_access *)
let analyze_struct env s f =
  let struct_type = get_ID_type env s in
  let field_bind =
    let struct_decl =
      match struct_type with
        Structtype n -> (
          try StringMap.find n !(env.btmodule).struct_map
          with | Not_found -> raise(Exceptions.Impossible("analyze_struct")))
      | _ as d -> raise (Exceptions.ShouldAccessStructType (string_of_datatype d))
    in
    try List.find (fun field -> (snd field) = f) struct_decl.fields
    with | Not_found -> raise(Exceptions.StructFieldNotFound(s, f))
  in
  let field_id = S.Id(snd field_bind, fst field_bind) in
  let struct_id = S.Id(s, struct_type) in
  env, S.StructField(struct_id, field_id, fst field_bind)

let rec build_sast_expr env (expr : A.expr) =
  match expr with
    Id(s) -> env, S.Id(s, get_ID_type env s)
  | StructField(s, f) -> analyze_struct env s f
  | LitBool(b) -> env, S.LitBool(b)
  | LitInt(i) -> env, S.LitInt(i)
  | LitDouble(f) -> env, S.LitDouble(f)
  | LitStr(s) -> env, S.LitStr(s)
  | LitPitch(k, o, a) -> env, S.LitPitch(k,o,a)
  | Binop(e1, op, e2) -> analyze_binop env e1 op e2
  | Uniop(op, e) -> analyze_unop env op e
  | Assign(e1, e2) -> analyze_assign env e1 e2
  | FuncCall(s, el) -> (* TODO: Chord::func() ?? *)
    analyze_funccall env s el (* env, FuncCall (s,el,_) *)
  | Noexpr -> env, S.Noexpr
  | Null -> env, S.Null
  | LitArray(el) -> analyze_array env el
  | ArrayIdx(a, e) -> analyze_arrayidx env a e
  | ArraySub(a, e1, e2) -> analyze_arraysub env a e1 e2

and build_sast_expr_list env (expr_list:A.expr list) =
  let helper_expr expr = snd (build_sast_expr env expr) in
  let sast_expr_list = List.map helper_expr expr_list in
  (* print_int (get_map_size env.var_map); *)
  env, sast_expr_list

and analyze_array env (expr_list:A.expr list) =
  let _, sast_expr_list = build_sast_expr_list env expr_list in
  if List.length sast_expr_list = 0 then
    env, S.LitArray([], Any)
  else
    let ele_type =
      let ele_type = get_type_from_expr (List.hd sast_expr_list) in
      match ele_type with
      | Arraytype(d) -> d
      | _ -> ele_type
    in
    let sast_expr_list =
      let helper_array l expr =
        let d =
          match get_type_from_expr expr with
          | Arraytype(d) -> d
          | _ as d -> d
        in
        if d = ele_type or d = Any then
          match expr with
          | LitArray(el, _) -> (List.rev el) @ l
          | _ as e -> e :: l
        else raise (Exceptions.ArrayTypeNotMatch(string_of_datatype d))
      in
      List.rev (List.fold_left helper_array [] sast_expr_list)
    in
    env, S.LitArray(sast_expr_list, ele_type)

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
  match d with
  | Arraytype(_) -> (
      let _, idx1 = build_sast_expr env e1 in
      let _, idx2 = build_sast_expr env e2 in
      (* TODO J: check Python-like idx *)
      env, S.ArraySub(sast_arr, idx1, idx2, d)
    )
  | _ -> raise (Exceptions.ShouldAccessArray(string_of_datatype d))

and analyze_binop env e1 op e2 = (* -> env, Binop (e1,op,e2,t) *)
  let _, se1 = build_sast_expr env e1 in
  let _, se2 = build_sast_expr env e2 in
  let t1 = get_type_from_expr se1 in
  let t2 = get_type_from_expr se2 in
  match op with
    Equal | Neq                     -> env, get_equality_binop_type t1 t2 se1 se2 op
  | And | Or                        -> env, get_logical_binop_type se1 se2 op (t1, t2)
  | Less | Leq | Greater | Geq      -> env, get_comparison_binop_type t1 t2 se1 se2 op
  | Add | Mult | Sub | Div | Mod    -> env, get_arithmetic_binop_type se1 se2 op (t1, t2)
  | _                               -> raise (Exceptions.InvalidBinopExpression ((string_of_op op) ^ " is not a supported binary op"))

and analyze_unop env op e = (* -> env, Uniop (op,e,_) *)
  let check_num_unop t = function
      Neg -> t
    | _ -> raise(Exceptions.InvalidUnaryOperation)
  in
  let check_bool_unop = function
      Not -> A.Datatype(Bool)
    | _ -> raise(Exceptions.InvalidUnaryOperation)
  in
  let _, se = build_sast_expr env e in
  let t = get_type_from_expr se in
  match t with
    A.Datatype(Int)
  | A.Datatype(Double) -> env, S.Uniop(op, se, check_num_unop t op)
  | A.Datatype(Bool) -> env, S.Uniop(op, se, check_bool_unop op)
  | _ -> raise(Exceptions.InvalidUnaryOperation)

and analyze_assign env e1 e2 =
  let _, lhs = build_sast_expr env e1 in
  let _, rhs = build_sast_expr env e2 in
  let t1 = get_type_from_expr lhs in
  let t2 = get_type_from_expr rhs in
  if t1 = t2 or t2 = Any
  then env, S.Assign(lhs, rhs, t1)
  else
    raise (Exceptions.AssignmentTypeMismatch(string_of_datatype t1, string_of_datatype t2))

and analyze_funccall env s el =
  let _, sast_el = build_sast_expr_list env el in
  try
    let func = StringMap.find s env.builtin_funcs in
    env, S.FuncCall(func.fname, sast_el, func.returnType)
  (* TODO: check builtin funcs *)
  with | Not_found ->
  try
    let fname = env.name ^ "." ^ s in
    let func = StringMap.find fname !(env.btmodule).func_map in (* ast func *)
    let check_params (actuals : S.expr list) (formals : A.bind list) =
      if List.length actuals = List.length formals (* && *)
      then
        (* TODO: type checks as well ?list.iter typecheck each returntype? *)
        true
      else false
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


let get_sast_structtype env = function
    Structtype(sname) ->
    let n = get_global_name env.name sname in
    if not (StringMap.mem n !(env.btmodule).struct_map)
    then raise (Exceptions.UndefinedStructType n)
    else Structtype(n) (* rename Structtype using sast(global) name *)

let build_sast_vardecl env d s e =
  if StringMap.mem s env.var_map
  then
    raise (Exceptions.DuplicateVariable s)
  else
    let d =
      match d with
        Structtype(_) -> get_sast_structtype env d
      | _ -> d
    in
    let _, sast_expr = build_sast_expr env e in
    if (sast_expr = S.Noexpr) || (check_vardecl_type d sast_expr)
    then
      (* TODO: check if t is Unit *)
      (*
      if  get_type_from_expr sast_expr = A.Datatype(Unit)
      then
      raise (Exceptions.UnitTypeError "UnitTypeError")
      (* semant.ml's handle_expr_statement *)
      (* ref: Dice/local_handler *)
      else
       *)
      env.var_map <- StringMap.add s d env.var_map;
    (* print_int (get_map_size env.var_map); *)
    env, S.VarDecl(d, s, sast_expr)



let rec build_sast_block env = function
    [] -> env, S.Block([])
  | _ as l ->
    let _, sl = build_sast_stmt_list env l in env, S.Block(sl)

and build_sast_stmt env (stmt : A.stmt) =
  match stmt with
    Block sl -> build_sast_block env sl
  | Expr e -> let _, se = build_sast_expr env e in env, get_stmt_from_expr se
  | Return e -> check_return e env
  | If (e, s1, s2) -> check_if e s1 s2 env
  (* | If (se, s1, s2) -> check_stmt se s1 env *)
  (* | For(e1, e2, e3, e4) -> check_for e1 e2 e3 e4 env *)
  | While(e, s) -> check_while e s env
  | Break -> check_break env (* TODO: Need to check if in right context *)
  | Continue -> check_continue env (* TODO: Need to check if in right context *)
  | VarDecl(d, s, e) -> build_sast_vardecl env d s e
  | Struct _ -> env, S.Expr(Noexpr, A.Datatype(Unit)) (* skip structs *)


and build_sast_stmt_list env (stmt_list:A.stmt list) =
  let helper_stmt stmt =
    let sast_stmt = snd (build_sast_stmt env stmt) in
    sast_stmt (* env will be updated *)
  in
  let sast_stmt_list = List.map helper_stmt stmt_list in
  (* print_int (get_map_size env.var_map); *)
  env, sast_stmt_list

(* and check_if e s1 s2 env =
   let _, se = build_sast_expr env e in
   let t = get_type_from_expr se in
   let _, ifbody = build_sast_stmt env s1 in
   let _, elsebody = build_sast_stmt env s2 in
   if t = A.Datatype(Bool)
    then env, S.If(se, ifbody, elsebody)
    else raise (Exceptions.IfComparisonNotBool "foo") *)

(* and check_while e s env =
   let old_val = env.env_in_while in
   let env = update_call_stack env env.env_in_for true in

   let _, se = build_sast_expr env e in
   let t = get_type_from_expr se in
   let sstmt, _ = parse_stmt env s in
   let swhile =
    if (t = A.Datatype(Bool) || t = A.Datatype(Unit))
      then S.While(se, sstmt)
      else raise Exceptions.InvalidWhileStatementType
   in

   let env = update_call_stack env env.env_in_for old_val in
   swhile, env *)


and check_sblock sl env = match sl with
    []  -> S.Block([S.Expr(S.Noexpr, A.Datatype(Unit))])
  | _   ->
    let sl, _ = convert_stmt_list_to_sstmt_list env sl in
    S.Block(sl)

and check_expr_stmt e env =
  let _, se = build_sast_expr env e in
  let t = get_type_from_expr se in
  env, S.Expr(se, t)

and check_return e env =
  let _, se = build_sast_expr env e in
  let t = get_type_from_expr se in
  match t, env.env_returnType with
  (* A.Datatype(Unit), Datatype(Objecttype(_))
     |   Datatype(Null_t), Arraytype(_, _) -> SReturn(se, t), env *)
  |   _ ->
    if t = env.env_returnType
    then env, S.Return(se, t)
    else raise (Exceptions.ReturnTypeMismatch(string_of_datatype t, string_of_datatype env.env_returnType))

and check_if e s1 s2 env =
  let _, se = build_sast_expr env e in
  let t = get_type_from_expr se in
  let _, ifbody = build_sast_stmt env s1 in
  let _, elsebody = build_sast_stmt env s2 in
  if t = A.Datatype(Bool)
  then env, S.If(se, ifbody, elsebody)
  else raise (Exceptions.IfComparisonNotBool "foo")
(*
and check_for e1 e2 e3 s env =
  let old_val = env.env_in_for in
  (* let env = update_call_stack env true env.env_in_while in *)
  env.env_in_for <- true;
  let _, se1 = build_sast_expr env e1 in
  let _, se2 = build_sast_expr env e2 in
  let _, se3 = build_sast_expr env e3 in
  let forbody, _ = parse_stmt env s in
  let conditional = get_type_from_expr se2 in
  let sfor =
    if (conditional = A.Datatype(Bool) || conditional = A.Datatype(Unit))
      then S.For(se1, se2, se3, forbody)
      else raise (Exceptions.InvalidForStatementType "foo")
  in

  (* let env = update_call_stack env old_val env.env_in_while in *)
  env.env_in_for <- old_val;
  sfor, env
 *)
and check_while e s env =
  let old_val = env.env_in_while in
  (* let env = update_call_stack env env.env_in_for true in *)
  env.env_in_while <- true;

  let _, se = build_sast_expr env e in
  let t = get_type_from_expr se in
  let _, sstmt = parse_stmt env s in
  let swhile =
    if (t = A.Datatype(Bool) || t = A.Datatype(Unit))
    then S.While(se, sstmt)
    else raise Exceptions.InvalidWhileStatementType
  in

  (* let env = update_call_stack env env.env_in_for old_val in *)
  env.env_in_while <- old_val;
  env, swhile

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

and parse_stmt env = function
    Block sl            -> env, check_sblock sl env
  |   Expr e              -> check_expr_stmt e env
  |   Return e            -> check_return e env
  |   If(e, s1, s2)       -> check_if e s1 s2 env
  (* |   For(e1, e2, e3, e4) -> check_for e1 e2 e3 e4 env   *)
  |   While(e, s)         -> check_while e s env
  |   Break               -> check_break env (* Need to check if in right context *)
  |   Continue            -> check_continue env (* Need to check if in right context *)
(* |   Local(d, s, e)      -> local_handler d s e env *)
(* | VarDecl (d, _, e) -> if get_type_from_expr e != d then raise (Exceptions.VariableDeclarationNotMatch "foo"); ()
   | If (e, _, _) -> if get_type_from_expr e != A.Datatype(A.Bool) then raise (Exceptions.IfComparisonNotBool "foo"); ()
   | Return (e,_) -> if get_type_from_expr e != returnType then raise (Exceptions.ReturntypeNotMatch "foo"); ()
*)

(* Update this function to return an env object *)
and convert_stmt_list_to_sstmt_list env stmt_list =
  let env_ref = ref(env) in
  let rec iter = function
      head::tail ->
      let env, a_head = parse_stmt !env_ref head in
      env_ref := env;
      a_head::(iter tail)
    | [] -> []
  in
  let sstmt_list = (iter stmt_list), !env_ref in
  sstmt_list

let check_fbody fbody returnType =
  let len = List.length fbody in
  if len = 0 then true else
    let final_stmt = List.hd (List.rev fbody) in
    match returnType, final_stmt with
      A.Datatype(Unit), _   -> true
    |   _, S.Return(_, _)   -> true
    |   _                   -> false




let build_sast_func_decl btmodule_map btmodule_env mname (func:A.func_decl) =
  let env =
    let formal_map =
      let helper_formal map formal =
        StringMap.add (snd formal) formal map
      in
      List.fold_left helper_formal StringMap.empty func.formals
    in
    (* initialize a new environment for every func *)
    {
      (* same for all envs *)
      builtin_funcs = builtin_funcs;
      btmodule_map = btmodule_map;
      (* immutable in this env  *)
      name = mname; (* current module ?? does it change later ?? *)
      btmodule = btmodule_env; (* current module *)
      formal_map = formal_map;
      (* mutable in this env  *)
      var_map = StringMap.empty; (* why empty, fields? *)
      env_returnType = func.returnType;
      env_in_for = false;
      env_in_while = false;
    }
  in
  let _, fbody = build_sast_stmt_list env func.body in
  if check_fbody fbody func.returnType
  then
    {
      S.fname = get_global_func_name mname func;
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
    fields = struct_decl.fields;
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
      if btmodule.mname = default_mname then builtin_types_list @ sast_structs
      else sast_structs
    in
    let sast_funcs =
      let helper_func_decl func =
        build_sast_func_decl btmodule_map btmodule_env btmodule.mname func
      in
      List.map helper_func_decl btmodule.funcs
    in
    {
      S.mname = btmodule.mname;
      S.structs = sast_structs;
      S.funcs = sast_funcs;
    }
  in
  let sast_btmodule_list = List.map build_sast_btmodule btmodule_list in
  match sast_btmodule_list with
    [] -> raise (Exceptions.ShouldNotHappenUnlessCompilerHasBug "no default module")
  | head::tail ->
    {
      S.main_module = head;
      S.btmodules = tail;
    }


(* ref: build_class_maps - Generate map of all modules to be used for semantic checking *)
let build_btmodule_map (btmodule_list : A.btmodule list) =
  (* default module?? *)
  let build_btmodule_env map btmodule =
    let helper_func map func =
      (* Exceptions.DuplicateFunction *)
      if (StringMap.mem (get_global_func_name btmodule.mname func) map)
      then raise(Exceptions.DuplicateFunction(get_global_func_name btmodule.mname func))
      else if (StringMap.mem (func.fname) builtin_funcs)
      then raise(Exceptions.CannotUseBuiltinFuncName(func.fname))
      else StringMap.add (get_global_func_name btmodule.mname func) func map
    in
    StringMap.add btmodule.mname
      {
        func_map = List.fold_left helper_func StringMap.empty btmodule.funcs;
        struct_map = if btmodule.mname = default_mname then builtin_types
          else StringMap.empty;
      }
      map
  in
  List.fold_left build_btmodule_env StringMap.empty btmodule_list

let analyze_ast (btmodule_list) =
  let btmodule_map = build_btmodule_map btmodule_list in
  let sast = build_sast btmodule_map btmodule_list in
  sast

(* typed_ast.ml *)

(* ------------------------------------------------ *)


(*
let check (btmodule) =
 (* Raise an exception if the given list has a duplicate *)
 let report_duplicate exceptf list =
 let rec helper = function
 n1 :: n2 :: _ when n1 = n2 -> raise (Failure (exceptf n1))
 | _ :: t -> helper t
 | [] -> ()
 in helper (List.sort compare list)
 in

 (* Raise an exception if a given binding is to a Unit type *)
 let check_not_Unit exceptf = function
 (Primitive(Unit), n) -> raise (Failure (exceptf n))
 | _ -> ()
 in

 (* Raise an exception of the given rvalue type cannot be assigned to
 the given lvalue type *)
 let check_assign lvaluet rvaluet err =
 if lvaluet == rvaluet then lvaluet else raise err
 in

 List.iter (check_not_Unit (fun n -> "illegal Unit global " ^ n)) btmodule.funcs.formals;

 report_duplicate (fun n -> "duplicate global " ^ n) (List.map snd btmodule.funcs.formals);
*)
