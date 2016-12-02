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
 



(* let check_stmt_list (stmt : Ast.func_decl.body) = *)

(* type bind = datatype * string *)

(* let check_bind bind = *)


(* ------------------- SAST Utilities ------------------- *)
let get_type_from_expr (expr : S.expr) =
  match expr with
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
(*
  | Null -> Datatype(Null_t)
   | Noexpr -> Datatype(Void_t)
   | Assign(_, _, d) -> d
   | Unop(_, _, d) -> d
   | Binop(_, _, _, d) -> d *)

let get_stmt_from_expr e =
  let t = get_type_from_expr e in
  S.Expr(e, t)



(*
 **********************************************************
let rec check_expr e =
 match e with
 Binop of expr * binary_operator * expr
 | Uniop of unary_operator * expr
 | Assign of expr * expr
 | FuncCall of string * expr list
 | _ -> ();
 *)

(*
let to_ast_expr = function
    Id (s,_) -> A.Id(s)
  | LitBool b -> A.LitBool(b)
  | LitInt i -> A.LitInt(i)
  | LitDouble f -> A.LitDouble(f)
  | LitStr s -> A.LitStr(s)
  | Binop (e1,op,e2,_) -> A.Binop(e1,op,e2)
  | Uniop (op,e,_) -> A.Uniop(op,e)
  | Assign (e1,e2,_) -> A.Assign(e1,e2)
  | FuncCall (s,el,_) -> A.FuncCall(s,el)
  | Noexpr -> A.Noexpr
  | Null -> A.Null
 *)

let rec check_stmt returnType (stmt : S.stmt) =
  match stmt with
    Block sl -> List.iter (check_stmt returnType) sl
  (* | Expr e -> check_expr e *)
  (* | If (e, s, s) -> *)
  (* | While of expr * stmt *)
  | Return (e,_) -> if get_type_from_expr e != returnType then raise (Exceptions.ReturntypeNotMatch "foo"); ()
  (* | Break -> () *)
  (* | Continue -> () *)
  | VarDecl (d, _, e) -> if get_type_from_expr e != d then raise (Exceptions.VariableDeclarationNotMatch "foo"); ()
  | _ -> ()

let check_func (btfunc : S.func_decl) =
  (* List.iter check_bind func.formals; *)
  List.iter (check_stmt btfunc.returnType) btfunc.body

let analyze program (btmodule : S.btmodule) =
  List.iter check_func btmodule.funcs


(* ------------------- check sast ------------------- *)
(* DONE *)
let check_vardecl_type d sast_expr =
  let t = get_type_from_expr sast_expr in
  if d = t
  then true
  else false

(* ------------------- debug ------------------- *)


let get_map_size map =
  StringMap.fold (fun k v i -> i + 1) map 0

(* ------------------- build sast from ast ------------------- *)

let get_global_func_name mname (func:A.func_decl) =
  if mname = A.default_mname && func.fname = A.default_fname
  then "main"
  else mname ^ "." ^ func.fname (* module.main *)
(* We use '.' to separate types so llvm will recognize the function name and it won't conflict *)

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


let rec build_sast_expr env (expr : A.expr) =
  match expr with
    Id(s) -> env, S.Id(s, get_ID_type env s)
  | LitBool(b) -> env, S.LitBool(b)
  | LitInt(i) -> env, S.LitInt(i)
  | LitDouble(f) -> env, S.LitDouble(f)
  | LitStr(s) -> env, S.LitStr(s)
  | Binop(e1,op,e2) -> analyze_binop env e1 op e2
  | Uniop(op,e) -> analyze_unop env op e
  | Assign(e1,e2) -> analyze_assign env e1 e2
  | FuncCall(s,el) -> (* Chord::func() ?? *)
    analyze_funccall env s el (* env, FuncCall (s,el,_) *)
  | Noexpr -> env, S.Noexpr
  | Null -> env, S.Null

and build_sast_expr_list env (expr_list:A.expr list) =
  let helper_expr expr = snd (build_sast_expr env expr) in
  let sast_expr_list = List.map helper_expr expr_list in
  (* print_int (get_map_size env.var_map); *)
  env, sast_expr_list

(* --- Analyze expressions --- *)

and analyze_binop env e1 op e2 =
  (* env, S.Noexpr  *)
  (* env, Binop (e1,op,e2,_) *)
  
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
 

and analyze_unop env op e =
  env, S.Noexpr (* env, Uniop (op,e,_) *)
  (*
    let check_num_unop t = function
        Sub -> t
      | _ -> raise(Exceptions.InvalidUnaryOperation)
    in
    let check_bool_unop = function
        Not -> Datatype(Bool_t)
      | _ -> raise(Exceptions.InvalidUnaryOperation)
    in
    let se, env = expr_to_sexpr env e in
    let t = get_type_from_sexpr se in
    match t with
      Datatype(Int)
    | Datatype(Double) -> S.Unop(op, se, check_num_unop t op)
    | Datatype(Bool_t) -> S.Unop(op, se, check_bool_unop op)
    | _ -> raise(Exceptions.InvalidUnaryOperation)
  *)

and analyze_assign env e1 e2 =
  let _, se1 = build_sast_expr env e1 in
  let _, se2 = build_sast_expr env e2 in
  let t1 = get_type_from_expr se1 in
  let t2 = get_type_from_expr se2 in
  (* DONE: check type *)
  if t1 = t2(* check_vardecl_type t1 se1 && check_vardecl_type t2 se2 *)
  then 
    env, S.Assign(se1, se2, t1)
  else
    raise (Exceptions.VarDeclCheckFail "type check fail")

and analyze_funccall env s el =
  let _, sast_el = build_sast_expr_list env el in
  try
    let func = StringMap.find s env.builtin_funcs in
    env, S.FuncCall(func.fname, sast_el, func.returnType)
    (* TODO: check builtin funcs *)
  with | Not_found ->
  try
    let fname = env.name ^ "." ^ s in
    let func = StringMap.find fname env.btmodule.func_map in (* ast func *)
    let foo (actuals : S.expr list) (formals : A.bind list) = 
      (* todo: type checks as well ?list.iter typecheck each returntype? *)
      if List.length actuals = List.length formals (* && *)
      then
        true 
      else
        false
    in 
    if foo sast_el func.formals 
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


let build_sast_vardecl env d s e =
  if StringMap.mem s env.var_map 
  then 
    raise (Exceptions.DuplicateLocal s)
  else
    let _, sast_expr = build_sast_expr env e in
    if (sast_expr = S.Noexpr) || (check_vardecl_type d sast_expr)
    then
      (* TODO: check if t is Unit jakeQ*)
      (* semant.ml's handle_expr_statement *)
      (* dice, analyzer's local_handler *)
      env.var_map <- StringMap.add s d env.var_map;
    (* print_int (get_map_size env.var_map); *)
    env, S.VarDecl(d, s, sast_expr)
(* TODO (NOT YET): if the user-defined type being declared is not in global classes map, it is an undefined class *)


let rec build_sast_block env = function
    [] -> env, S.Block([])
  | _ as l ->
    let _, sl = build_sast_stmt_list env l in env, S.Block(sl) (* is env updated? *)

and build_sast_stmt env (stmt : A.stmt) =
  match stmt with
    Block sl -> build_sast_block env sl
  | Expr e ->
    let _, se = build_sast_expr env e in
    env, get_stmt_from_expr se
  (* | Return e -> check_return e env
     | If(e, s1, s2) -> check_if e s1 s2	env
     | For(e1, e2, e3, e4) -> check_for e1 e2 e3 e4 env
     | While(e, s) -> check_while e s env
     | Break -> check_break env (* TODO: Need to check if in right context *)
     | Continue -> check_continue env (* TODO: Need to check if in right context *) *)
  | VarDecl(d, s, e) -> build_sast_vardecl env d s e

and build_sast_stmt_list env (stmt_list:A.stmt list) =
  let helper_stmt stmt =
    let sast_stmt = snd (build_sast_stmt env stmt) in
    sast_stmt (* env will be updated *)
  in
  let sast_stmt_list = List.map helper_stmt stmt_list in
  (* print_int (get_map_size env.var_map); *)
  env, sast_stmt_list


let build_sast_func_decl btmodule_map btmodule_env mname (func:A.func_decl) =
  let env =
    let formal_map =
      let helper_formal map formal =
        StringMap.add (snd formal) formal map
      in
      List.fold_left helper_formal StringMap.empty func.formals
    in
    (* initialize environment per func ?? *)
    {
      builtin_funcs = builtin_funcs;
      name = mname; (* current module *)
      var_map = StringMap.empty; (* why empty, fields? *)
      formal_map = formal_map; (* current func *)
      btmodule = btmodule_env; (* current module *)
      btmodule_map = btmodule_map;
    }
  in
  let _, fbody = build_sast_stmt_list env func.body in
  (* TODO: check_fbody *)
  if true (* here *)
  then
    {
      S.fname = get_global_func_name mname func;
      S.formals = func.formals;
      S.returnType = func.returnType; (*??*)
      S.body = fbody;
    }
  else 
    raise (Exceptions.CheckFbodyFail "check_fbody fail")


let build_sast btmodule_map (btmodule_list:A.btmodule list) =
  let build_sast_btmodule btmodule =
    let btmodule_env = StringMap.find btmodule.mname btmodule_map in
    let sast_funcs =
      let helper_func_decl func =
        build_sast_func_decl btmodule_map btmodule_env btmodule.mname func
      in
      List.map helper_func_decl btmodule.funcs
    in
    {
      S.mname = btmodule.mname;
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
      (* user_type ?? *)
    }


(* ref: build_class_maps - Generate list of all classes to be used for semantic checking *)
let build_btmodule_map (btmodule_list : A.btmodule list) =
  (* reserved/default module?? *)
  let build_btmodule_env map btmodule =
    let helper_func map func =
      (* Exceptions.CannotUseReservedFuncName *)
      (* Exceptions.DuplicateFunction *)
      StringMap.add (get_global_func_name btmodule.mname func) func map
    in
    StringMap.add btmodule.mname
      {
        func_map = List.fold_left helper_func StringMap.empty btmodule.funcs;
        (* decl = btmodule; *)
        (* fields hashtbl ?? *)
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



