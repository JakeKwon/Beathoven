open Ast;;
open Environment;;


(* module StringMap = Map.Make(String) *)


exception ReturnStatementMissing;;
exception ImproperBraceSelection;;
exception ImproperBraceSelectorType;;
exception MultiDimensionalArraysNotAllowed;;
exception NotBoolExpr;;
exception BadBinopType;;
exception IncorrectWhereType;;
exception UpdatingBool;;
exception IncorrectSelectorId;;
exception UniterableType;;


(* Semantic checking of a program. Returns Unit if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)
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



let ast_data_to_data (dt : Ast.data_type) = match dt
  with Int -> Int
  | Double -> Double
  | Bool -> Bool
  | String -> String

let ast_data_to_string (dt : Ast.data_type) = match dt
  with Int -> "int"
  | Double -> "Double"
  | Bool -> "bool"
  | String -> "string"

let data_to_ast_data (dt : data_type) = match dt
  with Int -> Ast.Int
  | Double -> Ast.Double
  | Bool -> Ast.Bool
  | String -> Ast.String

let string_to_data_type (s : string) = match s
  with "int" -> Int
  | "Double" -> Double
  | "bool" -> Bool
  | "string" -> String
  | _ -> raise (Failure "Unsupported Data Type.")

let string_data_literal (expr : Ast.expr) = match expr
    with Literal_int(i) -> string_of_int i
  | Literal_Double(i) -> string_of_Double i
  | Literal_bool(i) -> i
  | Literal_string(i) -> i
  | _ -> raise (Failure "There is no defined way to print this expression.")

let check_binop_type (left_expr : data_type) (op : Ast.math_op) (right_expr : data_type) = match (left_expr, op, right_expr)
  with (Int, _, Int) -> Int
  | (Double, _, Double) -> Double
  | (String, Add, String) -> String
  | (_, _, _) ->
    raise (Failure "Cannot perform binary operations with provided arguments.")

(*Possibly add int/Double comparison*)
let check_bool_expr_binop_type (left_expr : data_type) (op : Ast.bool_op) (right_expr : data_type) = match op
    with Equal | Neq -> (match (left_expr, right_expr)
      with (Int, Int) -> Bool
      | (Double, Double) -> Bool
      | (String, String) -> Bool
      | (Bool, Bool) -> Bool
      | _ -> raise (Failure "Cannot perform boolean binary operations with provided arguments.")
      )
    | Less | Leq | Greater | Geq -> (match (left_expr, right_expr)
      with (Int, Int) -> Bool
      | (Double, Double) -> Bool
      | _ -> raise (Failure "Cannot perform comparison binary operations with provided arguments.")
      )

let rec check_bracket_select_type (d_type : data_type) (selectors : expr list) (env : symbol_table) (id : string) (serial : string) = match d_type
  with Array ->
    if List.length selectors != 1 then raise MultiDimensionalArraysNotAllowed;
    (* We can ignore the env because we're explicitly updating later. *)
    let (expr_type, _) = check_expr_type (List.hd selectors) (env) in
    if expr_type != Int then raise ImproperBraceSelectorType;
    let ast_array_type = array_type (id) (env) in
    let data_type = ast_data_to_data ast_array_type in  

and check_expr_type (expr : Ast.expr) (env: Environment.symbol_table) = match expr
  with Literal_int(i) -> (Int,env)
  | Literal_double(i) -> (Double,env)
  | Literal_bool(i) -> (Bool,env)
  | Literal_string(i) -> (String,env)
  | Binop(left_expr, op, right_expr) ->
    let (left_type, left_env) = (check_expr_type left_expr env) in
    let (right_type, right_env) = (check_expr_type right_expr left_env) in
    let resulting_type = check_binop_type left_type op right_type in
    (match (left_type, right_type) with
      | (_,_) -> (resulting_type, right_env))
  | Id(i) -> (ast_data_to_data((var_type i env)), env)
  | Call(func_name, arg_list) ->
    let arg_types = List.map (fun expr ->
      let (expr_type, _) = (check_expr_type (expr) (env)) in
      (data_to_ast_data(expr_type))) arg_list in
    verify_func_call func_name arg_types env;
    let func_return_type = func_return_type func_name env in (match func_name
      with "print" -> (ast_data_to_data(func_return_type), env)
      | "length" -> (ast_data_to_data(func_return_type), env)
    )
  | Bracket_select(id, selectors) ->
    let selector_ast_data_type = var_type id env in
    let selector_data_type = ast_data_to_data selector_ast_data_type in
    let serialized = serialize (expr) (env) in
    (check_bracket_select_type (selector_data_type) (selectors) (env) (id) serialized)

and serialize (expr : Ast.expr) (env : symbol_table) = match expr
  with Bracket_select(id, selectors) ->
    let serialized = List.fold_left (fun acc x ->
      let (expr_type,_) = check_expr_type (x) (env) in
      (match expr_type
        with String -> acc ^ "[\"" ^ (serialize_literal x) ^ "\"]"
        | Int -> acc ^ (serialize_literal x)
        | _ -> acc
      )
      ) id (List.rev selectors) in
    serialized
    (* This function is hard to write - hard to serialize an arbitrary expression. *)
  | _ -> raise (Failure "Cannot serialize a non-Bracket Select type.")

and serialize_literal (literal : expr) = match literal
    with Literal_int(i) -> string_of_int i
  | Literal_Double(i) -> string_of_Double i
  | Literal_bool(i) -> i
  | Literal_string(i) -> i
  | Id(i) -> i
  | Bracket_select(id, selectors) -> (List.fold_left (
    fun str expr -> str ^ (serialize_literal (expr)))) id selectors
  | _ -> raise (Failure "Printing this is undefined.")

let equate e1 e2 =
  if (e1 != e2) then raise (Failure "data_type mismatch")

let string_data_literal (expr : Ast.expr) = match expr
    with Literal_int(i) -> string_of_int i
  | Literal_Double(i) -> string_of_Double i
  | Literal_bool(i) -> i
  | Literal_string(i) -> i
  | _ -> raise (Failure "Printing this is undefined.")

let handle_expr_statement (expr : Ast.expr) (env: Environment.symbol_table) = match expr
  with Call(f_name, args) -> (match f_name with
    "print" ->
      if List.length args != 1 then
        raise (Failure "Print only takes one argument.")
      else
        let (_, _) = (check_expr_type (List.hd args) (env)) in
        env
    | "length" ->
      if List.length args != 1 then
        raise (Failure "Length only takes one argument.")
      else
        let (_, _) = (check_expr_type (List.hd args) (env)) in
        env
    )
  | _ -> env

let rec handle_bool_expr (bool_expr : Ast.bool_expr) (env : Environment.symbol_table) = match bool_expr
  with Literal_bool(i) -> (Bool,env)
  | Binop(e1, op, e2) ->
      let (l_type, left_env) = (check_expr_type e1 env) in
      let (r_type, right_env) = (check_expr_type e2 left_env) in
      let _ = check_bool_expr_binop_type (l_type) (op) (r_type) in
      (match (l_type, r_type)
        with (_, _) -> (Bool, right_env)
      )
  | Bool_binop(e1, conditional, e2) ->
    let (_, left_env) = handle_bool_expr (e1) (env) in
    let (_, right_env) = handle_bool_expr (e2) (left_env) in
    (Bool, right_env)
  | Not(e1) ->
    let (_,new_env) = handle_bool_expr (e1) (env) in
    (Bool, new_env)
  | Id(i) -> match var_type i env
    with Bool -> (Bool,env)
    | _ -> raise NotBoolExpr

let rec check_statement (stmt : Ast.stmt) (env : Environment.symbol_table) = match stmt
  with Expr(e1) ->
    let updated_expr = (handle_expr_statement (e1) (env)) in
    updated_expr
  | Update_variable (id, e1) ->
    let ast_dt = var_type id env in
      if ast_dt == Bool then
        raise UpdatingBool
  | If(bool_expr, then_stmt, else_stmt) ->
    let (_,new_env) = handle_bool_expr bool_expr env in
    let post_if_env = check_statements (List.rev then_stmt) (new_env) in
    let override_if = overwrite_js_map new_env post_if_env in
    let post_else_env = check_statements (List.rev else_stmt) (override_if) in
    let post_loop_env = overwrite_js_map override_if post_else_env in
    post_loop_env
    | Update_array_element (id, e1, e2) ->
      let ast_array_data_type = array_type id env in
    let data_type = ast_data_to_data ast_array_data_type in
    let (right, new_env) = check_expr_type (e2) (env) in
      equate data_type right;
      env;
  | For(init_stmt, bool_expr, update_stmt, stmt_list) ->
    let init_env = check_statement init_stmt env in
    let (_,new_env) = handle_bool_expr bool_expr init_env in
    let update_env = check_statement update_stmt new_env in
    let post_loop_env = check_statements (List.rev stmt_list) (update_env) in
      (* We need to worry about scoping here. I think we want all the things in bool expr to count. *)
    overwrite_js_map new_env post_loop_env
  | While(bool_expr, body) ->
    let (_,while_env) = handle_bool_expr bool_expr env in
    let post_loop_env = check_statements (List.rev body) (while_env) in
    (* Same thing here. We might want to be returning while_env *)
    overwrite_js_map while_env post_loop_env
  | Array_assign(expected_data_type, id, e1) ->
    let left = data_to_ast_data(string_to_data_type(expected_data_type)) in
      let inferred_type = List.map (fun expr ->
        let (data_type,_) = (check_expr_type (expr) (env)) in
        data_to_ast_data (data_type)) e1 in
        let declare_var_env = declare_var id "array" env in
          define_array_type (left) (inferred_type) (declare_var_env) (id)
  | Fixed_length_array_assign(expected_data_type, id, length) ->
    let left = data_to_ast_data(string_to_data_type(expected_data_type)) in
      let declare_var_env = declare_var id "array" env in
        define_array_type left [] declare_var_env id
  | Bool_assign(data_type, id, e1) ->
    let left = string_to_data_type(data_type) and (right,new_env) = handle_bool_expr (e1) (env) in
      equate left right;
      declare_var id data_type new_env;
  | Func_decl(func_name, arg_list, return_type, stmt_list) ->
    let func_env = declare_func func_name return_type arg_list env in
    let func_env_vars = define_func_vars arg_list func_env in
    (* TODO: Implement void functions *)
    if (return_type != "void" && (List.length stmt_list) == 0) then raise ReturnStatementMissing;
    let post_func_env = check_function_statements (List.rev stmt_list) func_env_vars return_type in
    overwrite_js_map func_env post_func_env
  | Noop -> env
  | _ -> raise (Failure "Unimplemented functionality.")

and check_statements (stmts : Ast.stmt list) (env : Environment.symbol_table) = match stmts
    with [] -> env
  | [stmt] -> check_statement stmt env
  | stmt :: other_stmts ->
      let new_env = check_statement stmt env in
      check_statements other_stmts new_env

and check_function_statements stmts env return_type = match stmts
    with [] ->
    if return_type != "void" then raise ReturnStatementMissing else
    env
  | [stmt] ->
    check_return_statement stmt env return_type
  | stmt :: other_stmts ->
      let env = check_statement stmt env in
      check_function_statements other_stmts env return_type

and check_return_statement (stmt : Ast.stmt) (env : Environment.symbol_table) (return_type : string) =
  if return_type != "void" then match stmt
    with Return(expr) ->
      let left = string_to_data_type(return_type) and (right,_) = check_expr_type (expr) (env) in
        equate left right;
        env
    | _ -> raise (Failure "Function must end with return statement.")
  else
    check_statement stmt env

(* entry point into semantic checker *)
let check_program (stmt_list : Ast.func_decl.body) =
  let env = Environment.create in
  check_statements (stmt_list) (env);











(* DICE *)
(* 

let build_class_maps reserved cdecls =
  let reserved_map = List.fold_left (fun m f -> StringMap.add (Utils.string_of_fname f.sfname) f m) StringMap.empty reserved in
  let helper m (cdecl:Ast.btmodule) =  
    let fieldfun = (fun m -> (function Field(s, d, n) -> if (StringMap.mem (n) m) then raise(Exceptions.DuplicateField) else (StringMap.add n (Field(s, d, n)) m))) in
    let funcname = get_name cdecl.cname in
    let funcfun m fdecl = 
      if (StringMap.mem (funcname fdecl) m) 
        then raise(Exceptions.DuplicateFunction(funcname fdecl)) 
      else if (StringMap.mem (Utils.string_of_fname fdecl.fname) reserved_map)
        then raise(Exceptions.CannotUseReservedFuncName(Utils.string_of_fname fdecl.fname))
      else (StringMap.add (funcname fdecl) fdecl m) 
    in
    let constructor_name = get_constructor_name cdecl.cname in
    let constructorfun m fdecl = 
      if fdecl.formals = [] then m
      else if StringMap.mem (constructor_name fdecl) m 
        then raise(Exceptions.DuplicateConstructor) 
        else (StringMap.add (constructor_name fdecl) fdecl m)
    in
    let default_c = default_c cdecl.cname in
    let constructor_map = StringMap.add (get_constructor_name cdecl.cname default_c) default_c StringMap.empty in
    (if (StringMap.mem cdecl.cname m) then raise (Exceptions.DuplicateClassName(cdecl.cname)) else
      StringMap.add cdecl.cname 
      {   field_map = List.fold_left fieldfun StringMap.empty cdecl.cbody.fields; 
        func_map = List.fold_left funcfun StringMap.empty cdecl.cbody.methods;
        constructor_map = List.fold_left constructorfun constructor_map cdecl.cbody.constructors; 
        reserved_map = reserved_map; 
        cdecl = cdecl } 
                     m) in
  List.fold_left helper StringMap.empty cdecls


(* Main method for analyzer *)
let analyze (btmodule) = match btmodule with
  Program(includes, classes) ->
  (* Generate the class_maps for look up in checking functions *)
  let class_maps = build_class_maps reserved cdecls in
  let class_maps, cdecls = handle_inheritance cdecls class_maps in
  let sast = convert_cdecls_to_sast class_maps reserved cdecls in
  sast

 *)











(* MICROC *)
  (**** Checking funclists ****)

  (* if List.mem "print" (List.map (fun fd -> fd.fname) funclists)
  then raise (Failure ("function print may not be defined")) else ();

  report_duplicate (fun n -> "duplicate function " ^ n)
    (List.map (fun fd -> fd.fname) funclists); *)

  (* Function declaration for a named function *)
  (* let built_in_decls =  StringMap.add "print"
     { returnType = Unit; fname = "print"; formals = [(Int, "x")];
       body = [] } (StringMap.singleton "printb"
     { returnType = Unit; fname = "printb"; formals = [(Bool, "x")];
       body = [] })
   in *)
     
  (* let function_decls = List.fold_left (fun m fd -> StringMap.add fd.fname fd m)
                         built_in_decls funclists
  in *)

  (* let function_decl s = try StringMap.find s function_decls
       with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in *)

(*   let _ = function_decl "main" in (* Ensure "main" is defined *)
 *)(* 
  let check_function func =

    List.iter (check_not_Unit (fun n -> "illegal Unit formal " ^ n ^
      " in " ^ func.fname)) func.formals;

    report_duplicate (fun n -> "duplicate formal " ^ n ^ " in " ^ func.fname)
      (List.map snd func.formals);

    List.iter (check_not_Unit (fun n -> "illegal Unit local " ^ n ^
      " in " ^ func.fname)) func.locals;

    report_duplicate (fun n -> "duplicate local " ^ n ^ " in " ^ func.fname)
      (List.map snd func.locals);

    (* Type of each variable (global, formal, or local *)
    let symbols = List.fold_left (fun m (t, n) -> StringMap.add n t m)
  StringMap.empty (btmodule.funcs.formals @ func.formals @ func.locals )
    in

    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    (* Return the type of an expression or throw an exception *)
    let rec expr = function
  LitInt _ -> Int
      | LitBool _ -> Bool
      | Id s -> type_of_identifier s
      | Binop(e1, op, e2) as e -> let t1 = expr e1 and t2 = expr e2 in
  (match op with
          Add | Sub | Mult | Div when t1 = Int && t2 = Int -> Int
  | Equal | Neq when t1 = t2 -> Bool
  | Less | Leq | Greater | Geq when t1 = Int && t2 = Int -> Bool
  | And | Or when t1 = Bool && t2 = Bool -> Bool
        | _ -> raise (Failure ("illegal binary operator " ^
              string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
              string_of_typ t2 ^ " in " ^ string_of_expr e))
        )
      | Uniop(op, e) as ex -> let t = expr e in
   (match op with
     Neg when t = Int -> Int
   | Not when t = Bool -> Bool
         | _ -> raise (Failure ("illegal unary operator " ^ string_of_uop op ^
           string_of_typ t ^ " in " ^ string_of_expr ex)))
      | Noexpr -> Unit
      | Assign(var, e) as ex -> let lt = type_of_identifier var
                                and rt = expr e in
        check_assign lt rt (Failure ("illegal assignment " ^ string_of_typ lt ^
             " = " ^ string_of_typ rt ^ " in " ^ 
             string_of_expr ex))
      | FuncCall(fname, actuals) as call -> let fd = function_decl fname in
         if List.length actuals != List.length fd.formals then
           raise (Failure ("expecting " ^ string_of_int
             (List.length fd.formals) ^ " arguments in " ^ string_of_expr call))
         else
           List.iter2 (fun (ft, _) e -> let et = expr e in
              ignore (check_assign ft et
                (Failure ("illegal actual argument found " ^ string_of_typ et ^
                " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e))))
             fd.formals actuals;
           fd.returnType
    in

    let check_bool_expr e = if expr e != Bool
     then raise (Failure ("expected Boolean expression in " ^ string_of_expr e))
     else () in

    (* Verify a statement or throw an exception *)
    let rec stmt = function
  Block sl -> let rec check_block = function
           [Return _ as s] -> stmt s
         | Return _ :: _ -> raise (Failure "nothing may follow a return")
         | Block sl :: ss -> check_block (sl @ ss)
         | s :: ss -> stmt s ; check_block ss
         | [] -> ()
        in check_block sl
      | Expr e -> ignore (expr e)
      | Return e -> let t = expr e in if t = func.returnType then () else
         raise (Failure ("return gives " ^ string_of_typ t ^ " expected " ^
                         string_of_typ func.returnType ^ " in " ^ string_of_expr e))
           
      | If(p, b1, b2) -> check_bool_expr p; stmt b1; stmt b2
      | While(p, s) -> check_bool_expr p; stmt s
    in

    stmt (Block func.body)
   
  in
  List.iter check_function funclists
 *)