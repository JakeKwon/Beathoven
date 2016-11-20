open Sast

module A = Ast
module StringMap = Map.Make (String)
module StringSet = Set.Make (String)


(* type func_decl = {
  fname : string;
  formals : bind list;
  returnType : datatype;
  body : stmt list;
  (* TODO?: separate vars from stmt list *)
} *)

(* let check_stmt_list (stmt : Ast.func_decl.body) = *)



(* type bind = datatype * string *)

(* let check_bind bind = *)

let get_type_from_expr = function
	Id (_,d)		-> A.Datatype(d)
  | LitBool(_)		-> A.Datatype(Bool)
  | LitInt(_)		-> A.Datatype(Int)
  | LitDouble(_)	-> A.Datatype(Double)
  | LitStr(_)		-> A.Datatype(String)
  | Null			-> A.Datatype(Unit)
  | Binop (_,_,_,d)	-> d
  | Uniop (_,_,d)	-> d
  | Assign (_,_,d)	-> d
  | FuncCall (_,_,d)-> d 	(* ??? *)
  | Noexpr			-> A.Datatype(Unit);

(* type expr =
    Id of string
  | LitBool of bool
  | LitInt of int
  | LitDouble of float
  | LitStr of string
  | Null
  | Binop of expr * binary_operator * expr
  | Uniop of unary_operator * expr
  | Assign of expr * expr
  | FuncCall of string * expr list
  | Noexpr *)

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


(* type stmt =
    Block of stmt list
  | Expr of expr
  | If of expr * stmt * stmt
  | While of expr * stmt
  | Return of expr
  | Break
  | Continue
  | VarDecl of datatype * string * expr
 *)

let rec check_stmt returnType statement = 
	match statement with 
    Block sl 			-> List.iter (check_stmt returnType) sl
  (* | Expr e 				-> check_expr e *)
  (* | If (e, s, s) ->  *)
  (* | While of expr * stmt *)
  | Return e 			-> if get_type_from_expr e != returnType then raise (Exceptions.ReturntypeNotMatch e)); ()
  (* | Break		-> () *)
  (* | Continue	-> () *)
  | VarDecl (d, _, e) 	-> if get_type_from_expr e != d then raise (Exceptions.VariableDeclarationNotMatch e)); ()
  | _ -> ()
 
let check_func btfunc =
	(* fname doesnt need to be checked *)
	(* List.iter check_bind func.formals; *)
	List.iter (check_stmt func.returnType) btfunc.body

let analyze program (btmodule) =
	(* mname doesnt need to be checked  *)
	List.iter check_func btmodule.funcs;



















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


(* type class_map = {
	(* field_map       : Ast.field StringMap.t; *)
	func_map        : Ast.func_decl StringMap.t;
	constructor_map : Ast.func_decl StringMap.t;
	cdecl 			: Ast.class_decl;
}

type env = {
	env_class_maps: class_map StringMap.t;
	env_name      : string;
	env_cmap 	  : class_map;
	env_locals    : datatype StringMap.t; 
	env_parameters: Ast.formal StringMap.t;
	env_returnType: datatype;
	env_in_for    : bool;
	env_in_while  : bool;
	env_reserved  : sfunc_decl list;
}

(* ??? whats going on here? with env *)
let rec get_ID_type env s = 
	try StringMap.find s env.env_locals
	with | Not_found -> 
	try let formal = StringMap.find s env.env_parameters in
		(function Formal(t, _) -> t | Many t -> t ) formal
	with | Not_found -> raise (Exceptions.UndefinedID s)

and check_binop env e1 op e2 =
	let se1, env = expr_to_sexpr env e1 in
	let se2, env = expr_to_sexpr env e2 in
	let type1 = get_type_from_sexpr se1 in
	let type2 = get_type_from_sexpr se2 in
	match op with
	Equal | Neq -> get_equality_binop_type type1 type2 se1 se2 op
	| And | Or -> get_logical_binop_type se1 se2 op (type1, type2)
	| Less | Leq | Greater | Geq -> get_comparison_binop_type type1 type2 se1 se2 op
	| Add | Mult | Sub | Div | Mod -> get_arithmetic_binop_type se1 se2 op (type1, type2) 
	| _ -> raise (Exceptions.InvalidBinopExpression ((Utils.string_of_op op) ^ " is not a supported binary op"))



and expr_to_sexpr env = function
		Int_Lit i           -> SInt_Lit(i), env
	|   Boolean_Lit b       -> SBoolean_Lit(b), env
	|   Float_Lit f         -> SFloat_Lit(f), env
	|   String_Lit s        -> SString_Lit(s), env
	|   Char_Lit c          -> SChar_Lit(c), env
	|   This                -> SId("this", Datatype(Objecttype(env.env_name))), env
	|   Id s                -> SId(s, get_ID_type env s), env
	|   Null                -> SNull, env
	|   Noexpr              -> SNoexpr, env

	|   ObjAccess(e1, e2)   -> check_obj_access env e1 e2, env
	|   ObjectCreate(s, el) -> check_object_constructor env s el, env
	|   Call(s, el)         -> check_call_type env false env s el, env

	|   ArrayCreate(d, el)  -> check_array_init env d el, env
	|   ArrayAccess(e, el)  -> check_array_access env e el, env
	|   ArrayPrimitive el   -> check_array_primitive env el, env

	|   Assign(e1, e2)      -> check_assign env e1 e2, env
	|   Unop(op, e)         -> check_unop env op e, env
	|   Binop(e1, op, e2)   -> check_binop env e1 op e2, env
	| 	Delete(e) 			-> check_delete env e, env


and get_type_from_sexpr = function
		SInt_Lit(_)				-> Datatype(Int_t)
	| 	SBoolean_Lit(_)			-> Datatype(Bool_t)
	| 	SFloat_Lit(_)			-> Datatype(Float_t)
	| 	SString_Lit(_) 			-> Arraytype(Char_t, 1)
	| 	SChar_Lit(_) 			-> Datatype(Char_t)
	| 	SId(_, d) 				-> d
	| 	SBinop(_, _, _, d) 		-> d
	| 	SAssign(_, _, d) 		-> d
	| 	SNoexpr 				-> Datatype(Void_t)
	| 	SArrayCreate(_, _, d)	-> d
	| 	SArrayAccess(_, _, d) 	-> d
	| 	SObjAccess(_, _, d)		-> d
	| 	SCall(_, _, d, _)		-> d
	|   SObjectCreate(_, _, d) 	-> d
	| 	SArrayPrimitive(_, d)	-> d
	|  	SUnop(_, _, d) 			-> d
	| 	SNull					-> Datatype(Null_t)
	| 	SDelete _ 				-> Datatype(Void_t)

and check_assign env e1 e2 = 
	let se1, env = expr_to_sexpr env e1 in
	let se2, env = expr_to_sexpr env e2 in
	let type1 = get_type_from_sexpr se1 in
	let type2 = get_type_from_sexpr se2 in 
	match (type1, se2) with
		Datatype(Objecttype(_)), SNull 
	| 	Arraytype(_, _), SNull -> SAssign(se1, se2, type1)
	|   _ -> 
	match type1, type2 with
		Datatype(Char_t), Datatype(Int_t)
	| 	Datatype(Int_t), Datatype(Char_t) -> SAssign(se1, se2, type1)
	| 	Datatype(Objecttype(d)), Datatype(Objecttype(t)) ->
		if d = t then SAssign(se1, se2, type1)
		else if inherited type1 type2 then
			SAssign(se1, SCall("cast", [se2; SId("ignore", type1)], type1, 0), type1)  
		else raise (Exceptions.AssignmentTypeMismatch(Utils.string_of_datatype type1, Utils.string_of_datatype type2))
	| _ -> 
	if type1 = type2 
		then SAssign(se1, se2, type1)
		else raise (Exceptions.AssignmentTypeMismatch(Utils.string_of_datatype type1, Utils.string_of_datatype type2))

and check_unop env op e = 
	let check_num_unop t = function
			Sub 	-> t
		| 	_ 		-> raise(Exceptions.InvalidUnaryOperation)
	in 
	let check_bool_unop = function
			Not 	-> Datatype(Bool_t)
		| 	_ 		-> raise(Exceptions.InvalidUnaryOperation)
	in
	let se, env = expr_to_sexpr env e in
	let t = get_type_from_sexpr se in
	match t with 
		Datatype(Int_t) 	
	|	Datatype(Float_t) 	-> SUnop(op, se, check_num_unop t op)
	|  	Datatype(Bool_t) 	-> SUnop(op, se, check_bool_unop op)
	| 	_ -> raise(Exceptions.InvalidUnaryOperation)




 *)