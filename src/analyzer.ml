(* method copied from Dice by David Watkins for alteration *)

module S = Sast
module A = Ast

module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

(* creation of environment *)
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

(* get ID of environment *)
let rec get_ID_type env s = 
	try StringMap.find s env.env_locals
	with | Not_found -> 
	try let formal = StringMap.find s env.env_parameters in
		(function Formal(t, _) -> t | Many t -> t ) formal
	with | Not_found -> raise (Exceptions.UndefinedID s)

(* BINARY TYPES *)
(* binary equality *)
let get_equality_binop_type type1 type2 se1 se2 op = 
	(* Equality op not supported for float operands. The correct way to test floats 
	   for equality is to check the difference between the operands in question *)
	if (type1 = Datatype(Float_t) || type2 = Datatype(Float_t)) then raise (Exceptions.InvalidBinopExpression "Equality operation is not supported for Float types")
	else 
	match type1, type2 with
		Datatype(Char_t), Datatype(Int_t) 
	| 	Datatype(Int_t), Datatype(Char_t)
	| 	Datatype(Objecttype(_)), Datatype(Null_t)
	| 	Datatype(Null_t), Datatype(Objecttype(_))
	| 	Datatype(Null_t), Arraytype(_, _)
	| 	Arraytype(_, _), Datatype(Null_t) -> SBinop(se1, op, se2, Datatype(Bool_t))
	| _ ->
		if type1 = type2 then SBinop(se1, op, se2, Datatype(Bool_t))
		else raise (Exceptions.InvalidBinopExpression "Equality operator can't operate on different types, with the exception of Int_t and Char_t")

(* binary logic *)
let get_logical_binop_type se1 se2 op = function 
	(Datatype(Bool_t), Datatype(Bool_t)) -> SBinop(se1, op, se2, Datatype(Bool_t))
	| _ -> raise (Exceptions.InvalidBinopExpression "Logical operators only operate on Bool_t types")

(* binary comparison *)
let get_comparison_binop_type type1 type2 se1 se2 op =  
	let numerics = SS.of_list [Datatype(Int_t); Datatype(Char_t); Datatype(Float_t)]
	in
		if SS.mem type1 numerics && SS.mem type2 numerics
			then SBinop(se1, op, se2, Datatype(Bool_t))
		else raise (Exceptions.InvalidBinopExpression "Comparison operators operate on numeric types only")

(* binary arithmetics *)
let get_arithmetic_binop_type se1 se2 op = function 
			(Datatype(Int_t), Datatype(Float_t)) 
		| 	(Datatype(Float_t), Datatype(Int_t)) 
		| 	(Datatype(Float_t), Datatype(Float_t)) 	-> SBinop(se1, op, se2, Datatype(Float_t))

		| 	(Datatype(Int_t), Datatype(Char_t)) 
		| 	(Datatype(Char_t), Datatype(Int_t)) 
		| 	(Datatype(Char_t), Datatype(Char_t)) 	-> SBinop(se1, op, se2, Datatype(Char_t))

		| 	(Datatype(Int_t), Datatype(Int_t)) 		-> SBinop(se1, op, se2, Datatype(Int_t))

		| _ -> raise (Exceptions.InvalidBinopExpression "Arithmetic operators don't support these types")

(* assign *)
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

(* UNARY OPERATORS *)
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


(* Get type from Sast *)
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

(* convert Ast expr to Sast expr *)
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
