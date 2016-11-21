open Sast
open Environment
open Pprint



(* BINARY TYPES *)
(*
let get_equality_binop_type type1 type2 se1 se2 op =
  (* Equality op not supported for float operands. The correct way to test floats
     for equality is to check the difference between the operands in question *)
  if (type1 = Datatype(Float_t) || type2 = Datatype(Float_t)) then raise (Exceptions.InvalidBinopExpression "Equality operation is not supported for Float types")
  else
    match type1, type2 with
      Datatype(Char_t), Datatype(Int_t)
    | Datatype(Int_t), Datatype(Char_t)
    | Datatype(Objecttype(_)), Datatype(Null_t)
    | Datatype(Null_t), Datatype(Objecttype(_))
    | Datatype(Null_t), Arraytype(_, _)
    | Arraytype(_, _), Datatype(Null_t) -> S.Binop(se1, op, se2, Datatype(Bool_t))
    | _ ->
      if type1 = type2 then S.Binop(se1, op, se2, Datatype(Bool_t))
      else raise (Exceptions.InvalidBinopExpression "Equality operator can't operate on different types, with the exception of Int_t and Char_t")

let get_logical_binop_type se1 se2 op = function
    (Datatype(Bool_t), Datatype(Bool_t)) -> S.Binop(se1, op, se2, Datatype(Bool_t))
  | _ -> raise (Exceptions.InvalidBinopExpression "Logical operators only operate on Bool_t types")

let get_comparison_binop_type type1 type2 se1 se2 op =
  let numerics = SS.of_list [Datatype(Int_t); Datatype(Char_t); Datatype(Float_t)]
  in
  if SS.mem type1 numerics && SS.mem type2 numerics
  then S.Binop(se1, op, se2, Datatype(Bool_t))
  else raise (Exceptions.InvalidBinopExpression "Comparison operators operate on numeric types only")

let get_arithmetic_binop_type se1 se2 op = function
    (Datatype(Int_t), Datatype(Float_t))
  | (Datatype(Float_t), Datatype(Int_t))
  | (Datatype(Float_t), Datatype(Float_t)) -> S.Binop(se1, op, se2, Datatype(Float_t))

  | (Datatype(Int_t), Datatype(Char_t))
  | (Datatype(Char_t), Datatype(Int_t))
  | (Datatype(Char_t), Datatype(Char_t)) -> S.Binop(se1, op, se2, Datatype(Char_t))

  | (Datatype(Int_t), Datatype(Int_t)) -> S.Binop(se1, op, se2, Datatype(Int_t))

  | _ -> raise (Exceptions.InvalidBinopExpression "Arithmetic operators don't support these types")
 *)

let analyze_binop env e1 op e2 =
  env, Noexpr (* env, Binop (e1,op,e2,_) *)
  (*
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
 *)

and analyze_unop env op e =
  env, Noexpr (* env, Uniop (op,e,_) *)
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
    Datatype(Int_t)
  | Datatype(Float_t) -> S.Unop(op, se, check_num_unop t op)
  | Datatype(Bool_t) -> S.Unop(op, se, check_bool_unop op)
  | _ -> raise(Exceptions.InvalidUnaryOperation)
*)

and analyze_assign env e1 e2 =
  env, Noexpr(* env, Assign (e1,e2,_) *)
(*
  let se1, env = expr_to_sexpr env e1 in
  let se2, env = expr_to_sexpr env e2 in
  let type1 = get_type_from_sexpr se1 in
  let type2 = get_type_from_sexpr se2 in
  match (type1, se2) with
    Datatype(Objecttype(_)), S.Null
  | Arraytype(_, _), S.Null -> S.Assign(se1, se2, type1) (*don't have arraytype supported yet*)
  | _ ->
    match type1, type2 with
      Datatype(Char_t), Datatype(Int_t)
    | Datatype(Int_t), Datatype(Char_t) -> S.Assign(se1, se2, type1)
    | Datatype(Objecttype(d)), Datatype(Objecttype(t)) ->
      if d = t then S.Assign(se1, se2, type1)
      else if inherited type1 type2 then
        S.Assign(se1, S.Call("cast", [se2; S.Id("ignore", type1)], type1, 0), type1)
      else raise (Exceptions.AssignmentTypeMismatch(Utils.string_of_datatype type1, Utils.string_of_datatype type2))
    | _ ->
      if type1 = type2
      then S.Assign(se1, se2, type1)
      else raise (Exceptions.AssignmentTypeMismatch(Utils.string_of_datatype type1, Utils.string_of_datatype type2))
 *)

and analyze_call env s el =
  env, Noexpr(* env, FuncCall (s,el,_) *)

(* convert Ast expr to Sast expr *)
let to_sast_expr env = function
    A.Id(s) -> env, Id(s, get_ID_type env s)
  | A.LitBool(b) -> env, LitBool(b)
  | A.LitInt(i) -> env, LitInt(i)
  | A.LitDouble(f) -> env, LitDouble(f)
  | A.LitStr(s) -> env, LitStr(s)
  | A.Binop(e1,op,e2) -> analyze_binop env e1 op e2 (* env, Binop (e1,op,e2,_) *)
  | A.Uniop(op,e) -> analyze_unop env op e (* env, Uniop (op,e,_) *)
  | A.Assign(e1,e2) -> analyze_assign env e1 e2 (* env, Assign (e1,e2,_) *)
  | A.FuncCall(s,el) -> analyze_call env s el (* env, FuncCall (s,el,_) *)
  (* | Call(s, el) -> check_call_type env false env s el, env *)
  | A.Noexpr -> env, Noexpr
  | A.Null -> env, Null


(* let check_stmt_list (stmt : Ast.func_decl.body) = *)

(* type bind = datatype * string *)

(* let check_bind bind = *)


(* SAST TYPE CONVERSIONS *)
let get_type_from_expr = function
    Id (_,d) -> d
  | LitBool(_) -> A.Datatype(Bool)
  | LitInt(_) -> A.Datatype(Int)
  | LitDouble(_) -> A.Datatype(Double)
  | LitStr(_) -> A.Datatype(String)
  | Null -> A.Datatype(Unit)
  | Binop (_,_,_,d) -> A.Datatype(d)
  | Uniop (_,_,d) -> A.Datatype(d)
  | Assign (_,_,d) -> A.Datatype(d)
  | FuncCall (_,_,d)-> A.Datatype(d) (* ??? *)
  | Noexpr -> A.Datatype(Unit)
(*
  | S.Null -> Datatype(Null_t)
   | S.Noexpr -> Datatype(Void_t)
   | S.Assign(_, _, d) -> d
   | S.Unop(_, _, d) -> d
   | S.Binop(_, _, _, d) -> d *)



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

let rec check_stmt returnType statement =
  match statement with
    Block sl -> List.iter (check_stmt returnType) sl
  (* | Expr e -> check_expr e *)
  (* | If (e, s, s) -> *)
  (* | While of expr * stmt *)
  | Return (e,_) -> if get_type_from_expr e != returnType then raise (Exceptions.ReturntypeNotMatch "foo"); ()
  (* | Break -> () *)
  (* | Continue -> () *)
  | VarDecl (d, _, e) -> if get_type_from_expr e != d then raise (Exceptions.VariableDeclarationNotMatch "foo"); ()
  | _ -> ()

let check_func btfunc =
  (* List.iter check_bind func.formals; *)
  List.iter (check_stmt btfunc.returnType) btfunc.body

let analyze program (btmodule) =
  List.iter check_func btmodule.funcs


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

 report_duplicate (fun n -> "duplicate global " ^ n) (List.map snd btmodule.funcs.formals); *)


(*

   let get_type_from_sexpr = function
   SInt_Lit(_) -> Datatype(Int_t)
   | SBoolean_Lit(_) -> Datatype(Bool_t)
   | SFloat_Lit(_) -> Datatype(Float_t)
   | SString_Lit(_) -> Arraytype(Char_t, 1)
   | SChar_Lit(_) -> Datatype(Char_t)
   | SId(_, d) -> d
   | SBinop(_, _, _, d) -> d
   | SAssign(_, _, d) -> d
   | SNoexpr -> Datatype(Void_t)
   | SArrayCreate(_, _, d) -> d
   | SArrayAccess(_, _, d) -> d
   | SObjAccess(_, _, d) -> d
   | SCall(_, _, d, _) -> d
   | SObjectCreate(_, _, d) -> d
   | SArrayPrimitive(_, d) -> d
   | SUnop(_, _, d) -> d
   | SNull -> Datatype(Null_t)
   | SDelete _ -> Datatype(Void_t)

*)
