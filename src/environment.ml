(* creation of environment *)
open Sast

(* include ?? *)
let beathoven_lib = "stdlib.bt"

module StringMap = Map.Make (String)
module StringSet = Set.Make (String)


type btmodule_env = {
  func_map : A.func_decl StringMap.t; (* key: global name *)
  (* an immutable field, as funcs are known before analyzer *)
  mutable struct_map : A.struct_decl StringMap.t; (* key: global name *)
  (* what's the use except findding duplicate?? *)
  (* decl : A.btmodule; *)
  (* field_map : A.datatype StringMap.t; *)
}

type env = {
  builtin_funcs : func_decl StringMap.t;
  (* builtin_types :  *)
  btmodule_map : btmodule_env StringMap.t;
  name : string;
  btmodule : btmodule_env ref; (* current module *)
  formal_map : A.bind StringMap.t;
  mutable var_map : A.datatype StringMap.t;
  mutable env_returnType: A.datatype; (* why mutable ?? *)
  mutable env_in_for : bool;
  mutable env_in_while : bool;
}

(*
type environment = {
  scope: symbol_table; (* vars symbol table *)
  functions: (int * Ast.fundef) list; (* (num args * fundef) list *)
  extern_functions: Ast.externfun list;
  types: Sast.tdefault list;
}
 *)

(* Environment Utilities *)

let get_ID_type env s =
  try StringMap.find s env.var_map
  with | Not_found ->
  try
    let (d, _) = StringMap.find s env.formal_map in d
  with | Not_found -> raise (Exceptions.UndefinedID s)

(* (*
 * QL
 * Manager: Matthew Piccolella
 *)

   module FunctionMap = Map.Make(String);;
   module VariableMap = Map.Make(String);;
   module ArrayTypeMap = Map.Make(String);;

   exception VarAlreadyDeclared;;
   exception VarNotDeclared;;
   exception FunctionAlreadyDeclared;;
   exception FunctionNotDeclared;;
   exception IncorrectFunctionParameterTypes;;
   exception MixedTypeArray;;
   exception ArrayInferTypeMismatch;;
   exception IncorrectArrayAssignmentSize;;

   type func_info = {
   id : string;
   return : primi;
   args : primi list;
   arg_names: string list;
   }

   type symbol_table = {
   func_map: func_info FunctionMap.t;
   var_map: primi VariableMap.t;
   array_type_map: primi ArrayTypeMap.t;
   }

   let create =
   {
    func_map = FunctionMap.empty;
    var_map = VariableMap.empty;
    array_type_map = ArrayTypeMap.empty;
   }

   let update f_map v_map a_type_map js_map =
   {
    func_map = f_map;
    var_map = v_map;
    array_type_map = a_type_map;
   }

   let string_to_primi (s : string) = match s
   with "int" -> Int
   | "bool" -> Bool
   | "string" -> String
   | "double" -> Double
   | _ -> raise (Failure "String does not match a particular data type. Something went wrong.")

   let rec primi_to_string (dt : primi) = match dt
   with Int -> "int"
   | Bool -> "bool"
   | String -> "string"
   | Double -> "double"
   | _ -> raise (Failure "Data Type doesn't have a corresponding string.")

   let declare_var (id : string) (primi : string) (env : symbol_table) =
   if VariableMap.mem id env.var_map then
    raise VarAlreadyDeclared
   else
    let update_var_map = VariableMap.add id (string_to_primi(primi)) env.var_map in
    update env.func_map update_var_map env.array_type_map

   let var_type (id : string) (env : symbol_table) =
   if VariableMap.mem id env.var_map then
    VariableMap.find id env.var_map
   else
    raise VarNotDeclared

   let create_func (func_name: string) (ret_type : string) (args : arg_decl list) =
   {
    id = func_name;
    return = (string_to_primi ret_type);
    args = List.map (fun arg -> string_to_primi arg.var_type) args;
    arg_names = List.map (fun arg -> arg.var_name) args;
   }

   let define_array_type (expected_type: primi)
   (inferred_type : primi list) (env : symbol_table) (id : string) =
   if List.length inferred_type != 0 then
    let first_type = List.hd inferred_type in
    (* Verify all types are the same *)
    List.iter (fun (primi) -> if first_type != primi then raise MixedTypeArray) inferred_type;
    (if first_type == expected_type then
      let update_array_type_map = ArrayTypeMap.add id first_type env.array_type_map in
      update env.func_map env.var_map update_array_type_map
    else
      raise ArrayInferTypeMismatch)
   else
    (* Empty array created *)
    let update_array_type_map = ArrayTypeMap.add id expected_type env.array_type_map in
    update env.func_map env.var_map update_array_type_map

   let array_type (id : string) (env : symbol_table) =
   if ArrayTypeMap.mem id env.array_type_map then
    ArrayTypeMap.find id env.array_type_map
   else
    raise VarNotDeclared

   let rec define_func_vars (func_vars : arg_decl list) (env : symbol_table) = match func_vars
   with [] -> env
   | head::body ->
    let new_env = declare_var head.var_name head.var_type env in
    define_func_vars body new_env

   let declare_func (func_name : string) (ret_type : string) (args : arg_decl list) (env : symbol_table) =
   if FunctionMap.mem func_name env.func_map then
    raise FunctionAlreadyDeclared
   else
    let update_func_map = FunctionMap.add func_name (create_func func_name ret_type args) env.func_map in
    update update_func_map env.var_map env.array_type_map

   let verify_func_call (func_name: string) (args : primi list) (env : symbol_table) =
   match func_name with
    "print" | "length" -> ()
    | _ ->
    if FunctionMap.mem func_name env.func_map then
      let declared_func = FunctionMap.find func_name env.func_map in
      let type_pairs = List.combine args declared_func.args in
      List.iter (fun (left, right) ->
        if left != right then
          raise IncorrectFunctionParameterTypes
      ) type_pairs
    else
      raise FunctionNotDeclared

   let func_return_type (func_name : string) (env : symbol_table) =
   match func_name with
    "print" -> String
    | "length" -> Int
    | _ ->
      if FunctionMap.mem func_name env.func_map then
        let declared_func = FunctionMap.find func_name env.func_map in
        declared_func.return
      else
        raise FunctionNotDeclared
*)
