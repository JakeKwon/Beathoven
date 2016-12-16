(*
 * Authors:
 *  - Ruonan Xu
 *)

(*
Translation Environments
*)
open Sast

(* include ?? *)
let beathoven_lib = "stdlib.bt"

module StringMap = Map.Make (String)


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
  btmodule_map : btmodule_env StringMap.t;
  name : string;
  btmodule : btmodule_env ref; (* current module *)
  formal_map : A.bind StringMap.t;
  mutable var_map : A.datatype StringMap.t;
  mutable env_returnType: A.datatype; (* why mutable ?? *)
  mutable env_in_for : bool;
  mutable env_in_while : bool;
}

(* Initialize builtin_types *)
let (builtin_types_list : A.struct_decl list) =
  [{
    A.sname = "pitch";
    A.fields = [(A.Primitive(String), "key"); (A.Primitive(Int), "octave");
                (A.Primitive(Int), "alter");];
  };
  {
    A.sname = "duration";
    A.fields = [(A.Primitive(Int), "a");(A.Primitive(Int), "b");];
  };]

let (builtin_types : A.struct_decl StringMap.t) =
  let add_to_map (builtin_type : A.struct_decl) map =
    StringMap.add builtin_type.sname builtin_type map
  in
  List.fold_right add_to_map builtin_types_list StringMap.empty

(* Initialize builtin_funcs *)
let (builtin_funcs : func_decl StringMap.t) =
  let get_func_decl name (returnType : A.datatype) formalsType =
    {
      fname = name; body = [];
      returnType = returnType;
      formals = List.map (fun typ -> (typ, "")) formalsType;
    }
  in
  let unit_t = A.Primitive(Unit) in
  let map = StringMap.empty in
  let map = StringMap.add "print"
    (get_func_decl "printf" unit_t []) map in
  let map = StringMap.add "print_pitch"
      (get_func_decl "_print_pitch" (Primitive(String)) [ A.Musictype(Pitch) ]) map in
  map
(*
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

(*
type environment = {
  scope: symbol_table; (* vars symbol table *)
  functions: (int * Ast.fundef) list; (* (num args * fundef) list *)
  extern_functions: Ast.externfun list;
  types: Sast.tdefault list;
}
 *)



(*
   module FunctionMap = Map.Make(String);;
   module VariableMap = Map.Make(String);;
   module ArrayTypeMap = Map.Make(String);;

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

   let string_to_primi (s : string) = match s
   with "int" -> Int
   | "bool" -> Bool
   | "string" -> String
   | "double" -> Double
   | _ -> raise (Failure "String does not match a particular data type. Something went wrong.")

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
