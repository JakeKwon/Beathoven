(*
 * Authors:
 *  - Ruonan Xu
 *  - Jake Kwon
 *  - Sona Roy
 *  - Eunice Kokor
 *)

(*
Translation Environments
*)
open Sast
module StringMap = Map.Make (String)


let get_global_func_name mname fname =
  if mname = A.default_mname && fname = A.default_fname
  then "main" (* main entry *)
  (* We use '.' to separate types so llvm will recognize the function name
     and it won't conflict *)
  else mname ^ "." ^ fname

let get_global_name mname n =
  (* TODO: maybe need another module name for user main, instead of
     default_mname. Since user ids are not visible to all. Work on this during
     stdlib.bt *)
  (* if mname = A.default_mname then n else  *)
  mname ^ "." ^ n


type btmodule_env = {
  (* an immutable field, as all funcs are known in Ast *)
  func_map : A.func_decl StringMap.t; (* key: global name *)
  mutable struct_map : A.struct_decl StringMap.t; (* key: global name *)
  (* what's the use except findding duplicate?? *)
  mutable field_map : A.datatype StringMap.t; (* key: global name *)
}

(* initialize a new environment for every func *)
type env = {
  (* same for all envs *)
  btmodule_map : btmodule_env StringMap.t;
  (* the module this func is in *)
  name : string;
  btmodule : btmodule_env ref;
  ismain : bool; (* whether this func is main of module *)
  (* func locals *)
  formal_map : A.bind StringMap.t;
  mutable var_map : A.datatype StringMap.t;

  mutable env_returnType: A.datatype; (* why mutable ?? *)
  mutable env_in_for : bool;
  mutable env_in_while : bool;
}

(* Initialize builtin_types *)
let (builtin_types_list : A.struct_decl list) =
  [{
    A.sname = "_pitch";
    A.fields = [(A.Primitive(Char), "key"); (A.Primitive(Int), "octave");
                (A.Primitive(Int), "alter");];
  };
   {
     A.sname = "_duration";
     A.fields = [(A.Primitive(Int), "a");(A.Primitive(Int), "b");];
   };
   {
     A.sname = "Note";
     A.fields = [(A.Primitive(Pitch), "p");(A.Primitive(Duration), "d");];
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
      (* Note that formal types here correspond to codegen/get_bind_type,
      codegen_builtin_funcs, and function parameters in stdlib.c *)
      formals = List.map (fun typ -> (typ, "")) formalsType;
    }
  in
  let unit_t = A.Primitive(Unit) and string_t = A.Primitive(String)
  and pitch_t = A.Primitive(Pitch) and duration_t = A.Primitive(Duration)
  in
  let map = StringMap.empty in
  let map = StringMap.add "print"
      (get_func_decl "printf" unit_t []) map in
  let map = StringMap.add "render_as_midi"
      (get_func_decl "render_as_midi" unit_t [ A.Arraytype(A.Musictype(Note)) ]) map in (* TODO: add the param *)
  let map = StringMap.add "str_of_pitch"
      (get_func_decl "_str_of_pitch" string_t [ pitch_t ]) map in
  let map = StringMap.add "str_of_duration"
      (get_func_decl "_str_of_duration" string_t [ duration_t ]) map in
  let map = StringMap.add "str_of_Note"
      (get_func_decl "_str_of_Note" string_t [ A.Musictype(Note) ]) map in
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

   let var_type (id : string) (env : symbol_table) =
   if VariableMap.mem id env.var_map then
    VariableMap.find id env.var_map
   else
    raise VarNotDeclared

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
