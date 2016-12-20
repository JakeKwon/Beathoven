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

(* Initialize builtin_types. *)
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
     (* TODO: if Note is just struct, why not declare it in stdlib.bt *)
     A.sname = "Note";
     A.fields = [(A.Primitive(Pitch), "p");(A.Primitive(Duration), "d");];
   };]

let (builtin_types : A.struct_decl StringMap.t) =
  let add_to_map (builtin_type : A.struct_decl) map =
    StringMap.add builtin_type.sname builtin_type map
  in
  List.fold_right add_to_map builtin_types_list StringMap.empty

(* Initialize builtin_funcs *)
(* This is part is only for function checking, such as parameters type checking *)
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
  let unit_t = A.Primitive(Unit) and int_t = A.Primitive(Int)
  and string_t = A.Primitive(String)
  and pitch_t = A.Primitive(Pitch) and duration_t = A.Primitive(Duration)
  in
  let map = StringMap.empty in
  let map = StringMap.add "print"
      (get_func_decl "printf" unit_t []) map in
  let map = StringMap.add "len"
      (get_func_decl "len" int_t [ ]) map in (* TODO: add the param *)
  let map = StringMap.add "render_as_midi"
      (get_func_decl "render_as_midi" unit_t [ A.Arraytype(A.seq_ele_type) ]) map in (* TODO: add the param *)
  let map = StringMap.add "str_of_pitch"
      (get_func_decl "_str_of_pitch" string_t [ pitch_t ]) map in
  let map = StringMap.add "str_of_duration"
      (get_func_decl "_str_of_duration" string_t [ duration_t ]) map in
  let map = StringMap.add "str_of_Note"
      (get_func_decl "_str_of_Note" string_t [ A.Structtype("Note") ]) map in
  map
