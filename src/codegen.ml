module L = Llvm (* LLVM VMCore interface library *)
open Ast
(* open Log *)

module StringMap = Map.Make(String)

exception Error of string

let context = L.global_context () (* global data container *)
let the_module = L.create_module context "Beathoven Codegen" (* container *)
(* let builder = L.builder context *)
let double_t = L.double_type context
and i64_t = L.i64_type context
and i32_t = L.i32_type context
and i8_t = L.i8_type context
and i1_t = L.i1_type context
and unit_t = L.void_type context
let str_t = L.pointer_type i8_t
let global_vars:(string, L.llvalue) Hashtbl.t = Hashtbl.create 50
(* cannot use map since StringMap.add returns a new map.  *)

let lltype_of_typ = function
    Unit -> unit_t
  | Int -> i32_t
  | Double -> double_t
  | String -> str_t
  | Bool -> i1_t

(* Declare global variable; remember its llvalue in a map *)
let allocate typ var_name builder =
  let alloca = L.build_alloca (lltype_of_typ typ) var_name builder in
  Hashtbl.add global_vars var_name alloca

(* Return the value for a variable or formal argument *)
let lookup s = try Hashtbl.find global_vars s
  with Not_found -> raise (Exceptions.UnknownVariable s)



(* Construct code for an expression; return its llvalue *)
let rec codegen_expr builder = function
    Id s -> L.build_load (lookup s) s builder
  | LitBool b -> L.const_int i1_t (if b then 1 else 0)
  | LitInt i -> L.const_int i32_t i
  | LitDouble d -> L.const_float double_t d
  | LitStr s -> L.build_global_stringptr s "tmp" builder
  | Noexpr -> L.const_int i32_t 0
  | Null -> L.const_null i32_t
  | Assign (e1, e2) ->
    let codegen_assign lhs rhs builder =
      let lhs = match lhs with Id s -> lookup s in
      let rhs = codegen_expr builder rhs in
      ignore(L.build_store rhs lhs builder);
      rhs
    in codegen_assign e1 e2 builder

let rec codegen_stmt builder = function (*rec??*)
    Block sl -> List.fold_left codegen_stmt builder sl
  | Expr e -> ignore (codegen_expr builder e); builder
and codegen_stmts builder sl = List.fold_left codegen_stmt builder sl


let codegen_builtin_functions () =
  (* Declare printf(), which the print built-in function will call *)
  let printf_t = L.var_arg_function_type i32_t [| str_t |] in
  let _ = L.declare_function "printf" printf_t the_module in
  ()


let codegen_main (globals, statements) =

  let beathoven_t = L.function_type i32_t [| i32_t; L.pointer_type str_t |] in
  let beathoven = L.define_function "~beathoven" beathoven_t the_module in
  (* An instance of the IRBuilder class used in generating LLVM instructions *)
  let llbuilder = L.builder_at_end context (L.entry_block beathoven) in

  (* Declare each global variable; remember its value in map global_vars *)
  let _ =
    let add_global_var (datatype, var_name) =
      ignore (allocate datatype var_name llbuilder) in
    let tmp k v = print_string k in
    List.iter add_global_var globals;
  in

  let _ = codegen_stmts llbuilder statements in

  (* Finish off the function. *)
  L.build_ret (L.const_int i32_t 0) llbuilder;
  the_module

(*
| Binop of expr * binary_operator * expr
| Uniop of unary_operator * expr

| FuncCall of string * expr list *)

(* and codegen_expr llbuilder = function
   	|   SBinop(e1, op, e2, d)     	-> handle_binop e1 op e2 d llbuilder
   	|   SCall(fname, el, d, _)       	-> codegen_call llbuilder d el fname
   	|   SUnop(op, e, d)           	-> handle_unop op e d llbuilder
*)

(* Batteries  *)

(* Invoke "f builder" if the current block doesn't already
   have a terminal (e.g., a branch). *)
(* let add_terminal builder f =
  match L.block_terminator (L.insertion_block builder) with
    Some _ -> ()
  | None -> ignore (f builder) in *)


(*
Code generation: translate takes a semantically checked AST and produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial
  http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:
  http://llvm.moe/
  http://llvm.moe/ocaml/
*)
