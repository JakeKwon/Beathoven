module L = Llvm (* LLVM VMCore interface library *)
open Ast

module StringMap = Map.Make(String)

exception Error of string

let context = L.global_context () (* global data container *)
let the_module = L.create_module context "Beathoven Codegeen" (* container *)
(* An instance of the IRBuilder class used in generating LLVM instructions *)
let builder = L.builder context
let double_t = L.double_type context
  and i64_t = L.i64_type context
  and i32_t = L.i32_type context
  and i8_t = L.i8_type context
  and i1_t = L.i1_type context
  and unit_t = L.void_type context
  and str_t = L.pointer_type i8_t
(* let named_values:(string, L.llvalue) Hashtbl.t = Hashtbl.create 10 *)

let translate (globals, statements) =
  let make_global_var = 

(* Declare each global variable; remember its value in a map *)
(* let global_vars =
  let global_var m (t, n) =
    let init = L.const_int (ltype_of_typ t) 0
    in StringMap.add n (L.define_global n init the_module) m in
  List.fold_left global_var StringMap.empty globals in *)



(* Return the value for a variable or formal argument *)
let lookup s = try StringMap.find s global_vars
               with Not_found -> raise (Exceptions.UnknownVariable id)
in

(* Construct code for an expression; return its value *)
let rec codegen_expr builder = function
    Id s -> L.build_load (lookup s) s builder
    (* try
    with | Not_found ->  *)
  | LitBool b -> L.const_int i1_t (if b then 1 else 0)
  | LitInt i -> L.const_int i32_t i
  | LitDouble d -> L.const_float double_t d
  | Noexpr -> L.const_int i32_t 0


(* | LitStr of string
| Null
| Binop of expr * binary_operator * expr
| Uniop of unary_operator * expr
| Assign of expr * expr
| FuncCall of string * expr list *)

(* and codegen_expr llbuilder = function
	|   SString_Lit(s)         		-> codegen_string_lit s llbuilder
	|   SChar_Lit(c)           		-> const_int i8_t (Char.code c)
	|   SId(id, d)                	-> codegen_id true false id d llbuilder
	|   SBinop(e1, op, e2, d)     	-> handle_binop e1 op e2 d llbuilder
	|   SAssign(e1, e2, d)        	-> codegen_assign e1 e2 d llbuilder
	|   SNoexpr                 	-> build_add (const_int i32_t 0) (const_int i32_t 0) "nop" llbuilder
	|   SArrayCreate(t, el, d)    	-> codegen_array_create llbuilder t d el
	|   SArrayAccess(e, el, d)    	-> codegen_array_access false e el d llbuilder
	|   SObjAccess(e1, e2, d)     	-> codegen_obj_access true e1 e2 d llbuilder
	|   SCall(fname, el, d, _)       	-> codegen_call llbuilder d el fname
	|   SObjectCreate(id, el, d)  	-> codegen_obj_create id el d llbuilder
	|   SArrayPrimitive(el, d)    	-> codegen_array_prim d el llbuilder
	|   SUnop(op, e, d)           	-> handle_unop op e d llbuilder
	|   SNull          	        	-> const_null i32_t
	| 	SDelete e 				 	-> codegen_delete e llbuilder

  let rec codegen_expr = function
    | Ast.Variable name ->
        (try Hashtbl.find named_values name with
          | Not_found -> raise (Error "unknown variable name"))
    | Ast.Binary (op, lhs, rhs) ->
        let lhs_val = codegen_expr lhs in
        let rhs_val = codegen_expr rhs in
        begin
          match op with
          | '+' -> build_add lhs_val rhs_val "addtmp" builder
          | '-' -> build_sub lhs_val rhs_val "subtmp" builder
          | '*' -> build_mul lhs_val rhs_val "multmp" builder
          | '<' ->
              (* Convert bool 0/1 to double 0.0 or 1.0 *)
              let i = build_fcmp Fcmp.Ult lhs_val rhs_val "cmptmp" builder in
              build_uitofp i double_type "booltmp" builder
          | _ -> raise (Error "invalid binary operator")
        end
    | Ast.Call (callee, args) ->
        (* Look up the name in the module table. *)
        let callee =
          match lookup_function callee the_module with
          | Some callee -> callee
          | None -> raise (Error "unknown function referenced")
        in
        let params = params callee in

        (* If argument mismatch error. *)
        if Array.length params == Array.length args then () else
          raise (Error "incorrect # arguments passed");
        let args = Array.map codegen_expr args in
        build_call callee args "calltmp" builder

  let codegen_proto = function
    | Ast.Prototype (name, args) ->
        (* Make the function type: double(double,double) etc. *)
        let doubles = Array.make (Array.length args) double_type in
        let ft = function_type double_type doubles in
        let f =
          match lookup_function name the_module with
          | None -> declare_function name ft the_module

          (* If 'f' conflicted, there was already something named 'name'. If it
           * has a body, don't allow redefinition or reextern. *)
          | Some f ->
              (* If 'f' already has a body, reject this. *)
              if block_begin f <> At_end f then
                raise (Error "redefinition of function");

              (* If 'f' took a different number of arguments, reject. *)
              if element_type (type_of f) <> ft then
                raise (Error "redefinition of function with different # args");
              f
        in

        (* Set names for all arguments. *)
        Array.iteri (fun i a ->
          let n = args.(i) in
          set_value_name n a;
          Hashtbl.add named_values n a;
        ) (params f);
        f

  let codegen_func = function
    | Ast.Function (proto, body) ->
        Hashtbl.clear named_values;
        let the_function = codegen_proto proto in

        (* Create a new basic block to start insertion into. *)
        let bb = append_block context "entry" the_function in
        position_at_end bb builder;

        try
          let ret_val = codegen_expr body in

          (* Finish off the function. *)
          let _ = build_ret ret_val builder in

          (* Validate the generated code, checking for consistency. *)
          Llvm_analysis.assert_valid_function the_function;

          the_function
        with e ->
          delete_function the_function;
          raise e *)

(* Hashtbl, Batteries  *)

(* let rec codegen_expr = function
      Int_Lit i
  |   Boolean_Lit b
  |   Float_Lit f
  |   String_Lit s
  |   Char_Lit c
  |   This
  |   Id s
  |   Binop (e1, op, e2)
  |   Assign (e1, e2)
  |   Noexpr
  |   ArrayOp (e1, el)
  |   ObjAccess (e1, e2)
  |   Call (fname, el) ->
        (* Look up the name in the module table. *)
        let callee =
          match lookup_function callee the_module with
            | Some callee -> callee
            | None -> raise (Error "unknown function referenced")
        in
        let params = params callee in
        (* If argument mismatch error. *)
        if Array.length params == Array.length args then () else
          raise (Error "incorrect # arguments passed");
        let args = Array.map codegen_expr args in
        build_call callee args "calltmp" builder
  |   ArrayPrimitive el
  |   Null
 *)

(* let rec codegen_expr = function
  | Ast.Number n -> const_float double_type n
  | Ast.Variable name ->
      (try Hashtbl.find named_values name with
        | Not_found -> raise (Error "unknown variable name"))
  | Ast.Binary (op, lhs, rhs) ->
      let lhs_val = codegen_expr lhs in
      let rhs_val = codegen_expr rhs in
      begin
        match op with
        | '+' -> build_add lhs_val rhs_val "addtmp" builder
        | '-' -> build_sub lhs_val rhs_val "subtmp" builder
        | '*' -> build_mul lhs_val rhs_val "multmp" builder
        | '<' ->
            (* Convert bool 0/1 to double 0.0 or 1.0 *)
            let i = build_fcmp Fcmp.Ult lhs_val rhs_val "cmptmp" builder in
            build_uitofp i double_type "booltmp" builder
        | _ -> raise (Error "invalid binary operator")
      end
  | Ast.Call (callee, args) ->
      (* Look up the name in the module table. *)
      let callee =
        match lookup_function callee the_module with
        | Some callee -> callee
        | None -> raise (Error "unknown function referenced")
      in
      let params = params callee in
      (* If argument mismatch error. *)
      if Array.length params == Array.length args then () else
        raise (Error "incorrect # arguments passed");
      let args = Array.map codegen_expr args in
      build_call callee args "calltmp" builder
let codegen_proto = function
  | Ast.Prototype (name, args) ->
      (* Make the function type: double(double,double) etc. *)
      let doubles = Array.make (Array.length args) double_type in
      let ft = function_type double_type doubles in
      let f =
        match lookup_function name the_module with
        | None -> declare_function name ft the_module
        (* If 'f' conflicted, there was already something named 'name'. If it
         * has a body, don't allow redefinition or reextern. *)
        | Some f ->
            (* If 'f' already has a body, reject this. *)
            if block_begin f <> At_end f then
              raise (Error "redefinition of function");
            (* If 'f' took a different number of arguments, reject. *)
            if element_type (type_of f) <> ft then
              raise (Error "redefinition of function with different # args");
            f
      in
      (* Set names for all arguments. *)
      Array.iteri (fun i a ->
        let n = args.(i) in
        set_value_name n a;
        Hashtbl.add named_values n a;
      ) (params f);
      f
let codegen_func = function
  | Ast.Function (proto, body) ->
      Hashtbl.clear named_values;
      let the_function = codegen_proto proto in
      (* Create a new basic block to start insertion into. *)
      let bb = append_block context "entry" the_function in
      position_at_end bb builder;
      try
        let ret_val = codegen_expr body in
        (* Finish off the function. *)
        let _ = build_ret ret_val builder in
        (* Validate the generated code, checking for consistency. *)
        Llvm_analysis.assert_valid_function the_function;
        the_function
      with e ->
        delete_function the_function;
        raise e *)


(*
Code generation: translate takes a semantically checked AST and produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial
  http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:
  http://llvm.moe/
  http://llvm.moe/ocaml/
*)
