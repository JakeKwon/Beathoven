(*
 * Authors:
 *  - Ruonan Xu
 *	- Jake Kwon
 *)

exception Impossible of string
exception ErrorReportedByLog

(* ------------------- Scanner ------------------- *)
exception Lexing_error of string

(* ------------------- Analyzer ------------------- *)
exception ReturntypeNotMatch of string
exception FuncCallCheckFail of string
exception CheckFbodyFail of string
exception ReturnTypeMismatch of string * string
exception CannotCallBreakOutsideOfLoop
exception CannotCallContinueOutsideOfLoop
exception InvalidUnaryOperation

(* Variables  *)
exception VariableNotDefined of string
exception DuplicateVariable of string
(* Vardecl *)
exception UnitTypeError
exception VardeclTypeMismatch of string * string
exception InvalidPitchAssignment of string
(* Operators *)
exception InvalidBinopExpression of string
(* Assign *)
exception AssignTypeMismatch of string * string
(* Functions *)
exception DuplicateFunction of string
exception CannotUseBuiltinFuncName of string
exception FuncNotFound of string * string
exception ParamNumberNotMatch of string
exception ParamTypeNotMatch of string
(* Array *)
exception ArrayTypeNotMatch of string
exception ShouldAccessArray of string
(* Struct :: Testing DONE *)
exception UndefinedStructType of string
exception ShouldAccessStructType of string
exception StructFieldNotFound of string * string
(* Statement *)
exception InvalidConditionType

(* ------------------- Codegen :: Testing DONE ------------------- *)
 (* TODO: Only runtime error should be here. *)
exception LLVMFunctionNotFound of string
exception InvalidTypePassedToPrint
exception ExpressionNotAssignable of string

(*
   exception IncorrectArrayAssignmentSize
*)
