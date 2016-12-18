(*
 * Authors:
 *  - Ruonan Xu
 *	- Jake Kwon
 *)

exception Impossible of string

(* ------------------- Scanner ------------------- *)
exception Lexing_error of string

(* ------------------- Analyzer ------------------- *)
exception ReturntypeNotMatch of string
exception FuncCallCheckFail of string
exception CheckFbodyFail of string
exception IfComparisonNotBool of string
exception ReturnTypeMismatch of string * string
exception InvalidForStatementType of string
exception CannotCallBreakOutsideOfLoop
exception CannotCallContinueOutsideOfLoop
exception InvalidWhileStatementType
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

(* ------------------- Environment ------------------- *)

(* ------------------- Codegen :: Testing DONE ------------------- *)
exception LLVMFunctionNotFound of string
exception InvalidTypePassedToPrint
exception ExpressionNotAssignable of string


(*
   exception VarAlreadyDeclared;;
   exception VarNotDeclared;;
   exception IncorrectFunctionParameterTypes;;
   exception IncorrectArrayAssignmentSize;;
*)
