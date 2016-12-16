(*
 * Authors:
 *  - Ruonan Xu
 *)

exception Impossible of string

(* ------------------- Scanner ------------------- *)
exception Lexing_error of string

(* ------------------- Analyzer ------------------- *)
exception VariableNotDefined of string
exception ReturntypeNotMatch of string
exception VariableDeclarationNotMatch of string
exception ShouldNotHappenUnlessCompilerHasBug of string
exception FuncNotFound of string * string
exception DuplicateVariable of string
exception FuncCallCheckFail of string
exception CheckFbodyFail of string
exception IfComparisonNotBool of string
exception InvalidBinopExpression of string
exception ReturnTypeMismatch of string * string
exception InvalidForStatementType of string
exception CannotCallBreakOutsideOfLoop
exception CannotCallContinueOutsideOfLoop
exception InvalidWhileStatementType
exception InvalidUnaryOperation
exception UnitTypeError of string
exception DuplicateFunction of string

(* Array *)
exception ArrayTypeNotMatch of string
exception ShouldAccessArray of string
(* Assign *)
exception AssignmentTypeMismatch of string * string
exception CannotUseBuiltinFuncName of string
(* Struct *)
exception UndefinedStructType of string
exception ShouldAccessStructType of string
exception StructFieldNotFound of string * string

(* ------------------- Environment ------------------- *)
exception UndefinedID of string

(* ------------------- Codegen ------------------- *)
exception LLVMFunctionNotFound of string
exception InvalidTypePassedToPrint
exception ExpressionNotAssignable of string


(*
   exception VarAlreadyDeclared;;
   exception VarNotDeclared;;
   exception FunctionAlreadyDeclared;;
   exception FunctionNotDeclared;;
   exception IncorrectFunctionParameterTypes;;
   exception MixedTypeArray;;
   exception ArrayInferTypeMismatch;;
   exception IncorrectArrayAssignmentSize;;
*)
