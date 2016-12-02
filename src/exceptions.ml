(* ------------------- Analyzer ------------------- *)
exception VariableNotDefined of string
exception ReturntypeNotMatch of string
exception VariableDeclarationNotMatch of string
exception UndefinedID of string
exception ShouldNotHappenUnlessCompilerHasBug of string
exception FuncNotFound of string * string
exception DuplicateLocal of string
exception VarDeclCheckFail of string
exception FuncCallCheckFail of string
exception CheckFbodyFail of string
exception IfComparisonNotBool of string
exception InvalidBinopExpression of string
exception ReturnTypeMismatch of string * string
exception InvalidForStatementType of string
exception CannotCallBreakOutsideOfLoop
exception CannotCallContinueOutsideOfLoop
exception InvalidWhileStatementType
(* ------------------- Codegen ------------------- *)
exception LLVMFunctionNotFound of string
exception InvalidTypePassedToPrintf
