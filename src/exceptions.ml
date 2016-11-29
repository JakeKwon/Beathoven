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

(* ------------------- Codegen ------------------- *)
exception LLVMFunctionNotFound of string
exception InvalidTypePassedToPrintf
