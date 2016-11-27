(* ------------------- Analyzer ------------------- *)
exception VariableNotDefined of string
exception ReturntypeNotMatch of string
exception VariableDeclarationNotMatch of string
exception UndefinedID of string
exception ShouldNotHappenUnlessCompilerHasBug of string
exception FuncNotFound of string * string

(* ------------------- Codegen ------------------- *)
exception LLVMFunctionNotFound of string
