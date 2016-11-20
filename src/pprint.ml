(* Pretty Print  *)
open Ast

let ast_data_to_string (dt : Ast.data_type) = match dt
  with Int -> "int"
  | Double -> "Double"
  | Bool -> "bool"
  | String -> "string"