open Ast;;

let check (btmodule) =

  (* Raise an exception if the given list has a duplicate *)
  let report_duplicate exceptf list =
    let rec helper = function
	n1 :: n2 :: _ when n1 = n2 -> raise (Failure (exceptf n1))
      | _ :: t -> helper t
      | [] -> ()
    in helper (List.sort compare list)
  in

  (* Raise an exception if a given binding is to a Unit type *)
  let check_not_Unit exceptf = function
      (Primitive(Unit), n) -> raise (Failure (exceptf n))
    | _ -> ()
  in

  (* Raise an exception of the given rvalue type cannot be assigned to
     the given lvalue type *)
  let check_assign lvaluet rvaluet err =
     if lvaluet == rvaluet then lvaluet else raise err
  in

  List.iter (check_not_Unit (fun n -> "illegal Unit global " ^ n)) btmodule.funcs.formals;

  report_duplicate (fun n -> "duplicate global " ^ n) (List.map snd btmodule.funcs.formals);


  