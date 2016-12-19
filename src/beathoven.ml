(*
 * Authors:
 *  - Ruonan Xu
 *  - Jake Kwon
 *  - Eunice Kokor
 *)

type action = Compile | Help | Raw | Sast

let get_help =
  "Beathoven Usage: beathoven.sh <flag> [input_file] [output_file]\n" ^
  "  -c\tCompile beathoven input_file to c code in output_file with stdlib\n" ^
  "  -h\tDisplay this list of options\n" ^
  "  -r\tCompile beathoven input_file into raw c output_file\n"

(* Error reporting helper function *)
let get_pos_and_tok lexbuf =
  let cur = lexbuf.Lexing.lex_curr_p in
  let line_num = cur.Lexing.pos_lnum and
    column_num = cur.Lexing.pos_cnum - cur.Lexing.pos_bol and
    token = Lexing.lexeme lexbuf in
  line_num, column_num, token


let _ =
  let action = List.assoc Sys.argv.(1)
        [("-c", Compile) ; ("-h", Help) ; ("-r", Raw); ("-s", Sast)] in
  if action = Help then print_endline get_help
    else
  let lexbuf = Lexing.from_channel stdin in
  try
    let ast = Parser.program Scanner.token lexbuf in
    let sast = Analyzer.analyze_ast ast in
    match action with
    | Sast -> print_string (Yojson.Basic.pretty_to_string (Pprint.json_of_program sast))
    | Raw -> let output_file = Sys.argv.(2) in
          let file = open_out output_file in
          let m = Codegen.codegen_program sast in
      (* Llvm_analysis.assert_valid_module m; *) (* Useful built-in check *)

          Printf.fprintf file "%s\n" (Llvm.string_of_llmodule m); close_out file
    | Compile ->
    let m = Codegen.codegen_program sast in
      (* Llvm_analysis.assert_valid_module m; *) (* Useful built-in check *)
      print_string (Llvm.string_of_llmodule m)
      (* let output_file = Sys.argv.(2) and stdlib_file = Sys.argv.(3) in
      let stdlib = Utils.str_of_file stdlib_file in
      let file = open_out output_file
      in fprintf file "%s\n\n%s\n%s\n"
        stdlib prog (Utils.conclude_program ()); close_out file *)
    | Help -> print_string get_help
  with
    (* Must add rule for Analyzer *)(*
    | Scanner.Illegal_Character(m) ->
      let line_num, column_num, _ = get_pos_and_tok lexbuf in
        eprintf
          "\x1b[31mSyntax error\x1b[0m, line %d at column %d: %s\n"
          line_num column_num m *)
    | Parsing.Parse_error ->
        let line_num, column_num, token = get_pos_and_tok lexbuf in
        let errormsg = "parser error line " ^ string_of_int line_num ^ " at column " ^ string_of_int column_num ^ ": " ^ token in
        Log.error (errormsg)
