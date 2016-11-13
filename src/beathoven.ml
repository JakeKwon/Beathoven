(* open Printf *)

type action = Compile | Help | Raw | Sast

let get_help =
  "Beathoven Usage: beathoven.sh <flag> [input_file] [output_file]\n" ^
  "  -c\tCompile beathoven input_file to c code in output_file with stdlib #NOT IMPLEMENTED#\n" ^
  "  -h\tDisplay this list of options\n" ^
  "  -r\tCompile beathoven input_file into raw c output_file #NOT IMPLEMENTED#\n"

(* Error reporting helper function *)
let get_pos_and_tok lexbuf =
  let cur = lexbuf.Lexing.lex_curr_p in
  let line_num = cur.Lexing.pos_lnum and
    column_num = cur.Lexing.pos_cnum - cur.Lexing.pos_bol and
    token = Lexing.lexeme lexbuf in
  line_num, column_num, token


let _ =
  let action =
    if Array.length Sys.argv > 1 then
      List.assoc Sys.argv.(1)
        [("-c", Compile) ; ("-h", Help) ; ("-r", Raw); ("-s", Sast)]
    else Sast in
  let lexbuf = Lexing.from_channel stdin in
  (* try *)
    let ast = Parser.program Scanner.token lexbuf in
    Semant.check ast;
    (* let sast = Analyzer.check_ast ast in
    let past = Pythonizer.generate_past sast in
    let prog = Generator.gen_program ast in *)
    match action with
      Sast -> () (* print_string (Ast.string_of_program ast) *)
    | Raw -> () (* print_string (Llvm.string_of_llmodule (Codegen.translate ast)) *)
    | Compile -> let m = Codegen.codegen_main ast in
      (* Llvm_analysis.assert_valid_module m; *)
      print_string (Llvm.string_of_llmodule m)
      (* let output_file = Sys.argv.(2) and stdlib_file = Sys.argv.(3) in
      let stdlib = Utils.str_of_file stdlib_file in
      let file = open_out output_file
      in fprintf file "%s\n\n%s\n%s\n"
        stdlib prog (Utils.conclude_program ()); close_out file *)
    | Help -> print_endline get_help
  (* with
    (* Must add rule for Analyzer *)
    | Scanner.Illegal_Character(m) ->
      let line_num, column_num, _ = get_pos_and_tok lexbuf in
        eprintf
          "\x1b[31mSyntax error\x1b[0m, line %d at column %d: %s\n"
          line_num column_num m
    | Parsing.Parse_error ->
        let line_num, column_num, token = get_pos_and_tok lexbuf in
        eprintf
        "\x1b[31mSyntax error\x1b[0m, line %d at column %d: '%s'\n"
        line_num column_num token
 *)
