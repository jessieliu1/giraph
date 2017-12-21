(* Authors: 
Daniel Benett deb2174
Seth Benjamin sjb2190
Jennifer Bi jb3495
Jessie Liu jll2219
*)

(* Top-level of the Giraph compiler: scan & parse the input,
   check the resulting AST and generate SAST, generate LLVM IR, 
   and dump the module *)

module StringMap = Map.Make(String)

type action = Ast | LLVM_IR | Compile

let _ =
    let action = ref Compile in
    let set_action a () = action := a in
    let speclist = [
      ("-a", Arg.Unit (set_action Ast), "Print the SAST");
      ("-l", Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR");
      ("-c", Arg.Unit (set_action Compile),
        "Check and print the generated LLVM IR (default)");
    ] in  
    let usage_msg = "usage: ./giraph.native [-a|-l|-c] [file.gir]" in
    let channel = ref stdin in
    Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;
    let lexbuf = Lexing.from_channel !channel in
    let ast = Parser.program Scanner.token lexbuf in
    let sast = Semant.check ast in
    match !action with
        Ast -> (*print_string (Ast.string_of_program ast)*) print_string (Sast.string_of_sprogram sast)
      | LLVM_IR -> print_string (Llvm.string_of_llmodule (Codegen.translate sast))
      | Compile -> let m = Codegen.translate sast in
        Llvm_analysis.assert_valid_module m;
        print_string (Llvm.string_of_llmodule m)
