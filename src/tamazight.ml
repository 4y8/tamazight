open Syntax

let stdlib = Type.SMap.of_list
    ["array_int", ([TInt], TArr TInt, 0);
     "print_string", ([TArr TInt], TUnit, 1);
     "len", ([TArr TAny], TInt, 2);
     "open", ([TArr TInt], TFile, 3);
     "close", ([TFile], TUnit, 4);
     "list_dir", ([TFile], TArr (TArr TInt), 5);
     "exec", ([TArr TInt], TUnit, 6);
     "key_wait", ([], TInt, 7)]

let n_stdlib = 8

let read_file f =
  let ic = open_in f in
  let lexbuf = Lexing.from_channel ic in
  let prog = Parser.file Lexer.lexer lexbuf in
  let ir = Type.translate_program stdlib n_stdlib prog in
  let instr = Compile.compile_program ir in
  Emit.emit_program instr;
  close_in ic

let _ = Arg.parse [] read_file ""
