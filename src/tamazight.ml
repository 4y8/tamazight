open Syntax

let stdlib = Type.SMap.of_list
    ["array_int", ([TInt], TArr TInt, 0);
     "print_string", ([TArr TInt], TUnit, 1);
     "len", ([TArr TAny], TInt, 2);
     "open_file", ([TArr TInt], TFile, 3);
     "close_file", ([TFile], TUnit, 4);
     "list_dir", ([TDir], TArr (TArr TInt), 5);
     "exec", ([TArr TInt], TUnit, 6);
     "key_wait", ([], TInt, 7);
     "open_dir", ([TArr TInt], TDir, 8);
     "close_dir", ([TDir], TUnit, 9);
     "poweroff", ([], TUnit, 10);
     "read_file", ([TFile], TArr TInt, 11);
     "clear_screen", ([], TUnit, 12)]

let n_stdlib = 13

let read_file f =
  let ic = open_in f in
  let lexbuf = Lexing.from_channel ic in
  let prog = Parser.file Lexer.lexer lexbuf in
  let ir = Type.translate_program stdlib n_stdlib prog in
  let instr = Compile.compile_program ir in
  Emit.emit_program instr;
  close_in ic

let _ = Arg.parse [] read_file ""
