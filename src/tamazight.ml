open Syntax

let stdlib = Type.SMap.of_list
    ["zeros", ([TInt], TArr (TInt), 0);
     "print_string", ([TArr (TInt)], TUnit, 1)]
let n_stdlib = 2

let read_file f =
  let ic = open_in f in
  let lexbuf = Lexing.from_channel ic in
  let prog = Parser.file Lexer.lexer lexbuf in
  let ir = Type.translate_program stdlib n_stdlib prog in
  let instr = Compile.compile_program ir in
  Emit.emit_program instr;
  close_in ic

let _ = Arg.parse [] read_file ""
