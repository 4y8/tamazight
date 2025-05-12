let stdlib = Type.SMap.empty
let n_stdlib = 0


let read_file f =
  let ic = open_in f in
  let lexbuf = Lexing.from_channel ic in
  let prog = Parser.file Lexer.lexer lexbuf in
  let ir = Type.translate_program stdlib n_stdlib prog in
  let instr = Compile.compile_program ir in
  Emit.emit_program instr;
  close_in ic

let _ = Arg.parse [] read_file ""
