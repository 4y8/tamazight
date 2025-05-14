{
  open Parser
}

let digit = ['0'-'9']
let alpha = ['A'-'Z'] | '_' | ['a'-'z']
let ident = alpha (alpha | digit)*

rule lexer = parse
  | [' ' '\t' '\r' '\n'] { lexer lexbuf }
  | '#' [^'\n']* '\n' { lexer lexbuf }
  | "+" { PLUS }
  | "++" { DPLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "/" { DIV }
  | "!" { NOT }
  | "&&" { AND }
  | "||" { OR }
  | "==" { EQ }
  | "!=" { DIFF}
  | "<" { LT }
  | "<=" { LE}
  | ">" { GT }
  | ">=" { GE }
  | "=" { ASS }
  | ":=" { WAL }

  | ":" { DCOL }
  | ";" { SCOL }
  | "," { COMMA }
  | "(" { LPAR }
  | ")" { RPAR }
  | "[" { LSQU }
  | "]" { RSQU }
  | "{" { LCUR }
  | "}" { RCUR }

  | "fun" { FUN }
  | "if" { IF }
  | "elif" { ELIF }
  | "for" { FOR }
  | "in" { IN }
  | "else" { ELSE }
  | "while" { WHILE }
  | "return" { RETURN }
  | "int" { TINT }
  | "file" { FILE }
  | "unit" { UNIT }
  | "directory" { DIRECTORY }
  | ident as s { IDENT s }

  | "'" ([^ '\\'] as c) "'" { INT (Char.code c) }
  | "'\\n'" { INT (Char.code '\n') }
  | "'\\t'" { INT (Char.code '\t') }
  | "\"" ([^'\\''"']* as s) "\"" { STRING s }

  | digit+ as n { INT (int_of_string n) }

  | eof { EOF }
  | _ as c { failwith (Printf.sprintf "unexpected char: %d" (Char.code c)) }

{
}
