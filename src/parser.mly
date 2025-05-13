%{
    open Syntax

    let nvar = ref 0

    let new_var () = incr nvar; Printf.sprintf "x%d" !nvar
%}

%token FUN IF ELIF ELSE WHILE FOR IN RETURN TINT FILE UNIT DIRECTORY
%token WAL ASS EQ GT GE LT LE OR AND PLUS MINUS TIMES DIV NOT DIFF
%token DCOL SCOL COMMA LPAR RPAR LCUR RCUR LSQU RSQU
%token <string> IDENT
%token <int> INT
%token <string> STRING
%token EOF
%start file
%type <fun_decl list> file

%left EQ DIFF
%left LT LE GT GE
%left OR AND
%left PLUS MINUS
%left TIMES DIV
%nonassoc NOT

%%

let op ==
    AND; { And } | OR; { Or } | PLUS; { Pls } | MINUS; { Min } | TIMES; { Mul }
  | DIV; { Div } | EQ; { Eq } | DIFF; { Neq } | GT; { Gt } | GE; { Ge }
  | LT; { Lt } | LE; { Le }

expr:
  | LPAR e = expr RPAR { e }
  | v = IDENT { Var v }
  | v = IDENT LSQU e = expr RSQU { Acc (v, e) }
  | n = INT { Lit n }
  | f = IDENT LPAR x = separated_list(COMMA, expr) RPAR { Cal (f, x) }
  | e1 = expr o = op e2 = expr { Bop (o, e1, e2) }
  | NOT e = expr { Uop (Not, e) }
  | s = STRING { Str s }
;

elif:
  | ELIF e = expr LCUR s = stmt* RCUR { e, s }
;

stmt:
  | v = IDENT WAL e = expr SCOL { Decl (v, e) }
  | WHILE e = expr LCUR s = stmt* RCUR { While (e, s) }
  | IF e = expr LCUR s = stmt* RCUR f = elif*
      { If (e, s, List.fold_right (fun (e, s) b -> [If (e, s, b)]) f []) }
  | IF e = expr LCUR s1 = stmt* RCUR f = elif* ELSE LCUR s2 = stmt* RCUR
      { If (e, s1, List.fold_right (fun (e, s) b -> [If (e, s, b)]) f s2) }
  | v = IDENT ASS e = expr SCOL { SAss (v, e) }
  | v = IDENT LSQU e1 = expr RSQU ASS e2 = expr SCOL { AAss (v, e1, e2) }
  | RETURN e = expr SCOL { Ret e }
  | f = IDENT LPAR x = separated_list(COMMA, expr) RPAR SCOL
      { Expr (Cal (f, x)) }
  | FOR v = IDENT IN e = expr LCUR s = stmt* RCUR
      { let i = new_var () in
        let a = new_var () in
        let n = new_var () in
        Blk [Decl (i, Lit 0); Decl (a, e);
             Decl (n, Cal ("len", [Var a]));
             While (Bop (Lt, Var i, Var n),
                    [Decl (v, Acc (a, Var i))] @ s @
                      [SAss (i, Bop (Pls, Var i, Lit 1))])] }
;

ty:
  | TINT { TInt }
  | FILE { TFile }
  | UNIT { TUnit }
  | LSQU t = ty RSQU { TArr t }
  | DIRECTORY { TDir }
;

formal: x = IDENT DCOL t = ty { (x, t) } ;

func:
    FUN f = IDENT LPAR x = separated_list(COMMA, formal) RPAR DCOL ret = ty
      LCUR body = stmt* RCUR
    { {f; x; ret; body} }
;

file: p = func* EOF { p } ;
