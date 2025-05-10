%{
    open Syntax
%}

%token FUN IF ELSE WHILE RETURN TINT
%token WAL ASS EQ GT GE LT LE OR AND PLUS MINUS TIMES DIV NOT DIFF
%token DCOL SCOL COMMA LPAR RPAR LCUR RCUR LSQU RSQU
%token <string> IDENT
%token <int> INT
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
;

stmt:
  | v = IDENT WAL e = expr SCOL { Decl (v, e) }
  | WHILE e = expr LCUR s = stmt* RCUR { While (e, s) }
  | IF e = expr LCUR s = stmt* RCUR { If (e, s, []) }
  | IF e = expr LCUR s1 = stmt* RCUR ELSE LCUR s2 = stmt* RCUR
      { If (e, s1, s2) }
  | v = IDENT ASS e = expr { SAss (v, e) }
  | v = IDENT LSQU e1 = expr RSQU ASS e2 = expr { AAss (v, e1, e2) }
  | RETURN e = expr SCOL { Ret e }
  | f = IDENT LPAR x = separated_list(COMMA, expr) RPAR { Expr (Cal (f, x)) }
;

ty:
  | TINT { TInt }
  | LSQU t = ty RSQU { TArr t }
;

formal: x = IDENT DCOL t = ty { (x, t) } ;

func:
    FUN f = IDENT LPAR x = separated_list(COMMA, formal) RPAR
      LCUR body = stmt* RCUR
    { {f; x; body} }
;

file: p = func* EOF { p } ;
