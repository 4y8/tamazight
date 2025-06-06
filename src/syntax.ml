type ty = TFile | TDir | TInt | TArr of ty | TUnit | TAny

type uop = Not
type bop = And | Or | Pls | Min | Mul | Div | Eq | Neq | Gt | Ge | Lt | Le | Apnd

type 'a expr
  = Bop of bop * 'a expr * 'a expr
  | Uop of uop * 'a expr
  | Acc of 'a * 'a expr
  | Lit of int
  | Str of string
  | Var of 'a
  | Cal of 'a * 'a expr list

type 'a stmt
  = If of 'a expr * 'a stmt list * 'a stmt list
  | While of 'a expr * 'a stmt list
  | Decl of 'a * 'a expr
  | Expr of 'a expr
  | AAss of 'a * 'a expr * 'a expr
  | SAss of 'a * 'a expr
  | Ret of 'a expr
  | Blk of 'a stmt list

type fun_decl =
  { f : string ; x : (string * ty) list ; ret : ty; body : string stmt list }
