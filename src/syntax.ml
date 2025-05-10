type ty = TInt | TArr of ty

type uop = Not
type bop = And | Or | Pls | Min | Mul | Div | Eq | Neq | Gt | Ge | Lt | Le

type expr
  = Bop of bop * expr * expr
  | Uop of uop * expr
  | Acc of string * expr
  | Lit of int
  | Var of string
  | Cal of string * expr list

type stmt
  = If of expr * stmt list * stmt list
  | While of expr * stmt list
  | Decl of string * expr
  | Expr of expr
  | AAss of string * expr * expr
  | SAss of string * expr
  | Ret of expr

type fun_decl = { f : string ; x : (string * ty) list ; body : stmt list }
