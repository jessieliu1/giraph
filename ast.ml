type binop = Add | Sub | Mult | Div | Mod | Eq | Neq | 
          Less | Leq | Greater | Geq | And | Or
type unop = Neg | Not
type typ = Int | Float | Bool | Void | Char | Str | Node | Graph | Edge
type bind = typ * string

type expr =
    Literal of int
  | Id of string
  | Binop of expr * binop * expr
  | Unop of unop * expr
  | Assign of string * expr
  | Call of string * expr list
  | Bool_Lit of bool
  | Access of expr * expr
  | Noexpr

type vdecl = {
  v_typ : typ;
  v_name : string;
  v_init : expr;
}

type stmt =
    Block of stmt list
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  | For_Node of expr * expr * stmt
  | For_Edge of expr * expr * stmt
  | Bfs of expr * expr * expr * stmt
  | Dfs of expr * expr * expr * stmt
  | Break
  | Continue
  | Expr of expr
  | Vdecl of vdecl
  | Return of expr


type fdecl = {
    f_typ : typ;
    f_name : string;
    f_formals : bind list;
    f_locals : vdecl list;
    f_body : stmt list;
}


type program = vdecl list * fdecl list
