type binop = Add | Sub | Mult | Div | Mod | Eq | Neq | 
          Less | Leq | Greater | Geq | And | Or
type unop = Neg | Not
type typ = Int | Bool | Void | Graph
type bind = typ * string

type expr = Literal of int
        | Id of string
        | Binop of expr * binop * expr
        | Unop of uop * expr
        | Assign of string * expr
        | Bool_Lit of bool
        | Int_Lit of int
        | Access of expr * expr
        | Func_Lit of func
        | Func_call of string * expr list
        | Noexpr

and stmt = Block of stmt list
        | If of expr * stmt * stmt
        | For of expr * expr * expr * stmt
        | While of expr * stmt
        | Expr of expr
        | Vdecl of val_decl


type func_decl = {
        f_typ : typ;
        f_name : string;
        f_formals : bind list;
        f_locals : bind list;
        f_body : stmt list;
}

type val_decl = {
        v_name : string;
        v_type : typ;
        v_init : expr;
}


type program = bind list * func_decl list
