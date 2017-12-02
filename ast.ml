type binop = Add | Sub | Mult | Div | Mod | Eq | Neq | 
             Less | Leq | Greater | Geq | And | Or

type unop = Neg | Not

type typ = Int | Float | Bool | Void | String | Node | Graph | Edge

type bind = typ * string

type edge = string * string

type expr =
    Id of string
  | Binop of expr * binop * expr
  | Unop of unop * expr
  | Assign of string * expr
  | Call of string * expr list
  | Bool_Lit of bool
  | Int_Lit of int
  | Float_Lit of float
  | String_Lit of string
  | Node of string
  | Edge of string * string
  | Graph of string list * edge list
  | Noexpr

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
  | Vdecl of typ * string * expr
  | Return of expr


type fdecl = {
  f_typ : typ;
  f_name : string;
  f_formals : bind list;
  f_body : stmt list;
}


type program = bind list * fdecl list

(* Pretty-printing functions *)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Eq -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"


let string_of_uop = function
    Neg -> "-"
  | Not -> "!"


let string_of_typ = function
    Int -> "int"
  | Float -> "float"
  | Bool -> "bool"
  | String -> "str"
  | Node -> "node"
  | Graph -> "graph"
  | Edge -> "edge"
  | Void -> "void"


let rec string_of_expr = function
    Bool_Lit(true) -> "true"
  | Bool_Lit(false) -> "false"
  | Int_Lit(l) -> string_of_int l
  | String_Lit(l) -> l
  | Float_Lit(l) -> string_of_float l
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Graph(node_l, edge_l) ->
    "[" ^ String.concat ", " node_l ^ "] " ^
    "[" ^ String.concat ", " (List.map (fun(a,b) -> "(" ^ a ^ "," ^ b ^ ")") edge_l) ^ "]"
  | Noexpr -> ""

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Vdecl(t, id, Noexpr) -> string_of_typ t ^ " " ^ id ^ ";\n"
  | Vdecl(t, id, e) -> string_of_typ t ^ " " ^ string_of_expr e ^  ";\n"
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s


let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  string_of_typ fdecl.f_typ ^ " " ^
  fdecl.f_name ^ "(" ^ String.concat ", " (List.map snd fdecl.f_formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_stmt fdecl.f_body) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
