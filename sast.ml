open Ast

type sbind = typ * svdecl

type sexpr =
    SId of svdecl
  | SBinop of expr * binop * expr
  | SUnop of unop * expr
  | SAssign of string * expr
  | SCall of string * expr list
  | SBool_Lit of bool
  | SInt_Lit of int
  | SFloat_Lit of float
  | SString_Lit of string
  | SNode of string
  | SEdge of edge
  | SGraph of string list * edge list
  | SNoexpr

type expr_det = sexpr * typ

type sstmt =
    SBlock of sstmt list
  | SIf of expr_det * sstmt * sstmt
  | SFor of expr_det * expr_det * expr_det * sstmt
  | SWhile of expr_det * sstmt
  | SFor_Node of expr_det * expr_det * sstmt
  | SFor_Edge of expr_det * expr_det * sstmt
  | SBfs of expr_det * expr_det * expr_det * sstmt
  | SDfs of expr_det * expr_det * expr_det * sstmt
  | SBreak of sstmt
  | SContinue of sstmt
  | SExpr of expr_det
  | SVdecl of svdecl * expr_det
  | SReturn of expr_det
type svdecl = {
(* do we want this *) 
  sv_name : string;
  sv_type : typ;
  sv_init : expr_det;
}

type sfdecl = {
  sf_typ : typ;
  sf_name : string;
  sf_formals : bind list;
  sf_body : stmt list;
}

type translation_env = {
  scope : sym_tab;
  break_label : label option;
  cont_label : label option;
 (* exception_scope: exception_scope;*)
  labels : label list ref;
  forward_gotos : label list ref;
}

type sym_tab = {
  parent : sym_tab option;
  variables : vdecl list
}

type sprogram = sbind list * sfdecl list
(* THESE will work because we did open AST but they will pretty print AST types rather than SAST types *)
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
  | Vdecl(t, id, x) -> string_of_typ t ^ " " ^ id ^ ";\n"
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
