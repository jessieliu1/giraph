(* Semantically checked AST *)

open Ast

type sexpr =
    SId of string * typ
  | SBinop of sexpr * binop * sexpr * typ
  | SUnop of unop * sexpr * typ
  | SAssign of string * sexpr * typ
  | SCall of string * sexpr list * typ
  | SBool_Lit of bool
  | SInt_Lit of int
  | SFloat_Lit of float
  | SString_Lit of string
  (* TODO: graph things *)
  | SNode of string * typ
  | SEdge of edge * typ 
  | SGraph_Lit of string list * edge list * (string * expr) list * typ
  | SNoexpr

type svdecl = {
(* do we want this *) 
  sv_name : string;
  sv_type : typ;
  sv_init : sexpr;
}

(* type expr_det = sexpr * typ *) 
(* remove because will prob want to use sexpr in build 
in codegen*)

type sstmt =
    SBlock of sstmt list
  | SIf of sexpr * sstmt * sstmt
  | SFor of sexpr * sexpr * sexpr * sstmt
  | SWhile of sexpr * sstmt
  | SFor_Node of string * sexpr * sstmt
  | SFor_Edge of string * sexpr * sstmt
  | SBfs of string * sexpr * sexpr * sstmt
  | SDfs of string * sexpr * sexpr * sstmt
  | SBreak 
  | SContinue 
  | SExpr of sexpr * typ
  | SVdecl of typ * string * sexpr
  | SReturn of sexpr



type sfdecl = {
  sf_typ : typ;
  sf_name : string;
  sf_formals : bind list; 
  sf_body : sstmt list;
}

type sprogram = bind list * sfdecl list


(* THESE will work because we did open AST but they will pretty print AST types rather than SAST types *)
(* Pretty-printing functions *)

let sstring_of_op = function
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


let sstring_of_uop = function
    Neg -> "-"
  | Not -> "!"


let sstring_of_typ = function
    Int -> "int"
  | Float -> "float"
  | Bool -> "bool"
  | String -> "str"
  | NodeTyp -> "node"
  | Graph -> "graph"
  | EdgeTyp -> "edge"
  | Void -> "void"


let rec string_of_expr = function
    SBool_Lit(true) -> "true"
  | SBool_Lit(false) -> "false"
  | SInt_Lit(l) -> string_of_int l
  | SString_Lit(l) -> l
  | SFloat_Lit(l) -> string_of_float l
  | SId(s, t) -> s ^ ":" ^ string_of_typ t
  | SBinop(e1, o, e2, t) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^
      string_of_expr e2 ^ ":" ^ string_of_typ t
  | SUnop(o, e, t) -> string_of_uop o ^ string_of_expr e ^ ":" ^ string_of_typ t
  | SAssign(v, e, t) -> v ^ " = " ^ string_of_expr e^ ":" ^ string_of_typ t
  | SCall(f, el, t) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"^ ":" ^ string_of_typ t
  | SGraph_Lit(node_l, edge_l, node_init_list, t) ->
    "[" ^ String.concat ", " node_l ^ "] " ^
    "[" ^ String.concat ", " (List.map (fun(a,b) -> "(" ^ a ^ "," ^ b ^ ")") edge_l) ^ "]"
  | SNoexpr -> ""


(*let rec sstring_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map sstring_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> sstring_of_expr expr ^ ";\n";
  | Vdecl(t, id, x) -> sstring_of_typ t ^ " " ^ id ^ ";\n"
  | Return(expr) -> "return " ^ sstring_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ sstring_of_expr e ^ ")\n" ^ sstring_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ sstring_of_expr e ^ ")\n" ^
      sstring_of_stmt s1 ^ "else\n" ^ sstring_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ sstring_of_expr e1  ^ " ; " ^ sstring_of_expr e2 ^ " ; " ^
      sstring_of_expr e3  ^ ") " ^ sstring_of_stmt s
  | While(e, s) -> "while (" ^ sstring_of_expr e ^ ") " ^ sstring_of_stmt s


let sstring_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let sstring_of_fdecl fdecl =
  sstring_of_typ fdecl.f_typ ^ " " ^
  fdecl.f_name ^ "(" ^ String.concat ", " (List.map snd fdecl.f_formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_stmt sfdecl.sf_body) ^
  "}\n"

let sstring_of_program (vars, funcs) =
  String.concat "" (List.map sstring_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map sstring_of_fdecl funcs)*)

