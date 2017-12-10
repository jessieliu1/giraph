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
  | SGraph of string list * edge list * typ
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
  | Node -> "node"
  | Graph -> "graph"
  | Edge -> "edge"
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
  | SGraph(node_l, edge_l, t) ->
    "[" ^ String.concat ", " node_l ^ "] " ^
    "[" ^ String.concat ", " (List.map (fun(a,b) -> "(" ^ a ^ "," ^ b ^ ")") edge_l) ^ "]"
  | SNoexpr -> ""

let rec string_of_stmt = function
    SBlock(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | SExpr(expr, t) -> string_of_expr expr ^ ":" ^ string_of_typ t^ ";\n";
  | SVdecl(t, id, x) -> string_of_typ t ^ " " ^ id ^ ";\n"
  | SReturn(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | SIf(e, s, SBlock([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | SFor(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | SFor_Node(s1,  e, s2) ->
      "for ("^ s1 ^" ; " ^ string_of_expr e ^ " ; ) " ^ string_of_stmt s2
  | SFor_Edge(s1,  e, s2) ->
      "for ("^ s1 ^" ; " ^ string_of_expr e ^ " ; ) " ^ string_of_stmt s2
  | SWhile(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s


let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl sfdecl =
  string_of_typ sfdecl.sf_typ ^ " " ^
  sfdecl.sf_name ^ "(" ^ String.concat ", " (List.map snd sfdecl.sf_formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_stmt sfdecl.sf_body) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
