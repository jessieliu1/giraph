(* Authors: 
Seth Benjamin sjb2190
Jennifer Bi jb3495
Jessie Liu jll2219
*)

(* Semantically checked AST *)

open Ast

type sexpr =
    SId of string * typ
  | SBinop of sexpr * binop * sexpr * typ
  | SUnop of unop * sexpr * typ
  | SAssign of string * sexpr * typ
  | SMethod of sexpr * string * sexpr list * typ
  | SCall of string * sexpr list * typ
  | SBool_Lit of bool
  | SInt_Lit of int
  | SFloat_Lit of float
  | SString_Lit of string
  (* 1st typ is graph subtype (graph, digraph, wegraph, wedigraph), 2nd is the type of node data *)
  | SGraph_Lit of string list * (string * string * sexpr) list * (string * sexpr) list * typ * typ
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


(* Pretty-printing functions *)

let rec string_of_sexpr = function
    SBool_Lit(true) -> "true"
  | SBool_Lit(false) -> "false"
  | SInt_Lit(l) -> string_of_int l
  | SString_Lit(l) -> l
  | SFloat_Lit(l) -> string_of_float l
  | SId(s, t) -> s ^ ":" ^ string_of_typ t
  | SBinop(e1, o, e2, t) ->
      string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^
      string_of_sexpr e2 ^ ":" ^ string_of_typ t
  | SUnop(o, e, t) -> string_of_uop o ^ string_of_sexpr e ^ ":" ^ string_of_typ t
  | SAssign(v, e, t) -> v ^ " = " ^ string_of_sexpr e^ ":" ^ string_of_typ t
  | SCall(f, el, t) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"^ ":" ^ string_of_typ t
  | SGraph_Lit(node_l, edge_l, node_init_l, g_typ, n_typ) ->
    "[" ^ String.concat ", " node_l ^ "] " ^
    "[" ^ String.concat ", " (List.map (fun(f,t,w) -> "(" ^ f ^ "," ^ t ^ "," ^ string_of_sexpr w ^ ")") edge_l) ^ "] " ^
    "[" ^ String.concat ", " (List.map (fun(n,e) -> "(" ^ n ^ "," ^ string_of_sexpr e ^ ")") node_init_l) ^
    "] : " ^ string_of_typ g_typ ^ ", " ^ string_of_typ n_typ
  | SMethod(i, m, e, r_typ) -> string_of_sexpr i ^ "." ^ m ^ "(" ^ String.concat ", " (List.map string_of_sexpr e) ^ "):" ^ string_of_typ r_typ
  | SNoexpr -> ""


let rec string_of_sstmt = function
    SBlock(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr(expr, t) -> string_of_sexpr expr ^ ";\n";
  | SVdecl(t, id, SNoexpr) -> string_of_typ t ^ " " ^ id ^ ";\n"
  | SVdecl(t, id, a_expr) -> string_of_typ t ^ " " ^ string_of_sexpr a_expr ^ ";\n"
  | SReturn(expr) -> "return " ^ string_of_sexpr expr ^ ";\n";
  | SIf(e, s, SBlock([])) -> "if (" ^ string_of_sexpr e ^ ")\n" ^ string_of_sstmt s
  | SIf(e, s1, s2) ->  "if (" ^ string_of_sexpr e ^ ")\n" ^
      string_of_sstmt s1 ^ "else\n" ^ string_of_sstmt s2
  | SFor(e1, e2, e3, s) ->
      "for (" ^ string_of_sexpr e1  ^ " ; " ^ string_of_sexpr e2 ^ " ; " ^
      string_of_sexpr e3  ^ ") " ^ string_of_sstmt s
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s
  | SFor_Node(n, g, sl) -> "for_node (" ^ n ^ " : " ^ string_of_sexpr g ^ ") " ^ string_of_sstmt sl
  | SFor_Edge(e, g, sl) -> "for_edge (" ^ e ^ " : " ^ string_of_sexpr g ^ ") " ^ string_of_sstmt sl
  | SBfs(n, g, src, sl) -> "bfs (" ^ n ^ " : " ^ string_of_sexpr g ^ " ; " ^
                          string_of_sexpr src ^ ") " ^ string_of_sstmt sl



let string_of_svdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_sfdecl fdecl =
  string_of_typ fdecl.sf_typ ^ " " ^
  fdecl.sf_name ^ "(" ^ String.concat ", " (List.map snd fdecl.sf_formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_sstmt fdecl.sf_body) ^
  "}\n"

let string_of_sprogram (vars, funcs) =
  String.concat "" (List.map string_of_svdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_sfdecl funcs)
