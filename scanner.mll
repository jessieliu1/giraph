(* Authors: 
Daniel Benett deb2174
Seth Benjamin sjb2190
Jennifer Bi jb3495
Jessie Liu jll2219
*)
(* Ocammllex scanner for giraph *)
{ open Parser }

(* Definitions *)
let digit = ['0'-'9']
let decimal = ((digit+ '.' digit*) | ('.' digit+))
let letter = ['a'-'z' 'A'-'Z']

(* Rules *)
rule token = parse 
    [ ' ' '\t' '\r' '\n'] { token lexbuf } (* to ignore whitespace *)
  | "!~" { comment lexbuf }
  | ',' { COMMA }
  | '.' { DOT }
  | ';' { SEMI }
  | ':' { COLON }
  | '\'' { SINGLEQUOTE }
  | '\"' { DOUBLEQUOTE }

  (* scoping *)
  | '('	{ LPAREN }
  | ')'	{ RPAREN }
  | '{'	{ LBRACE }
  | '}'	{ RBRACE }
  | '['	{ LBRACK }
  | ']'	{ RBRACK }

  (* keywords *)
  | "for" { FOR }
  | "while" { WHILE }
  | "for_node" {FOR_NODE}
  | "for_edge" {FOR_EDGE}
  | "bfs" {BFS}
  | "dfs" {DFS}
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "bool" { BOOL }
  | "float" { FLOAT }
  | "int" { INT }
  | "string" { STRING }
  | "graph" { GRAPH }
  | "node" { NODE }
  | "wegraph" { WEGRAPH }
  | "digraph" { DIGRAPH }
  | "wedigraph" { WEDIGRAPH }
  | "map" { MAP }
(*  | "break" { BREAK }
  | "continue" { CONTINUE } *)
  | "return" { RETURN }
  | "void" { VOID }

  (* operators *)
  | '+'	{ PLUS }
  | '-'	{ MINUS }
  | '*'	{ TIMES }
  | '/'	{ DIVIDE }
  | '=' { ASSIGN }
  (* | "+=" { PLUSEQ }
     | "-=" { MINUSEQ }
     | "*=" { TIMESEQ }
     | "/=" { DIVEQ } *)
  | '%' { MOD }
  | "&&" { AND }
  | "||" { OR }
  (* | '&' { INTERSECTION }
     | '|' { UNION } *)
  | '!' { NOT }
  | "==" { EQ }
  | "!=" { NEQ }
  | ">=" { GEQ }
  | "<=" { LEQ }
  | '>' { GT }
  | '<' { LT }
  | "->" { RARROW }
  | "<-" { LARROW }
  | "<->" { DIARROW }
  | "--" { EDGE }

  (* literals and IDs *)
  | digit+ as lxm                               { INT_LIT(int_of_string lxm) }
  | decimal as lxm                              { FLOAT_LIT(float_of_string lxm) }
  | ("true" | "false") as lxm                   { BOOL_LIT(bool_of_string lxm) }
  | letter (letter | digit | '_')* as lxm       { ID(lxm) }
  | '\"' ([^'\"']* as lxm) '\"'                 { STRING_LIT(lxm) }
  | eof	{ EOF }
  | _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }


and comment = parse
    "~!" { token lexbuf }
  | _    { comment lexbuf }
