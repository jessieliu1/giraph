(* Ocammllex scanner for giraph *)
{ open Parser }

(* Rules *)
rule token = parse 
    [ ' ' '\t' '\r' '\n'] { token lexbuf } (* to ignore whitespace *)
  | "/*" { comment lexbuf }
  | "//" { singleline lexbuf }
  | ',' { COMMA }
  | ';' { SEMI }
  | '\'' { SINGLEQUOTE }
  | '\"' { DOUBLEQUOTE }
  | ['0'-'9']+ as lxm { LITERAL(int_of_string lxm) }
  | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
  | eof	{ EOF }

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
  | "true" { TRUE }
  | "false" { FALSE }
  | "bool" { BOOL }
  | "float" { FLOAT }
  | "int" { INT }
  | "string" { STRING }
  | "char" { CHAR }
  | "graph" { GRAPH }
  | "edge" { EDGE }
  | "node" { NODE }
  | "wegraph" { WEGRAPH }
  | "digraph" { DIGRAPH }
  | "wedigraph" { WEDIGRAPH }
  | "break" { BREAK }
  | "continue" { CONTINUE }
  | "function" { FUNCTION }
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
  (* | "&&" { AND }
     | "||" { OR }
     | '&' { INTERSECTION }
     | '|' { UNION } *)
  | '!' { NOT }
  | "==" { EQ }
  | ">=" { GEQ }
  | "<=" { LEQ }
  | '>' { GT }
  | '<' { LT }
  | "->" { RARROW }
  | "<-" { LARROW }
  | "<->" { DIARROW }
  | "--" { EDGE }
  | ':' { COLON }


and comment = parse
    "*/" { token lexbuf }
  | _    { comment lexbuf }

and singleline = parse
    '\n' { token lexbuf }
  | _ { singleline lexbuf }
