(* Ocammllex scanner for giraph *)
{ 	
	open Parser 
}

(* Definitions *)
let digit = ['0'-'9']
let letter = ['A'-'Z' 'a'-'z']

(* Rules *)
rule token = parse 
    [ ' ' '\t' '\r' '\n'] { token lexbuf } (* to ignore whitespace *)
    | "/*" { comment lexbuf }
    | "//" { singleline lexbuf }
    | ',' { COMMA }
    | ';' { SEMI }
    | '\'' { SINGLEQUOTE }
    | '\"' { DOUBLEQUOTE }
    | digit+ as lit		{ LITERAL(int_of_string lit) }
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
(*  | "break" { BREAK }
    | "continue" { CONTINUE } *)
    | "function" { FUNCTION }
    | "return" { RETURN }
    | "void" { VOID }


    (* operators *)
    | '+'	{ PLUS }
    | '-'	{ MINUS }
    | '*'	{ TIMES }
    | '/'	{ DIVIDE }
    | '=' { ASSIGN }
(*  | "+=" { PLUSEQ }
    | "-=" { MINUSEQ }
    | "*=" { TIMESEQ }
    | "/=" { DIVEQ } *)
    | '%' { MOD }
(*  | "&&" { AND }
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

{
	(* Trailer; optional *)
}