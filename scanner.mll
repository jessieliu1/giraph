{ 	(* Header *)
	open Parser 
}

(* Definitions: optional *)
let digits = ['0'-'9']
let letters = ['A'-'Z' 'a'-'z']

(* Rules: mandatory *)
rule token = 
parse [ ' ' '\t' '\r' '\n']	{ token lexbuf }
| "/*" { comment lexbuf }
| "//" { singleline lexbuf }
| ',' { COMMA }
| ';' { SEMI }
| '\'' { SINGLEQUOTE }
| '\"' { DOUBLEQUOTE }
| ['0'-'9']+ as lit		{ LITERAL(int_of_string lit) }
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
| "graph" { TYPE_GRAPH }
| "wegraph" { TYPE_WEGRAPH }
| "digraph" { TYPE_DIGRAPH }
| "wedigraph" { TYPE_WEDIGRAPH }
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
| "+=" { PLUSEQ }
| "-=" { MINUSEQ }
| "*=" { TIMESEQ }
| "/=" { DIVEQ }
| '%' { MOD }
| "&&" { AND }
| "||" { OR }
| '&' { INTERSECTION }
| '|' { UNION }
| '!' { BANG }
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