(* Ocamllex scanner for GOBLAN *)

{ open Parser }

let exp = ('e'|'E')('+'|'-')?['0'-'9']+

rule token = parse
  [' ' '\t' '\r' '\n']                 { token lexbuf }     (* Whitespace *)
| "/*"                                 { comment lexbuf }   (* Comments *)
| "//"                                 { slcomment lexbuf } (* Comments *)
| '#'                                  { slcomment lexbuf } (* Comments *)
| '"'      			                       { read_string (Buffer.create 16) lexbuf }
| '('                                  { LPAREN }
| ')'                                  { RPAREN }
| '{'                                  { LBRACE }
| '}'                                  { RBRACE }
| "[|"                                 { LLIST }
| "|]"                                 { RLIST }
| '['                                  { LBRACKET }
| ']'                                  { RBRACKET }
| ';'                                  { SEMI }
| ','                                  { COMMA }
| '.'                                  { PERIOD }
| '+'                                  { PLUS }
| '-'                                  { MINUS }
| '*'                                  { TIMES }
| '/'                                  { DIVIDE }
| '%'                                  { MODULO }
| '='                                  { ASSIGN }
| "+="                                 { LSTADD }
| "-="                                 { LSTRMV }
| "=="                                 { EQ }
| "!="                                 { NEQ }
| '<'                                  { LT }
| "<="                                 { LEQ }
| ">"                                  { GT }
| ">="                                 { GEQ }
| "&&"                                 { AND }
| "||"                                 { OR }
| "!"                                  { NOT }
| "->"                                 { ARROW }
| "if"                                 { IF }
| "else"                               { ELSE }
| "for"                                { FOR }
| "in"                                 { IN }
| "while"                              { WHILE }
| "return"                             { RETURN }
| "bool"                               { BOOL }
| "int"                                { INT }
| "float"                              { FLOAT }
| "string"                             { STRING }
| "list"                               { LIST }
| "new"                                { NEW }
| "void"                               { VOID }
| "graph"                              { GRAPH }
| "true"                               { TRUE }
| "false"                              { FALSE }
| "data"                               { DATA }
| "edge"                               { EDGE }
| "pack"                               { PACK }
| "do"                                 { DO }
| "catch"                              { CATCH }
| "self"                               { SELF }
| "prnt"                               { PARENT }
| "chld"                               { CHILD }
| "prnt_chld"                          { PRNTCHLD }
| "chld_prnt"                          { CHLDPRNT }
| "msg"                                { MESSAGE }
| "pass"                               { PASS }
| "run"                                { RUN }
| "null"                               { NULL }
| "print"                              { PRINT }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm
                                       { ID(lxm) }
| ('.'['0'-'9']+ exp?|['0'-'9']+('.'['0'-'9']* exp? | exp)) as lxm
                                       { FLT_LIT(float_of_string lxm) }
| ['0'-'9']+ as lxm                    { INT_LIT(int_of_string lxm) }
| "node:"['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm
                                       { NODE_TYP(lxm) }
| "tuple:"['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm
                                       { TUPLE_TYP(lxm) }
| eof                                  { EOF }
| _ as char                            { raise (Failure("illegal character " ^
                                         Char.escaped char))
                                       }

and read_string buf = parse
  | '"'       { STR_LIT(Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | '\\' '"'  { Buffer.add_char buf '\"'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (Failure("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (Failure("String is not terminated")) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }

and slcomment = parse
  '\n' { token lexbuf }
| _    { slcomment lexbuf }

