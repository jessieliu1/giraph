%{ open Ast %}


%token LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE COMMA SEMI
%token PLUS MINUS TIMES DIVIDE ASSIGN NOT MOD
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR
%token RETURN IF THEN ELSE FOR WHILE FOR_NODE FOR_EDGE BFS DFS BREAK CONTINUE
%token INT BOOL VOID FLOAT CHAR STRING NODE EDGE GRAPH WEGRAPH DIGRAPH WEDIGRAPH FUNCTION
%token COLON RARROW LARROW DIARROW 
%token SINGLEQUOTE DOUBLEQUOTE
%token <int> LITERAL
%token <string> ID
%token EOF

/*arithmetic ops*/
%left PLUS MINUS
%left TIMES DIVIDE MOD
%right NOT NEG
%right ASSIGN EXP
%left OR AND 
%nonassoc EQ NEQ
%nonassoc  GEQ LEQ GT 
%nonassoc  NOELSE
%nonassoc ELSE

%left RARROW DIARROW EDGE
%right LARROW
%nonassoc COLON

%start program
%type <Ast.program> program
%%
 
program:
  decls EOF { $1 }

decls:
  /* nothing */     { [], [] }
  | decls vdecl { ($2 :: fst $1), snd $1 }
  | decls fdecl { fst $1, ($2 :: snd $1) }

/** FUNCTIONS **/
functions:
    /* nothing */       { [] }
  | fn_list           { List.rev $1 }

fn_list:
    fdecl               { [$1] }  /* returns a list */
   | fn_list fdecl      { $2 :: $1 }
  
fdecl: typ ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
        { { f_typ = $1; f_name = $2; f_formals = $4;
                f_locals = List.rev $7; f_body = List.rev $8 } } 
  
typ:
    INT { Int }
  | FLOAT { Float }
  | BOOL { Bool }
  | VOID { Void }
  | CHAR { Char }
  | STRING { Str }
  | NODE { Node }
  | GRAPH { Graph }

formals_opt: /* nothing */  { [] }
        | formal_list { List.rev $1 } 

formal_list: typ ID { [($1, $2)] }
| formal_list COMMA typ ID { ($3,$4) :: $1 } 

vdecl_list: 
/* nothing */ { [] }
| vdecl_list vdecl { $2 :: $1 }  

vdecl: 
typ ID SEMI { ($1, $2, 0) }
| typ ID ASSIGN expr SEMI { ($1, $2, $4) }

stmt_list: { [] }
| stmt_list stmt { $2 :: $1 }

stmt:
  expr SEMI     { Expr $1 }
| RETURN SEMI   { Return Noexpr }
| RETURN expr SEMI { Return $2 }
| LBRACE stmt_list RBRACE { Block(List.rev $2) }
| IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
| IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
| FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt { For($3, $5, $7, $9) }
| FOR_NODE LPAREN expr COLON expr RPAREN stmt { For_Node($3, $5, $7) }
| FOR_EDGE LPAREN expr COLON expr RPAREN stmt { For_Edge($3, $5, $7) }
| BFS LPAREN expr COLON expr SEMI expr RPAREN stmt { Bfs($3, $5, $7, $9) }
| DFS LPAREN expr COLON expr SEMI expr RPAREN stmt { Dfs($3, $5, $7, $9) }
| WHILE LPAREN expr RPAREN stmt         { While($3, $5) }
| BREAK SEMI 	{Break}
| CONTINUE SEMI 	{Continue}

expr:
LITERAL  { Literal($1) } 
| TRUE          { Bool_Lit(true) }
| FALSE         { Bool_Lit(false) }
| ID            { Id($1) }
| expr PLUS expr { Binop($1, Add, $3)  } 
| expr MINUS expr { Binop($1, Sub, $3) }
| expr TIMES expr { Binop($1, Mult, $3) }
| expr DIVIDE expr { Binop($1, Div , $3) }
| expr MOD expr { Binop($1, Mod, $3) }
| expr EQ expr { Binop($1, Eq, $3) }
| expr NEQ expr { Binop($1, Neq,$3) }
| MINUS expr %prec NEG { Unop(Neg, $2) }
| NOT expr              { Unop(Not, $2) }
| ID ASSIGN expr { Assign($1, $3) }
| ID LPAREN actuals_opt RPAREN { Call($1, $3) }
| LPAREN expr RPAREN { $2 }

expr_opt: 
/* nothing */ { Noexpr }
| expr { $1 }


actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }


