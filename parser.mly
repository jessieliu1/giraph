/* Authors:
Daniel Benett deb2174
Seth Benjamin sjb2190
Jennifer Bi jb3495
Jessie Liu jll2219 */

%{ open Ast
open Prshelper %}


%token LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE DOT COMMA SEMI COLON
%token PLUS MINUS TIMES DIVIDE ASSIGN NOT MOD
%token EQ NEQ LT LEQ GT GEQ AND OR
%token RETURN IF THEN ELSE FOR WHILE FOR_NODE FOR_EDGE BFS DFS BREAK CONTINUE
%token INT BOOL VOID FLOAT STRING NODE EDGE GRAPH WEGRAPH DIGRAPH WEDIGRAPH MAP
%token RARROW LARROW DIARROW
%token SINGLEQUOTE DOUBLEQUOTE
%token <int> INT_LIT
%token <float> FLOAT_LIT
%token <bool> BOOL_LIT
%token <string> STRING_LIT
%token <string> ID
%token EOF

%right SEMI

/*arithmetic ops*/
%right ASSIGN
%left PLUS MINUS
%left TIMES DIVIDE MOD
%left OR AND 
%right NOT NEG

%nonassoc EQ NEQ
%nonassoc GEQ LEQ GT LT
%nonassoc NOELSE
%nonassoc ELSE

%left DOT

%right LARROW RARROW DIARROW EDGE
%nonassoc COLON

%start program
%type <Ast.program> program
%%

program:
  decls EOF { $1 }

decls:
  /* nothing */     { [], [] } /* first list has vdecls, second has fdecls*/
| decls vdecl { ($2 :: fst $1), snd $1 }
| decls fdecl { fst $1, ($2 :: snd $1) }
  
fdecl: typ ID LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
        { { f_typ = $1; f_name = $2; f_formals = $4; f_body = List.rev $7 } }
  
typ:
    INT { Int }
  | FLOAT { Float }
  | BOOL { Bool }
  | VOID { Void }
  | STRING { String }
  | NODE LT typ GT { Node($3) }
  | GRAPH LT typ GT { Graph($3) }
  | DIGRAPH LT typ GT { Digraph($3) }
  | WEGRAPH LT typ GT { Wegraph($3) }
  | WEDIGRAPH LT typ GT { Wedigraph($3) }
  | MAP LT typ GT { Map($3) }

formals_opt: /* nothing */  { [] }
        | formal_list { List.rev $1 } 

formal_list: typ ID { [($1, $2)] }
        | formal_list COMMA typ ID { ($3,$4) :: $1 } 

vdecl: 
  typ ID SEMI { ($1, $2) }

stmt_list: 
  /* nothing */  { [] }
| stmt_list stmt { $2 :: $1 }

stmt:
  expr SEMI     { Expr $1 }
| typ ID SEMI { Vdecl($1, $2, Noexpr) }
| typ ID ASSIGN expr SEMI { Vdecl($1, $2, Assign($2,$4)) }
| RETURN SEMI   { Return Noexpr }
| RETURN expr SEMI { Return $2 }
| LBRACE stmt_list RBRACE { Block(List.rev $2) }
| IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
| IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
| FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt { For($3, $5, $7, $9) }
| FOR_NODE LPAREN ID COLON expr RPAREN stmt { For_Node($3, $5, $7) }
| FOR_EDGE LPAREN ID COLON expr RPAREN stmt { For_Edge($3, $5, $7) }
| BFS LPAREN ID COLON expr SEMI expr RPAREN stmt { Bfs($3, $5, $7, $9) }
| DFS LPAREN ID COLON expr SEMI expr RPAREN stmt { Dfs($3, $5, $7, $9) }
| WHILE LPAREN expr RPAREN stmt         { While($3, $5) }
| BREAK SEMI 	{Break}
| CONTINUE SEMI 	{Continue}

expr:
  INT_LIT           { Int_Lit($1) }
| BOOL_LIT          { Bool_Lit($1) }
| STRING_LIT        { String_Lit($1) }
| FLOAT_LIT         { Float_Lit($1) }
| ID                { Id($1) }
| expr PLUS expr { Binop($1, Add, $3)  } 
| expr MINUS expr { Binop($1, Sub, $3) }
| expr TIMES expr { Binop($1, Mult, $3) }
| expr DIVIDE expr { Binop($1, Div , $3) }
| expr MOD expr { Binop($1, Mod, $3) }
| expr EQ expr { Binop($1, Eq, $3) }
| expr NEQ expr { Binop($1, Neq, $3) }
| expr LEQ expr { Binop($1, Leq, $3) }
| expr GT expr { Binop($1, Greater, $3) }
| expr LT expr { Binop($1, Less, $3) }
| expr AND expr { Binop($1, And, $3) }
| expr OR expr { Binop($1, Or, $3) }
| MINUS expr %prec NEG { Unop(Neg, $2) }
| NOT expr              { Unop(Not, $2) }
| ID ASSIGN expr        { Assign($1, $3) }
| ID LPAREN actuals_opt RPAREN { Call($1, $3) }
| expr DOT ID LPAREN actuals_opt RPAREN { Method($1, $3, $5) }
| LPAREN expr RPAREN { $2 }
| LBRACK graph_expr_opt RBRACK { match $2 with (n, e, n_i, is_d, is_w) ->
                                   Graph_Lit(n, e, n_i, is_d, is_w) }

graph_expr_opt:
  /* nothing */ { [], [], [], false, false }
| single_node_expr { match $1 with (n, e, n_i) -> (List.rev n, List.rev e, List.rev n_i, false, false) }
| single_node_exprs_list { match $1 with (n, e, n_i) -> (List.rev n, List.rev e, List.rev n_i, false, false) }
| ugraph_exprs_list { match $1 with (n, e, n_i) -> (List.rev n, List.rev e, List.rev n_i, false, false) }
| digraph_exprs_list { match $1 with (n, e, n_i) -> (List.rev n, List.rev e, List.rev n_i, true, false) }
| wegraph_exprs_list { match $1 with (n, e, n_i) -> (List.rev n, List.rev e, List.rev n_i, false, true) }
| wedigraph_exprs_list { match $1 with (n, e, n_i) -> (List.rev n, List.rev e, List.rev n_i, true, true) }

single_node_exprs_list:
  single_node_expr SEMI single_node_expr { merge_graph_exprs $1 $3 }
| single_node_expr SEMI single_node_exprs_list { merge_graph_exprs $1 $3 }

ugraph_exprs_list:
  ugraph_expr { $1 }
| ugraph_exprs_list SEMI ugraph_expr { merge_graph_exprs $1 $3 }
| ugraph_exprs_list SEMI single_node_expr { merge_graph_exprs $1 $3 }
| single_node_expr SEMI ugraph_exprs_list { merge_graph_exprs $1 $3 }

digraph_exprs_list:
  digraph_expr { $1 }
| digraph_exprs_list SEMI digraph_expr { merge_graph_exprs $1 $3 }
| digraph_exprs_list SEMI single_node_expr { merge_graph_exprs $1 $3 }
| single_node_expr SEMI digraph_exprs_list { merge_graph_exprs $1 $3 }

ugraph_expr:
  single_node_expr EDGE ID { update_graph $1 $3 (Int_Lit(0)) }
| single_node_expr EDGE ID COLON expr { update_graph_e $1 $3 $5 (Int_Lit(0)) }
| ugraph_expr EDGE ID { update_graph $1 $3 (Int_Lit(0)) }
| ugraph_expr EDGE ID COLON expr { update_graph_e $1 $3 $5 (Int_Lit(0)) }

digraph_expr:
  single_node_expr RARROW ID { update_digraph $1 $3 (Int_Lit(0)) 0 }
| single_node_expr LARROW ID { update_digraph $1 $3 (Int_Lit(0)) 1 }
| single_node_expr DIARROW ID { update_digraph_b $1 $3 (Int_Lit(0)) }
| single_node_expr RARROW ID COLON expr { update_digraph_e $1 $3 $5 (Int_Lit(0)) 0 }
| single_node_expr LARROW ID COLON expr { update_digraph_e $1 $3 $5 (Int_Lit(0)) 1 }
| single_node_expr DIARROW ID COLON expr { update_digraph_be $1 $3 $5 (Int_Lit(0)) }
| digraph_expr RARROW ID { update_digraph $1 $3 (Int_Lit(0)) 0 }
| digraph_expr LARROW ID { update_digraph $1 $3 (Int_Lit(0)) 1 }
| digraph_expr DIARROW ID { update_digraph_b $1 $3 (Int_Lit(0)) }
| digraph_expr RARROW ID COLON expr { update_digraph_e $1 $3 $5 (Int_Lit(0)) 0 }
| digraph_expr LARROW ID COLON expr { update_digraph_e $1 $3 $5 (Int_Lit(0)) 1 }
| digraph_expr DIARROW ID COLON expr { update_digraph_be $1 $3 $5 (Int_Lit(0)) }

/* weighted graphs */
wegraph_exprs_list:
  wegraph_expr { $1 }
| wegraph_exprs_list SEMI wegraph_expr { merge_graph_exprs $1 $3 }
| wegraph_exprs_list SEMI single_node_expr { merge_graph_exprs $1 $3 }
| single_node_expr SEMI wegraph_exprs_list { merge_graph_exprs $1 $3 }

wegraph_expr:
  single_node_expr MINUS LBRACE expr RBRACE MINUS ID %prec EDGE { update_graph $1 $7 $4 }
| single_node_expr MINUS LBRACE expr RBRACE MINUS ID COLON expr %prec EDGE { update_graph_e $1 $7 $9 $4 }
| wegraph_expr MINUS LBRACE expr RBRACE MINUS ID %prec EDGE { update_graph $1 $7 $4 }
| wegraph_expr MINUS LBRACE expr RBRACE MINUS ID COLON expr %prec EDGE { update_graph_e $1 $7 $9 $4 }

wedigraph_exprs_list:
  wedigraph_expr { $1 }
| wedigraph_exprs_list SEMI wedigraph_expr { merge_graph_exprs $1 $3 }
| wedigraph_exprs_list SEMI single_node_expr { merge_graph_exprs $1 $3 }
| single_node_expr SEMI wedigraph_exprs_list { merge_graph_exprs $1 $3 }

wedigraph_expr:
  single_node_expr MINUS LBRACE expr RBRACE RARROW ID %prec EDGE { update_digraph $1 $7 $4 0 }
| single_node_expr LARROW LBRACE expr RBRACE MINUS ID %prec EDGE { update_digraph $1 $7 $4 1 }
| single_node_expr LARROW LBRACE expr RBRACE RARROW ID %prec EDGE { update_digraph_b $1 $7 $4 }
| single_node_expr MINUS LBRACE expr RBRACE RARROW ID COLON expr %prec EDGE { update_digraph_e $1 $7 $9 $4 0 }
| single_node_expr LARROW LBRACE expr RBRACE MINUS ID COLON expr %prec EDGE { update_digraph_e $1 $7 $9 $4 1 }
| single_node_expr LARROW LBRACE expr RBRACE RARROW ID COLON expr %prec EDGE { update_digraph_be $1 $7 $9 $4 }
| wedigraph_expr MINUS LBRACE expr RBRACE RARROW ID %prec EDGE { update_digraph $1 $7 $4 0 }
| wedigraph_expr LARROW LBRACE expr RBRACE MINUS ID %prec EDGE { update_digraph $1 $7 $4 1 }
| wedigraph_expr LARROW LBRACE expr RBRACE RARROW ID %prec EDGE { update_digraph_b $1 $7 $4 }
| wedigraph_expr MINUS LBRACE expr RBRACE RARROW ID COLON expr %prec EDGE { update_digraph_e $1 $7 $9 $4 0 }
| wedigraph_expr LARROW LBRACE expr RBRACE MINUS ID COLON expr %prec EDGE { update_digraph_e $1 $7 $9 $4 1 }
| wedigraph_expr LARROW LBRACE expr RBRACE RARROW ID COLON expr %prec EDGE { update_digraph_be $1 $7 $9 $4 }



single_node_expr:
  ID { [$1], [], [] }
| ID COLON expr { [$1], [], [($1, $3)] }


expr_opt: 
    /* nothing */ { Noexpr }
  | expr { $1 }


actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }


