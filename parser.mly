%{ open Ast %}


%token LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE DOT COMMA SEMI COLON
%token PLUS MINUS TIMES DIVIDE ASSIGN NOT MOD
%token EQ NEQ LT LEQ GT GEQ AND OR
%token RETURN IF THEN ELSE FOR WHILE FOR_NODE FOR_EDGE BFS DFS BREAK CONTINUE
%token INT BOOL VOID FLOAT STRING NODE EDGE GRAPH WEGRAPH DIGRAPH WEDIGRAPH
%token RARROW LARROW DIARROW
%token SINGLEQUOTE DOUBLEQUOTE
%token <int> INT_LIT
%token <float> FLOAT_LIT
%token <bool> BOOL_LIT
%token <string> STRING_LIT
%token <string> ID
%token EOF

/*arithmetic ops*/
%right ASSIGN
%left PLUS MINUS
%left TIMES DIVIDE MOD
%left OR AND 
%right NOT NEG

%nonassoc EQ NEQ
%nonassoc  GEQ LEQ GT LT
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
  | NODE { Node }
  | GRAPH { Graph }

formals_opt: /* nothing */  { [] }
        | formal_list { List.rev $1 } 

formal_list: typ ID { [($1, $2)] }
| formal_list COMMA typ ID { ($3,$4) :: $1 } 

vdecl: 
typ ID SEMI { ($1, $2) }

stmt_list: { [] }
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
| expr NEQ expr { Binop($1, Neq,$3) }
| expr LEQ expr { Binop($1, Leq,$3) }
| expr GT expr { Binop($1, Greater,$3) }
| expr LT expr { Binop($1, Less,$3) }
| MINUS expr %prec NEG { Unop(Neg, $2) }
| NOT expr              { Unop(Not, $2) }
| ID ASSIGN expr { Assign($1, $3) }
| ID LPAREN actuals_opt RPAREN { Call($1, $3) }
| ID DOT ID LPAREN actuals_opt RPAREN { Method($1, $3, $5) }
| LPAREN expr RPAREN { $2 }
| LBRACK graph_expr_opt RBRACK { match $2 with (n, e, n_i) -> Graph_Lit(n, e, n_i) }

graph_expr_opt:
  /* nothing */ { [], [], [] }
| graph_exprs_list { match $1 with (n, e, n_i) -> (List.rev n, List.rev e, List.rev n_i) }

graph_exprs_list:
  graph_expr { $1 }
| graph_exprs_list SEMI graph_expr { match $1 with (n1, e1, n_i1) ->
                                       match $3 with (n2, e2, n_i2) ->
                                         (* essentially, take the union of node/edge/node_init lists. *)
                                         let add_if_missing list elem = if (List.mem elem list) then
                                                                          list
                                                                        else
                                                                          elem :: list
                                         in (List.fold_left add_if_missing n1 (List.rev n2),
                                             List.fold_left add_if_missing e1 (List.rev e2),
                                             List.fold_left add_if_missing n_i1 (List.rev n_i2))
                                   }

graph_expr:
  ID { [$1], [], [] }
| ID COLON expr { [$1], [], [($1, $3)] }
| graph_expr EDGE ID { match $1 with
                         (nodes, edges, nodes_init) ->
                         let nodes = if (List.mem $3 nodes) then (* if next node is already in this graph, *)
                                       ($3 :: List.filter (fun n -> n <> $3) nodes) (* move to front of nodelist so edges work *)
                                     else
                                       $3 :: nodes (* otherwise just add to front *)
                         and edges = let new_edge = ((List.hd nodes), $3) and
                                         new_edge_rev = ($3, (List.hd nodes)) in
                                     (* only add this edge if it's not already there *)
                                     if (List.mem new_edge edges || List.mem new_edge_rev edges) then
                                       edges
                                     else
                                       new_edge :: edges
                         in (nodes, edges, nodes_init)
                     }
| graph_expr EDGE ID COLON expr { match $1 with
                                    (nodes, edges, nodes_init) -> (* handle nodes and edges w/ same logic as above *)
                                    let nodes = if (List.mem $3 nodes) then
                                                  ($3 :: List.filter (fun n -> n <> $3) nodes)
                                                else
                                                  $3 :: nodes
                                    and edges = let new_edge = ((List.hd nodes), $3) and
                                                    new_edge_rev = ($3, (List.hd nodes)) in
                                                if (List.mem new_edge edges || List.mem new_edge_rev edges) then
                                                  edges
                                                else
                                                  new_edge :: edges
                                    and nodes_init = ($3, $5) :: nodes_init (* add node name/data pair to nodes_init *)
                                    in (nodes, edges, nodes_init)
                                }
/* logic for making the edgelist will be way more complicated for digraphs, not sure yet how */
expr_opt: 
/* nothing */ { Noexpr }
| expr { $1 }


actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }


