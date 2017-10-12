%{ open Ast %}


%token LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE COMMA
%token PLUS MINUS TIMES DIVIDE ASSIGN NOT MOD
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR
%token IF THEN ELSE FOR WHILE
%token INT BOOL VOID FLOAT CHAR STRING
%token <int> LITERAL
%token <string> ID
%token EOF

(*arithmetic ops*)
%left PLUS MINUS
%left TIMES DIVIDE MOD
%right NOT NEG
%right ASSIGN EXP
%left OR AND 
%nonassoc EQ NEQ
%nonassoc  GEQ LEQ GT LT

(*%left RARROW DIARROW EDGE
%right LARROW
%nonassoc COLON *) 

%start program
%type <Ast.program> program
%%
 
program:
        functions EOF { $1 }

(** FUNCTIONS **)
functions:
    /* nothing */       { [] }
  | fn_list           { List.rev $1 }

fn_list:
    fdecl               { [$1] }  (* returns a list *)
   | fn_list fdecl      { $2 :: $1 }
  
fdecl: typ ID LPAREN formals_opt RPAREN LBRACE vdecl_list RBRACE
        { { f_typ = $1; f_name = $2; f_formals = $4;
                f_locals = List.rev $7; f_body = List.rev $7 } } 
  
typ:
    INT { Int }
  | FLOAT { Float }
  | BOOL { Bool }
  | VOID { Void }
  | CHAR { Char }
  | STRING { Str }

formals_opt: /* nothing */  { [] }
(*        | formal_list { List.rev $1 } *)

formal_list: typ ID { [($1, $2)] }
(*| formal_list COMMA typ ID { ($3,$4) :: $1 } *)

vdecl_list: /* nothing */ { [] }

expr:
LITERAL  { Literal($1) } 
| TRUE          { Bool_Lit(true) }
| FALSE         { Bool_Lit(false) }
| ID            { Id($1) }
| ID ASSIGN expr { Assign($1, $3) }
| expr PLUS expr { Binop($1, Add, $3)  } 
| expr MINUS expr { Binop($1, Sub, $3) }
| expr TIMES expr { Binop($1, Mult, $3) }
| expr DIVIDE expr { Binop($1, Div , $3) }
| expr MOD expr { Binop($1, Mod, $3) }
| expr EQ expr { Binop($1, Eq, $3) }
| expr NEQ expr { Binop($1, Neq,$3) }
| LPAREN expr RPAREN { $2 }
| MINUS expr %prec NEG { Unop(Neg, $2) }





