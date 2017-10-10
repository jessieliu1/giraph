{ 	(* Header *)
	open Parser 
}

(* Definitions: optional *)

(* Rules: mandatory *)
rule token = 
	parse [ '' '\t' '\r' '\n']	{ token lexbuf }
		| '+'					{ PLUS }
		| '-'					{ MINUS }
		| '*'					{ TIMES }
		| '/'					{ DIVIDE }
		| ['0'-'9']+ as lit		{ LITERAL(int_of_string lit) }
		| eof					{ EOF }
and
and

{
	(* Trailer; optional *)
}