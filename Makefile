program:
	ocamllex scanner.mll # generates lexer.ml
	ocamlyacc parser.mly # generates parser.ml and parser.mli
	ocamlc -c ast.ml
	ocamlc -c parser.mli
	ocamlc -c scanner.ml
	ocamlc -c parser.ml
	#ocamlc -c giraph.ml
