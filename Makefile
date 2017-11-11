

.PHONY : all
all : giraph.native

.PHONY : giraph.native
giraph.native :
	ocamlbuild -use-ocamlfind -pkgs llvm,llvm.analysis -cflags -w,+a-4 \
		giraph.native

