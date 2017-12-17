

.PHONY : all
all : clean giraph.native graph.o

.PHONY : giraph.native
giraph.native :
	ocamlbuild -use-ocamlfind -pkgs llvm,llvm.analysis -cflags -w,+a-4 \
		giraph.native

.PHONY : clean
clean :
	rm -f *.o
