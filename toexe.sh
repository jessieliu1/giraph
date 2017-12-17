# Path to the LLVM interpreter
#LLI="lli"
LLI="/usr/local/opt/llvm/bin/lli"

# Path to the LLVM compiler
#LLC="llc"
LLC="/usr/local/opt/llvm/bin/llc"

# Path to giraph.native (must be made)
GIRAPH="./giraph.native"

# Path to the C compiler
CC="cc"
GIR=".gir"
BASENAME="${1%$GIR}"

if [ "$#" -ne 1 ]; then
    echo "usage: ./toexe filename.gir"
	exit
fi

"$GIRAPH" "$1" > "$BASENAME.ll" &&
"$LLC" "$BASENAME.ll" > "$BASENAME.s" &&
"$CC" "-o" "$BASENAME.exe" "$BASENAME.s" "graph.o"
