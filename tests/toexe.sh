# to run: "./toexe fnwithoutext" 

# Path to the LLVM interpreter
#LLI="lli"
LLI="/usr/local/opt/llvm/bin/lli"

# Path to the LLVM compiler
#LLC="llc"
LLC="/usr/local/opt/llvm/bin/llc"

# Path to giraph.native (must be made)
GIRAPH="../giraph.native"

# Path to the C compiler
CC="cc"

"$GIRAPH" "$1.gir" > "$1.ll" &&
"$LLC" "$1.ll" > "$1.s" &&
"$CC" "-o" "$1.exe" "$1.s" &&
"./$1.exe" > "$1.out"