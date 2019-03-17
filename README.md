# C-ptr-Compiler
A compiler for a C-like imperative programming language, supporting dataflow analysis and pointer analysis. (Under development)

# To Use (Currently)
ocamlbuild -use-menhir main.native
with a file "test.cptr" written in C-ptr, 
./main.native test.cptr
Then a translated version will be printed to the stdout channel.
