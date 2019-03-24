# C-ptr-Compiler
A compiler for a C-like imperative programming language, supporting dataflow analysis and pointer analysis. (Under development)

# Current
With an input C file, the compiler transforms the source file into an AST, and performs sementic checks (including variable inialization, type checking), and then prints the output.

# To Use (Currently)
```bash
ocamlbuild -use-menhir main.native
./main.native ./basic.cptr
```
with a file "test.cptr" written in C-ptr .
Then a translated version will be printed to the stdout channel.
