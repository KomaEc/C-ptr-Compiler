# C-ptr-Compiler
A compiler for a C-like imperative programming language, supporting dataflow analysis and pointer analysis. (Under development)

# Currently
With an input C file, the compiler transforms the source file into an AST, and performs sementic checks (including variable inialization, type checking), and then prints the output.

# To Use
```bash
ocamlbuild -use-menhir main.native
./main.native ./test/basic.cptr
```
File "basic.cptr" is written in C-ptr. A translated version will be printed to the stdout channel.
