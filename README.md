# Cm-Compiler
## Intro
A compiler for a C-like imperative programming language, supporting int array and struct. (Under development) Goals: midend that supports data flow analysis framework, and Andersen's pointer analysis.

## Currently
With an input C file, the compiler transforms the source file into an AST, and performs sementic checks (including variable inialization, type checking), and then prints the output.

## To Use
```bash
ocamlbuild -use-menhir main.native
./main.native ./test/basic.cptr
```
File "basic.cptr" is written in Cm. A translated version will be printed to the stdout channel.
