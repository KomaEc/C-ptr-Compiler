# C-ptr-Compiler
A compiler for a C-like imperative programming language, supporting dataflow analysis and pointer analysis. (Under development)

# To Use (Currently)
```bash
ocamlbuild -use-menhir main.native
./main.native test.cptr
```
with a file "test.cptr" written in C-ptr .
Then a translated version will be printed to the stdout channel.
