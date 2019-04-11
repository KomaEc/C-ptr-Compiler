# Cm-Compiler
## Intro
A compiler for a C-like imperative programming language (its semantics are closer to Java, though. For example, there's no pointer. The types are divided into two groups, primitive type and object type. Values of object types are passed by reference, etc.), supporting int array and struct. (Under development) Goals: midend that supports data flow analysis framework, and Andersen's pointer analysis.

## Currently
With an input C file, the compiler transforms the source file into an AST, and performs sementic checks (including variable inialization, type checking). After the semantics checking, the compiler will transform the original program into an intermediate representation Mimple. Mimple is an simplified and untyped version of Jimple. The result will be printed to the standard output channel.

## To Use
```bash
ocamlbuild -use-menhir main.native
./main.native ./test/basic.cm
```
File "basic.cptr" is written in Cm. A translated version will be printed to the stdout channel.
