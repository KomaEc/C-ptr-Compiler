# Cm-Compiler
## Intro
A compiler for a C-like imperative programming language (its semantics are closer to Java, though. For example, there's no pointer. The types are divided into two groups, primitive type and object type. Values of object types are passed by reference, etc.), supporting (currently) one-dimentional array and struct. (Under development) Goals: a middle end that supports data flow analysis framework, and Andersen's pointer analysis.

## Currently
With an input C file, the compiler transforms the source file into an AST, and performs sementic checks (including variable inialization, type checking). After the semantics checking, the compiler will transform the original program into an intermediate representation Mimple. The result will be printed to the standard output channel.

## Mimple
Mimple is an simplified version of Jimple (SOOT Java optimization framework). In this toy language, global vars are treated as static field reference.

## To Use
```bash
dune build ./bin/main.exe
dune exec ./bin/main.exe ./test/basic.cm
```
File "basic.cm" is written in Cm. A translated version will be printed to the stdout channel.
