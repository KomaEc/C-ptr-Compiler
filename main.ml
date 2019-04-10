open Ast 
open Ast.Util
open Parser 
open Printf 
open Lexing

open Mimple
open Lexer
open Printer
open Sement
open Support.Error

let print_position outx lexbuf = 
  let pos = lexbuf.lex_curr_p in 
  fprintf outx "%s : lines %d : offset %d\n"
  pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)
;;

let parse_with_error lexbuf = 
  try Parser.prog Lexer.read lexbuf with 
  | Lexer.Syntax_Error msg -> 
    fprintf stderr "%a: %s" print_position lexbuf msg;
    exit(0)
  | Parser.Error -> 
    fprintf stderr "%a" print_position lexbuf;
    exit(0)
;;

let check_with_error s = 
  try check s with 
  | Duplicated_Definition i -> 
    fprintf stderr "%a %s" printInfo i "Duplicated definition\n"
  | Lack_Definition i -> 
    fprintf stderr "%a %s" printInfo i "LackDefinition\n"
  | No_Initialization i -> 
    fprintf stderr "%a %s" printInfo i "No Initialization\n"
  | Ill_Typed i -> 
    fprintf stderr "%a %s" printInfo i "Ill Typed\n"
  | Arity_Mismatched i -> 
    fprintf stderr "%a %s" printInfo i "Arity Mismatched\n"
  | Fundec_Mismatched i -> 
    fprintf stderr "%a %s" printInfo i "Function Declaration Mismatched\n"
  | Not_Function i -> 
    fprintf stderr "%a %s" printInfo i "Identifier Not a Function\n"
  | No_Fieldname i -> 
    fprintf stderr "%a %s" printInfo i "No Such Field\n"
  | Not_Struct i -> 
    fprintf stderr "%a %s" printInfo i "Identifier Not a Struct\n"
  | Alloc_Non_Struct i -> 
    fprintf stderr "%a %s" printInfo i "Allocation of Array and Struct Only\n"
  | Type_Var_Misuse i -> 
    fprintf stderr "%a %s" printInfo i "Variable Expected. Not Type\n"
  | Null_Reference i -> 
    fprintf stderr "%a %s" printInfo i "Referencing a Nullptr\n"
let print_helper = "What do you need?\n"

let () = 
  match Array.length Sys.argv with 
  | 1 -> fprintf stderr "%s" print_helper; exit(0)  
  | 2 -> (let fname = Sys.argv.(1) in 
          let inx = open_in fname in 
          let lexbuf = from_channel inx in 
          lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = fname};
          (*(match parse_with_error lexbuf with 
          | Some s -> print_stmt "" (simplify s) 
          | None -> ());*)
          (*print_stmt "" (Seq(parse_with_error lexbuf, Support.Error.dummyinfo));*)
          let s = simplify (parse_with_error lexbuf) in 
          print_stmt "" s;
          print_newline();
          check_with_error s;
          close_in inx
          )
  | _ -> fprintf stderr "Too many arguments! Expected 1\n"; exit(0)


  
