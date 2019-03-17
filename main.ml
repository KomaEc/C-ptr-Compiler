open Ast 
open Parser 
open Printf 
open Lexing
open Lexer
open Printer

let print_position outx lexbuf = 
  let pos = lexbuf.lex_curr_p in 
  fprintf outx "%s : lines %d : offset %d"
  pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)
;;

let parse_with_error lexbuf = 
  try Parser.prog Lexer.read lexbuf with 
  | Lexer.Syntax_Error msg -> 
    fprintf stderr "%a: %s" print_position lexbuf msg;
    None
  | Parser.Error -> 
    fprintf stderr "%a" print_position lexbuf;
    exit(0)
;;

let print_helper = "What do you need?\n"

let () = 
  match Array.length Sys.argv with 
  | 1 -> fprintf stderr "%s" print_helper; exit(0)  
  | 2 -> (let fname = Sys.argv.(1) in 
          let inx = open_in fname in 
          let lexbuf = from_channel inx in 
          lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = fname};
          (match parse_with_error lexbuf with 
          | Some s -> print_stmt "" (simplify s) 
          | None -> ());
          close_in inx
          )
  | _ -> fprintf stderr "Too many arguments! Expected 1\n"; exit(0)


  