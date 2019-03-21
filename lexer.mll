{
  open Lexing
  open Parser
  open Symbol
  open Support
  open Support.Error

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = lexbuf.lex_curr_pos;
                 pos_lnum = pos.pos_lnum + 1;
      }

  exception Syntax_Error of string

  let reserved_word = [
    (* keywords *)
    "true", (fun i -> TRUE i);
    "false", (fun i -> FALSE i);
    "for", (fun i -> FOR i);
    "if", (fun i -> IF i);
    "int", (fun i -> INT i);
    "bool", (fun i -> BOOL i);
    "return", (fun i -> RETURN i);
    "else", (fun i -> ELSE i);

    (* symbols *)
    ":", (fun i -> COLON i);
    ",", (fun i -> COMMA i);
    ";", (fun i -> SEMICOLON i);
    "(", (fun i -> LPAREN i);
    ")", (fun i -> RPAREN i);
    "{", (fun i -> LBRACE i);
    "}", (fun i -> RBRACE i);
    "+", (fun i -> PLUS i);
    "-", (fun i -> MINUS i);
    "*", (fun i -> TIMES i);
    "/", (fun i -> DIV i);
    "&&", (fun i -> AND i);
    "||", (fun i -> OR i);
    "!", (fun i -> NOT i);
    "=", (fun i -> ASSIGN i);
    "!=", (fun i -> NEQ i);
    "<", (fun i -> LT i);
    ">", (fun i -> GT i);
    "<=", (fun i -> LEQ i);
    ">=", (fun i -> GEQ i);
    "==", (fun i -> EQ i);
  ]

  (* todo: use Symbol module *)
  let tbl = Hashtbl.create 1024
  let () =
    List.iter (fun (str, tk) -> Hashtbl.add tbl str tk) reserved_word
  let createId str i = 
    try Hashtbl.find tbl str i
    with _ -> ID({v=str;i})
  
  let info lexbuf = 
    let pos = lexbuf.lex_curr_p in 
    createInfo pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

}

let digit = ['0'-'9']
let int = digit+
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z']['_' 'a'-'z' 'A'-'Z' '0'-'9']*

rule read = 
  parse 
  | white { read lexbuf }
  | newline { next_line lexbuf; read lexbuf }
  | int { NUM {v=(int_of_string (Lexing.lexeme lexbuf)); i=info lexbuf}}
  | id { createId (Lexing.lexeme lexbuf) (info lexbuf) }

  | "+" | "-" | "*" | "/" | "<" | ">" | "=" 
  | "<=" | ">=" | "==" | "!=" | "&&" | "||" | "!" { createId (Lexing.lexeme lexbuf) (info lexbuf) }

  | "(" | ")" | "{" | "}" | ";" | "," { createId (Lexing.lexeme lexbuf) (info lexbuf) }
  | eof { EOF }
  | _ { raise (Syntax_Error (Lexing.lexeme lexbuf)) }
