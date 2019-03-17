open Ast 

let rec print_stmt pre = function 
  | Assign(id,e,_) -> 
      print_string pre;
      print_string (id ^ " = "); print_exp e; print_string ";"
  | If(e,s,sop,_) -> 
      print_string pre;
      print_string "if ("; print_exp e; print_endline ")"; 
      print_stmt (pre^"  ") s;
      print_newline();
      (match sop with
        | Some s ->  print_endline (pre^"else"); print_stmt (pre^"  ") s 
        | None -> ())
  | While(e,s,_) -> 
      print_string (pre^"while (");
      print_exp e; print_endline ")";
      print_stmt (pre^"  ") s
  | Return(e,_) ->
      print_string (pre^"return ");
      print_exp e; print_string ";"
  | Nop -> () 
  | Exp(e,_) -> 
      print_string pre; print_exp e; print_string ";"
  | Seq(sl,_) -> 
      print_string (pre^"{");
      List.iter (fun s -> print_newline(); print_stmt pre s) sl;
      print_newline();
      print_string (pre^"}"); 
  | Decl(id,t,_) -> 
      print_string pre;
      print_ty t; print_string (" "^id^";")
and print_exp = function 
  | Intconst(i,_) -> print_int i
  | True(_) -> print_string "true";
  | False(_) -> print_string "false";
  | Var(id,_) -> print_string id;
  | Un(op, exp,_) ->
      (match op with 
      | Not -> (match exp with 
               | Bin(_) -> print_string "!("; print_exp exp; print_string")"
               | _ -> print_string "!"; print_exp exp;))
  | Bin(e1,bop,e2,_) ->
      print_exp_paren e1;
      print_string 
      (match bop with 
      | Plus -> " + "
      | Minus -> " - "
      | Times -> " * "
      | Div -> " / "
      | And -> " && "
      | Or -> " || "
      | Lt -> " < "
      | Gt -> " > "
      | Eq -> " == ");
      print_exp_paren e2;
and print_ty = function 
  | Int(_) -> print_string "int"
  | Bool(_) -> print_string "bool"
and print_exp_paren = function 
  | Bin(_) as e ->  print_string "("; print_exp e; print_string")";
  | _ as e -> print_exp e;