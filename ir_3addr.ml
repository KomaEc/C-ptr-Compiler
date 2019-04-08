
(** Target of this intermedia representation:
 ** 1. isolate potentially effectful expressions, making their order
 ** of excution explicit. 
 ** 2. make the control flow explicit. 
 ** Therefore the IR here is described through pure expression and 
 ** command.  *)

open Temp

type var = Temp.t

and label = Temp.label

and prog = stmt list 

and stmt = 
  | Assign of var * simp_exp
  | Arith of var * simp_exp * binop * simp_exp 
  | Rel of var * simp_exp * relop * simp_exp 
  | Label of label 
  | Jmp of label 
  | CJmp of var * label 
  | Push of var 
  | Pop of var 
  | Fun_call_void of label 
  | Fun_call_ret of label * var
  | Fun_begin of int 
  | Fun_end 
  | Ret_val of var 
  | Ret_void
  | Fetch_simp of var * var 
  | Fetch_comp of var * var * int 
  | Store_simp of var * var 
  | Store_comp of var * int * var 

and binop = Plus | Minus | Times | Div

and relop = Eq | Lt | Gt | And | Or

and simp_exp = 
  | Const of int 
  | Var of var

let print_header () = print_string "  "

let string_of_bop = function 
  | Plus -> " + "
  | Minus -> " - "
  | Times -> " * "
  | Div ->  " / "

let string_of_rop = function 
  | Eq ->  " == "
  | Lt ->  " < "
  | And ->  " && "
  | Or ->  " || "
  | Gt -> " > "

let string_of_simp_exp = function 
  | Var(t) -> string_of_temp t 
  | Const(i) -> string_of_int i

let rec print_stmt = function 
  | Assign(t1, t2) -> 
    print_header();
    print_endline (string_of_temp t1 ^ " = " ^ string_of_simp_exp t2 ^ ";")
  (*| Assign_label(t, l) -> 
    print_header();
    print_endline (string_of_temp t ^ " = " ^ string_of_label l ^ ";") *)
  | Arith(t1, t2, bop, t3) -> 
    print_header();
    print_endline (string_of_temp t1 ^ " = " 
                  ^ string_of_simp_exp t2 ^ string_of_bop bop 
                  ^ string_of_simp_exp t3 ^ ";")
  | Rel(t1, t2, rop, t3) -> 
    print_header();
    print_endline (string_of_temp t1 ^ " = " 
                  ^ string_of_simp_exp t2 ^ string_of_rop rop 
                  ^ string_of_simp_exp t3 ^ ";")
  | Label(l) -> 
    print_endline (string_of_label l ^ ":")
  | Jmp(l) -> 
    print_header();
    print_endline ("goto " ^ string_of_label l ^ ";")
  | CJmp(t, l) -> 
    print_header();
    print_endline ("ifZ " ^ string_of_temp t ^ " goto "
                  ^ string_of_label l ^ ";")
  | Push(t) -> 
    print_header();
    print_endline ("push_param " ^ string_of_temp t ^ ";")
  | Pop(t) -> 
    print_header();
    print_endline ("pop_param " ^ string_of_temp t ^ ";")
  | Fun_call_void(l) ->
    print_header();
    print_endline ("LCall " ^ string_of_label l ^ ";") 
  | Fun_call_ret(l, t) -> 
    print_header();
    print_endline (string_of_temp t ^ " = " 
                  ^ "LCall " ^ string_of_label l ^ ";")
  | Fun_begin(i) -> 
    print_header();
    print_endline ("begin_func " ^ string_of_int i ^ ";")
  | Fun_end -> 
    print_header();
    print_endline ("end_func;")
  | Ret_val(t) -> 
    print_header();
    print_endline ("return " ^ string_of_temp t ^ ";") 
  | Ret_void -> 
    print_header();
    print_endline "return;"
  | Fetch_simp(t1, t2) -> 
    print_header();
    print_endline (string_of_temp t1 ^ " = *(" ^ string_of_temp t2 
                  ^ ");")
  | Fetch_comp(t1, t2, i) -> 
    print_header();
    print_endline (string_of_temp t1 ^ " = *(" ^ string_of_temp t2 
                  ^ " + " ^ string_of_int i ^ ");") 
  | Store_simp(t1, t2) -> 
    print_header();
    print_endline ("*(" ^ string_of_temp t1 ^ ") = "
                  ^ string_of_temp t2 ^ ";")
 | Store_comp(t1, i, t2) -> 
    print_header();
    print_endline ("*(" ^ string_of_temp t1 ^ " + " ^ string_of_int i
                  ^ ") = " ^ string_of_temp t2 ^ ";")


let print_prog = List.iter print_stmt
