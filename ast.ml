(*
type ident = string 
type const = int 
type binop = 
  | Plus | Minus | Times | Div 
type prog = stm list 
and stm = 
  | Assign of ident * exp 
  | Return of exp 
and exp = 
  | Const of const 
  | Var of ident 
  | Bin of exp * binop * exp 

*)
open Support.Error

type stmt = 
  | Assign of ident * exp * info
  | If of exp * stmt * stmt option * info
  | While of exp * stmt * info 
  | Return of exp * info 
  | Nop
  | Exp of exp * info 
  | Seq of stmt list * info 
  | Decl of ident * ty * stmt * info  (* succesive statement *)
and ident = string 
and exp = 
  | Intconst of int * info
  | True of info | False of info
  | Var of ident * info 
  | Bin of exp * binop * exp * info
  | Un of unop * exp * info
and binop = Plus | Minus | Times | Div | And | Or
  | Lt | Gt | Eq
and unop = Not
and ty = Int of info | Bool of info

  

let cons s i sl = 
  match sl with 
  | Seq(sl',_) -> Seq(s::sl',i) 
  | _ as sl -> Seq([s;sl], i)

let rec simplify = function 
  | If(e,s,sop,i) -> 
    (match sop with 
     | Some s' -> If(e, simplify s, Some (simplify s'),i)
     | None -> If(e, simplify s, sop, i))
  | While(e,s,i) -> While(e, simplify s,  i) 
  | Seq(sl, i) -> Seq(simplify_seq sl, i)
  | _ as s -> s 
and simplify_seq sl = 
  List.fold_right 
    (fun s acc -> 
    match simplify s with 
    | Seq(sl', _) -> sl' @ acc 
    | _ as s' -> s' :: acc) sl []

let extract_info_stmt = function 
  | Assign(_,_,i) -> i 
  | If(_,_,_,i) -> i 
  | While(_,_,i) -> i 
  | Return(_,i) -> i 
  | Nop -> dummyinfo 
  | Exp(_,i) -> i 
  | Seq(_,i) -> i 
  | Decl(_,_,_,i) -> i 
and extract_info_exp = function 
  | Intconst(_,i) -> i 
  | True(i) -> i 
  | False(i) -> i 
  | Var(_,i) -> i 
  | Bin(_,_,_,i) -> i 
  | Un(_,_,i) -> i 
and extract_info_type = function 
  | Int(i) -> i 
  | Bool(i) -> i
