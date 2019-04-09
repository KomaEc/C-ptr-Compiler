
(** Target of this intermedia representation:
 ** 1. isolate potentially effectful expressions, making their order
 ** of excution explicit. 
 ** 2. make the control flow explicit. 
 ** Therefore the IR here is described through pure expression and 
 ** command.  *)

 open Temp 


type intermediate = [
  | `Const of const 
  | `Temp of Temp.t
]

and const = [
  | `Null_const 
  | `Int_const of int
]

 and var = [
   | `Temp of Temp.t 
   | `Array_ref of intermediate * intermediate
 ]

 and label = Temp.label 

 and rvalue = [
   | `Array_ref of intermediate * intermediate
   | `Expr of expr
 ]

 and prog = stmt list 

 and stmt = [
   | `Assign of var * rvalue
   | `Label of label 
   | `Goto of label 
   | `If of condition * label
   | `Static_invoke of label * intermediate list
   | `Ret of intermediate 
   | `Ret_void
   | `Nop
 ]

 and condition = [
   | `Rel of intermediate * binop * intermediate
 ]

 and expr = [
   | `Bin of intermediate * binop * intermediate
   | `Rel of intermediate * binop * intermediate
   | `Static_invoke of label * intermediate list
 ]

 and binop = [ `Plus | `Minus | `Times | `Div ]

 and relop = [ `Eq | `Lt | `Gt | `And | `Or ]