
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
   | `Array_ref of [ `Temp of Temp.t ] * intermediate
   | `Instance_field_ref of [` Temp of Temp.t ] * Symbol.t
   | `Static_field_ref of Symbol.t
 ]

 and label = Temp.label 

 and rvalue = [
   | `Temp of Temp.t
   | `Const of const
   | `Expr of expr
   | `Array_ref of [ `Temp of Temp.t ] * intermediate
   | `Instance_field_ref of [ `Temp of Temp.t ] * Symbol.t
   | `Static_field_ref of Symbol.t
 ]

 and prog = stmt list 

 and stmt = [
   | `Assign of var * rvalue
   | `Identity of [ `Temp of Temp.t ] * identity_value 
   | `Label of label 
   | `Goto of label 
   | `If of condition * label
   | `Static_invoke of label * intermediate list
   | `Ret of intermediate 
   | `Ret_void
   | `Nop
 ]

 and condition = [
   | `Temp of Temp.t
   | `Const of const
   | `Rel of intermediate * relop * intermediate
 ]

 and expr = [
   | `Bin of intermediate * binop * intermediate
   | `Rel of intermediate * relop * intermediate
   | `Static_invoke of label * intermediate list
   | `Alloc of intermediate
 ]

 and identity_value = [
   | `Parameter_ref of int
 ]

 and binop = [ `Plus | `Minus | `Times | `Div ]

 and relop = [ `Eq | `Lt | `Gt | `And | `Or ]


 let var_to_rvalue = 
   function 
     | `Temp(t) -> `Temp(t) 
     | `Array_ref(t, i) -> `Array_ref(t, i)
     | `Instance_field_ref(t, id) -> `Instance_field_ref(t, id)
     | `Static_field_ref(id) -> `Static_field_ref(id)

