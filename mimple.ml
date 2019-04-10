
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


let intermediate_to_rvalue = 
  function 
    | `Temp(t) -> `Temp(t) 
    | `Const(c) -> `Const(c)

let string_of_const : const -> string = 
  function
    | `Int_const(num) -> string_of_int num 
    | `Null_const -> "NULL"


let rec string_of_value : rvalue -> string = 
  function 
    | `Temp(t) -> string_of_temp t 
    | `Const(c) -> string_of_const c
    | `Expr(expr) -> string_of_expr expr 
    | `Array_ref(`Temp(t), i) -> 
      string_of_temp t ^ "[" ^ string_of_value (intermediate_to_rvalue i) ^ "]"
    | `Instance_field_ref(`Temp(t), id) -> 
      string_of_temp t ^ "." ^ Symbol.name id 
    | `Static_field_ref(id) -> 
      Symbol.name id 

and string_of_stmt : stmt -> string = 
  function 
    | `Assign(var, rvalue) -> 
      "  " ^ string_of_value (var_to_rvalue var) ^ " = " ^ string_of_value rvalue 
      ^ ";\n"
    | `Identity(t, id_value) -> 
      "  " ^ string_of_value (intermediate_to_rvalue t) ^ " := " 
      ^ string_of_identity_value id_value ^ ";\n"
    | `Label(l) -> 
      string_of_label l ^ ":\n"
    | `Goto(l) -> 
      "  goto " ^ string_of_label l ^ ";\n"
    | `If(`Temp(t), l) -> 
      "  if " ^ string_of_temp t ^ " goto " ^ string_of_label l ^ ";\n"
    | `If(`Rel(_) as rexpr, l) -> 
      "  if " ^ string_of_expr rexpr ^ " goto " ^ string_of_label l ^ ";\n"
    | `Static_invoke(l, i_list) as expr -> 
      "  " ^ string_of_expr expr ^ ";\n"
    | `Ret(i) -> 
      "  return " ^ string_of_value (intermediate_to_rvalue i) ^ ";\n"
    | `Ret_void -> 
      "  return;\n"
    | `Nop -> ""


and string_of_expr : expr -> string = 
  function 
    | `Bin(i1, bop, i2) -> 
      string_of_value (intermediate_to_rvalue i1) ^ string_of_bop bop ^ string_of_value (intermediate_to_rvalue i2) 
    | `Rel(i1, rop, i2) -> 
      string_of_value (intermediate_to_rvalue i1) ^ string_of_rop rop ^ string_of_value (intermediate_to_rvalue i2) 
    | `Static_invoke(l, i_list) -> 
      string_of_label l ^ "(" ^ string_of_intermediate_list i_list
    | `Alloc(i) -> 
      "alloc(" ^ string_of_value (intermediate_to_rvalue i) ^ ")"

and string_of_identity_value : identity_value -> string = 
  function
    | `Parameter_ref(num) -> 
      "@P" ^ string_of_int num

and string_of_bop : binop -> string = 
  function 
    | `Plus -> " + "
    | `Times -> " * "
    | `Minus -> " - "
    | `Div -> " / "

and string_of_rop : relop -> string = 
  function 
    | `Eq -> " == "
    | `Lt -> " < "
    | `Gt -> " > "
    | `And -> " && "
    | `Or -> " || "

and string_of_intermediate_list : intermediate list -> string = 
  function 
    | [] -> ")"
    | [x] -> string_of_value (intermediate_to_rvalue x) ^ ")"
    | x :: xl -> string_of_value (intermediate_to_rvalue x) ^ ", " 
                 ^ string_of_intermediate_list xl

let print_prog : prog -> unit = 
  List.iter 
    (fun s -> string_of_stmt s |> print_endline)