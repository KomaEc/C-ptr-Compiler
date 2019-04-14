
(** Target of this intermedia representation:
 ** 1. isolate potentially effectful expressions, making their order
 ** of excution explicit. 
 ** 2. make the control flow explicit. 
 ** Therefore the IR here is described through pure expression and 
 ** command.  *)

 open Temp 
 open Types


type immediate = [
  | `Const of const 
  | `Temp of Temp.t
]

and const = [
  | `Null_const 
  | `Int_const of int
  | `Bool_const of bool
]

 and var = [
   | `Temp of Temp.t 
   | `Array_ref of immediate * immediate
   | `Instance_field_ref of immediate * field_signature
   | `Static_field_ref of Symbol.t
 ]

 and label = Temp.label 

 and rvalue = [
   | `Temp of Temp.t
   | `Const of const
   | `Expr of expr
   | `Array_ref of immediate * immediate
   | `Instance_field_ref of immediate * field_signature
   | `Static_field_ref of Symbol.t
 ]

and method_signature = Symbol.t * ty list * ty

and field_signature = Symbol.t * ty

 and stmt = [
   | `Assign of var * rvalue
   | `Identity of [ `Temp of Temp.t ] * identity_value 
   | `Label of label 
   | `Goto of label 
   | `If of condition * label
   | `Static_invoke of method_signature * immediate list
   | `Ret of immediate 
   | `Ret_void
   | `Nop
 ]

 and local_decl = [
   | `Temp_decl of [ `Temp of Temp.t ] * ty
 ]

 and condition = [
   | `Temp of Temp.t
   | `Rel of immediate * relop * immediate
 ]

 and expr = [
   | `Bin of immediate * binop * immediate
   | `Rel of immediate * relop * immediate
   | `Static_invoke of method_signature * immediate list
   | `New_expr of obj_type
   | `New_array_expr of ty * immediate
 ]

 and identity_value = [
   | `Parameter_ref of int
 ]

 and binop = [ `Plus | `Minus | `Times | `Div ]

 and relop = [ `Eq | `Lt | `Gt | `And | `Or | `Not ]

 and meth = 
  { meth_name : Symbol.t; 
    meth_ret : ty; 
    meth_args : (Symbol.t * ty) list;
    meth_body : stmt list
   }

and cls = 
  { cls_name : Symbol.t;
    cls_super : Symbol.t; (* by default : Object *)
    cls_fields : (Symbol.t * ty) list;
    cls_meths : meth list
   }


and func = 
  {  func_name : Symbol.t;
     func_args : ty list;
     func_ret : ty;
     local_decls : local_decl list;
     func_body : stmt list
  }


(* TODO : add glb_vars info and cls info! *)
 and prog = func list



 let var_to_rvalue : var -> rvalue = 
   function 
     | `Temp(t) -> `Temp(t) 
     | `Array_ref(t, i) -> `Array_ref(t, i)
     | `Instance_field_ref(t, id) -> `Instance_field_ref(t, id)
     | `Static_field_ref(id) -> `Static_field_ref(id)


let immediate_to_rvalue : immediate -> rvalue = 
  function 
    | `Temp(t) -> `Temp(t) 
    | `Const(c) -> `Const(c)

let string_of_const : const -> string = 
  function
    | `Int_const(num) -> string_of_int num 
    | `Null_const -> "NULL"
    | `Bool_const(true) -> "true"
    | `Bool_const(false) -> "false"


let rec string_of_value : rvalue -> string = 
  function 
    | `Temp(t) -> string_of_temp t 
    | `Const(c) -> string_of_const c
    | `Expr(expr) -> string_of_expr expr 
    | `Array_ref(i, i') -> 
      string_of_value (immediate_to_rvalue i) ^ "[" ^ string_of_value (immediate_to_rvalue i') ^ "]"
    | `Instance_field_ref(i, fsig) -> 
      string_of_value (immediate_to_rvalue i) ^ "." ^ string_of_field_sig fsig
    | `Static_field_ref(id) -> 
      Symbol.name id 

and string_of_method_sig : method_signature -> string = 
  fun (label, ty_list, ty) -> 
    string_of_label label 
    ^ " [" ^ string_of_ty_list ty_list ^ " : " ^ string_of_ty ty ^ "]"

and string_of_field_sig : field_signature -> string = 
  fun (name, ty) -> 
    Symbol.name name ^ " : " ^ string_of_ty ty

and string_of_stmt : stmt -> string = 
  function 
    | `Assign(var, rvalue) -> 
      "  " ^ string_of_value (var_to_rvalue var) ^ " = " ^ string_of_value rvalue 
      ^ ";\n"
    | `Identity(`Temp(t), id_value) -> 
      "  " ^ string_of_value (`Temp(t)) ^ " := " 
      ^ string_of_identity_value id_value ^ ";\n"
    | `Label(l) -> 
      string_of_label l ^ ":\n"
    | `Goto(l) -> 
      "  goto " ^ string_of_label l ^ ";\n"
    | `If(`Temp(t), l) -> 
      "  if " ^ string_of_temp t ^ " goto " ^ string_of_label l ^ ";\n"
    | `If(`Rel(_) as rexpr, l) -> 
      "  if " ^ string_of_expr rexpr ^ " goto " ^ string_of_label l ^ ";\n"
    | `Static_invoke(_) as expr -> 
      "  " ^ string_of_expr expr ^ ";\n"
    | `Ret(i) -> 
      "  return " ^ string_of_value (immediate_to_rvalue i) ^ ";\n"
    | `Ret_void -> 
      "  return;\n"
    | `Nop -> ""

  and string_of_decl : local_decl -> string = 
    function 
      | `Temp_decl(`Temp(t), ty) -> 
      "  " ^ string_of_ty ty ^ " " ^ string_of_temp t ^ ";\n"


and string_of_expr : expr -> string = 
  function 
    | `Bin(i1, bop, i2) -> 
      string_of_value (immediate_to_rvalue i1) ^ string_of_bop bop ^ string_of_value (immediate_to_rvalue i2) 
    | `Rel(i1, rop, i2) -> 
      string_of_value (immediate_to_rvalue i1) ^ string_of_rop rop ^ string_of_value (immediate_to_rvalue i2) 
    | `Static_invoke(msig, i_list) -> 
      string_of_method_sig msig ^ "(" ^ string_of_immediate_list i_list
    | `New_expr(obj_type) ->
      "new " ^ string_of_ty (Object(obj_type)) ^ "()"
    | `New_array_expr(ty, i) -> 
      "new " ^ string_of_ty ty ^ "[" 
      ^ string_of_value (immediate_to_rvalue i) ^ "]"

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
    | `Not -> "!"

and string_of_immediate_list : immediate list -> string = 
  function 
    | [] -> ")"
    | [x] -> string_of_value (immediate_to_rvalue x) ^ ")"
    | x :: xl -> string_of_value (immediate_to_rvalue x) ^ ", " 
                 ^ string_of_immediate_list xl

let string_of_func : func -> string = 
  fun { func_name; func_args; func_ret; local_decls; func_body } -> 
    "\nBeginFunc " ^ Symbol.name func_name 
    ^ " : " ^ string_of_ty_list func_args ^ " -> " 
    ^ string_of_ty func_ret ^ "\n"
    ^ (List.fold_left (fun acc decl -> acc ^ string_of_decl decl) "" local_decls)
    ^ (List.fold_left (fun acc stmt -> acc ^ string_of_stmt stmt) "" func_body)
    ^ "EndFunc\n"
    

let string_of_prog : prog -> string = 
  List.fold_left
    (fun prev method_chunk -> 
      prev ^ string_of_func method_chunk) ""

let print_prog : prog -> unit = 
  fun prog -> string_of_prog prog |> print_endline