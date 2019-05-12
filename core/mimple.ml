(** Target of this intermedia representation:
 ** 1. isolate potentially effectful expressions, making their order
 ** of excution explicit. 
 ** 2. make the control flow explicit. 
 ** Therefore the IR here is described through pure expression and 
 ** command.  *)

 open Temp 
 open Types
 module UF = Cm_util.Union_and_find
 module S = Symbol

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
 (* TODO : change label to a variant type 
  * Label of Temp.label | Line_num of int *)

and target = [
  | `Label of label 
  | `Line_num of int
]

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
   | `Label of label 
   | `Goto of target 
   | `If of condition * target
   | `Static_invoke of method_signature * immediate list
   | `Ret of immediate 
   | `Ret_void
   | `Nop (* For optimization purpose *)
 ]

and identity = [
  | `Identity of [ `Temp of Temp.t ] * identity_value
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

and func = 
  {  func_name : Symbol.t;
     func_args : ty list;
     func_ret : ty;
     (* TODO : add identity declaration!!! *)
     local_decls : local_decl list;
     identities : identity list;
     func_body : stmt list
  }


(* TODO : add glb_vars info and cls info! *)
and prog = func list

let get_locals (func : func) : (Temp.t * ty) list = 
  List.map (fun (`Temp_decl(`Temp(t), ty)) -> (t, ty)) func.local_decls

let get_formals (func : func) : (Temp.t * ty) list = 
  List.map2 
  (fun (`Identity(`Temp(t), _)) ty -> (t, ty)) func.identities func.func_args

  let convert_to_lnum func = 
    let instrs = Array.of_list func.func_body in
    let tbl = Hashtbl.create 16 in 
    let offset = ref 0 in 
    Array.iteri 
    (fun i -> function 
    | `Label(l) when i > 0 -> 
      Hashtbl.add tbl l (i - !offset - 1);
      incr offset 
    | _ -> ()) instrs;
    let aux = Array.make (Array.length instrs - !offset - 1) `Nop in
    offset := 0;
    Array.iteri
    (fun i -> function 
    | _ when i = 0 -> () 
    | `Label(_) -> () 
    | `Goto(`Label(l)) -> 
      aux.(!offset) <- (`Goto(`Line_num(Hashtbl.find tbl l)));
      incr offset 
    | `If(cond, `Label(l)) -> 
      aux.(!offset) <- (`If(cond, `Line_num(Hashtbl.find tbl l)));
      incr offset
    | _ as stmt -> 
      aux.(!offset) <- stmt;
      incr offset) instrs ;
      { func with func_body = Array.to_list aux}


let transform_stmt (to_var : Temp.t -> immediate) : stmt -> stmt * bool = 
  let flag = ref false in

let rec transform_stmt : stmt -> stmt = 
  function 
    | `Assign(var, rvalue) -> 
      begin 
        match var with 
          | `Temp(t) -> 
            begin 
              match to_var t with 
                | `Const(c) -> flag := true; `Assign(var, `Const(c))
                | _ -> `Assign(transform_var var, transform_rvalue rvalue)
            end 
          | _ as var -> `Assign(transform_var var, transform_rvalue rvalue)
      end
    | `Static_invoke(msig, il) -> 
      `Static_invoke(msig, List.map transform_immediate il) 
    | `Ret(i) -> `Ret(transform_immediate i)
    | _ as s -> s 
and transform_var = 
  function 
    | `Array_ref(i, i') -> `Array_ref(i, transform_immediate i') 
    | _ as var -> var
and transform_rvalue  = 
  function 
    | `Temp(t) -> (to_var t :> rvalue) 
    | `Expr(expr) -> `Expr(transform_expr expr) 
    | `Array_ref(i, i') -> `Array_ref(i, transform_immediate i')
    | _ as v -> v 
and transform_expr = 
  function 
    | `Bin(i1, op, i2) -> 
      `Bin(transform_immediate i1, op, transform_immediate i2) 
    | `Rel(i1, op, i2) -> 
      `Rel(transform_immediate i1, op, transform_immediate i2) 
    | `Static_invoke(msig, il) -> 
      `Static_invoke(msig, List.map transform_immediate il) 
    | `New_array_expr(ty, i) -> `New_array_expr(ty, transform_immediate i) 
    | _ as e -> e 
and transform_immediate = 
  function 
    | `Temp(t) -> 
      begin
        match to_var t with 
          | `Const(_) as c -> flag := true; c 
          | _ as t -> t 
      end
    | _ as c -> c in 

    fun stmt -> transform_stmt stmt, !flag




  
let rec temps_in_rvalue : rvalue -> Temp.t list = function 
  | `Temp(t) -> [t]
  | `Expr(expr) -> temps_in_expr expr 
  | `Array_ref(i1, i2) -> temps_in_immediate i1 @ temps_in_immediate i2 
  | `Instance_field_ref(i, _) -> temps_in_immediate i 
  | _ -> [] 
  
and temps_in_expr : expr -> Temp.t list = function 
  | `Bin(i1, _, i2) | `Rel(i1, _, i2) -> 
    temps_in_immediate i1 @ temps_in_immediate i2 
  | `Static_invoke(_, i_list) -> 
    List.fold_left (fun acc i -> temps_in_immediate i @ acc) [] i_list 
  | `New_array_expr(_, i) -> temps_in_immediate i 
  | _ -> [] 

and temps_in_immediate : immediate -> Temp.t list = 
  fun x -> temps_in_rvalue (x :> rvalue)

and temps_in_var : var -> Temp.t list = 
  fun x -> temps_in_rvalue (x :> rvalue)

and temps_in_condition : condition -> Temp.t list = function 
  | `Temp(t) -> [t] 
  | `Rel(x) -> temps_in_expr (`Rel(x))

(* Printing Utility *)

let var_to_rvalue : var -> rvalue = 
  fun x -> (x :> rvalue)


let immediate_to_rvalue : immediate -> rvalue = 
  fun x -> (x :> rvalue)

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

and string_of_target : target -> string = 
  function 
    | `Label(l) -> string_of_label l 
    | `Line_num(i) -> string_of_int i

and string_of_stmt : stmt -> string = 
  function 
    | `Assign(var, rvalue) -> 
      "  " ^ string_of_value (var_to_rvalue var) ^ " = " ^ string_of_value rvalue 
      ^ ";"
    | `Label(l) -> 
      string_of_label l ^ ":"
    | `Goto(l) -> 
      "  goto " ^ string_of_target l ^ ";"
    | `If(`Temp(t), l) -> 
      "  if " ^ string_of_temp t ^ " goto " ^ string_of_target l ^ ";"
    | `If(`Rel(_) as rexpr, l) -> 
      "  if " ^ string_of_expr rexpr ^ " goto " ^ string_of_target l ^ ";"
    | `Static_invoke(_) as expr -> 
      "  " ^ string_of_expr expr ^ ";"
    | `Ret(i) -> 
      "  return " ^ string_of_value (immediate_to_rvalue i) ^ ";"
    | `Ret_void -> 
      "  return;"
    | `Nop -> ""

and string_of_decl : local_decl -> string = 
  function 
    | `Temp_decl(`Temp(t), ty) -> 
    "  " ^ string_of_ty ty ^ " " ^ string_of_temp t ^ ";"

and string_of_identity : identity -> string = 
  function 
  | `Identity(`Temp(t), id_value) -> 
      "  " ^ string_of_value (`Temp(t)) ^ " := " 
      ^ string_of_identity_value id_value ^ ";"

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
  fun { func_name; func_args; func_ret; identities; local_decls; func_body } -> 
    "\nBeginFunc " ^ Symbol.name func_name 
    ^ " : " ^ string_of_ty_list func_args ^ " -> " 
    ^ string_of_ty func_ret ^ "\n"
    ^ (List.fold_left (fun acc decl -> acc ^ string_of_decl decl ^ "\n") "" local_decls)
    ^ (List.fold_left (fun acc idt -> acc ^ string_of_identity idt ^ "\n") "" identities)
    ^ (List.fold_left (fun acc stmt -> acc ^ string_of_stmt stmt ^ "\n") "" func_body)
    ^ "EndFunc\n"
    

let string_of_prog : prog -> string = 
  List.fold_left
    (fun prev method_chunk -> 
      prev ^ string_of_func method_chunk) ""

let print_prog : prog -> unit = 
  fun prog -> string_of_prog prog |> print_endline