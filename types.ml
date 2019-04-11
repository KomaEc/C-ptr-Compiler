

type ty = 
  | Primitive of primitive_type
  | Object of obj_type

and primitive_type = [
  | `Int | `Bool | `Void 
]

and obj_type = [
  | `Object
  | `ArrayTy of ty 
  | `ClassTy of class_type
]

and class_type = 
  { name : Symbol.t; field_signature : (Symbol.t * ty) list }


let rec string_of_ty : ty -> string = 
  function 
    | Primitive(`Int) -> "int"
    | Primitive(`Bool) -> "bool"
    | Primitive(`Void) -> "void"
    | Object(`Object) -> "Object" 
    | Object(`ArrayTy(ty)) -> string_of_ty ty ^ "[]"
    | Object(`ClassTy(_ as cl)) -> string_of_class_type cl

and string_of_class_type : class_type -> string = 
  fun { name; field_signature } -> 
    Symbol.name name
      
and string_of_ty_list : ty list -> string = 
  fun ty_list -> 
    begin 
      match ty_list with 
        | [] -> ")" 
        | [ty] -> string_of_ty ty ^ ")"
        | ty :: tyl -> 
          string_of_ty ty 
          ^ List.fold_left 
              (fun acc ty -> 
                acc ^ ", " ^ string_of_ty ty) "" tyl ^ ")"
    end
    |> ( ^ ) "("