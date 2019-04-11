

type ty = 
  | Primitive of primitive_type
  | Object

and primitive_type = [
  | `Int | `Bool | `Void | `Any
]

and obj_type = [
  | `Object
  | `ArrayTy of ty 
  | `ClassTy of class_type
]

and class_type = 
  { name : Symbol.t; field_signature : (Symbol.t * ty) list }