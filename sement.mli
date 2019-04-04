open Ast
open Symbol
open Support.Error

exception Duplicated_Definition of info 
exception Lack_Definition of info
exception No_Initialization of info
exception Ill_Typed of info
exception Arity_Mismatched of info
exception Fundec_Mismatched of info
exception Not_Function of info
exception No_Fieldname of info
exception Not_Struct of info
exception Alloc_Non_Struct of info
exception Type_Var_Misuse of info

(*
type def_bind 

val check_def : def_bind table -> stmt -> unit 

val check_init : stmt -> unit 

val check_type : ty table -> stmt -> unit 
*)

val check : stmt -> unit
