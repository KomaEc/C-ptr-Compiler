
(* open Mimple *)
open Types
module M = Mimple_temp

(* TODO : add global var reference, when doing semantics check, 
 * the occurence of glb vars are recorded here 
 * Treated as a static field ! *)


let type_convert : Ast.ty -> ty = 
  function 
    | Int -> Primitive(`Int) 
    | Bool -> Primitive(`Bool)
    | NameTy(ty_id) when Symbol.name ty_id = "Object" -> Object(`Object)
    | NameTy(ty_id) -> Object(`ClassTy(ty_id))
    | _ -> raise (Invalid_argument "primitive_type_convert")

let glb_static_vars_tbl_ref : ty Symbol.table ref = 
  ref Symbol.empty

let add_glb_vars : Symbol.t -> Ast.ty -> unit = 
  fun name ty ->
    let open Ast in
    let ty  = type_convert ty in
    glb_static_vars_tbl_ref := Symbol.enter name ty !glb_static_vars_tbl_ref

let glb_class_sig_tbl_ref : (Symbol.t * ty) list Symbol.table ref
  = ref Symbol.empty

(* TODO: get glb_class_sig_tbl *)


let cur_local_def : (Temp.t * ty) list ref = 
  ref [] 

let emit_local_def : Temp.t -> Ast.ty -> unit = 
  fun t ty -> 
    let open Ast in 
    let ty = type_convert ty in
    cur_local_def := (t, ty) :: !cur_local_def


let cur_method_chunk : M.stmt list ref = 
  ref []

let emit_stmt : M.stmt -> unit = 
  fun s -> cur_method_chunk := s :: !cur_method_chunk



let prog_frag : M.prog ref = ref []

let end_function () = 
  let decl_list = 
    List.rev !cur_local_def 
    |> List.map (fun (t, ty) -> `Temp_decl(`Temp(t), ty))
    in 
  let stmt_list = List.rev !cur_method_chunk in
  let () = cur_local_def := [] in 
  let () = cur_method_chunk := [] in
  prog_frag := (decl_list @ stmt_list) :: !prog_frag

let get_mimple () = 
  List.rev !prog_frag

(*
let code_frag : prog ref = ref [] 

let emit : stmt -> unit =
  fun s -> code_frag := s :: !code_frag
  


let final : prog -> prog = List.rev

let get_final_mimple () = final !code_frag
*)