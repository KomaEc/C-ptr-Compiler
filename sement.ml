
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

type def_bind = 
  | Vardef | Fundef of bool ref

let rec check_def def_env = function 
  | If(e,s,sop,_) -> 
    check_def_exp def_env e;
    check_def def_env s; 
    (match sop with 
     | Some s' -> check_def def_env s
     | None -> ()) 
  | While(e,s,i) -> check_def_exp def_env e; check_def def_env s 
  | Seq(sl,_) -> List.iter (check_def def_env) sl 
  | Vardecl(id,_,s,i) -> 
    (try check_id_def id i def_env
    with Not_found -> check_def (enter id Vardef def_env) s)
  | Fundecl(id,_,s,i) -> 
    (try check_id_def id i def_env
    with Not_found -> 
      let br = ref false in 
      check_def (enter id (Fundef(br)) def_env) s;
      if !br then () else raise (Lack_Definition i)) 
  | Fundefn(id,idl,_,s',s,i) -> 
    (try let d = lookup id def_env in 
         match d with 
         | Vardef -> assert false 
         | Fundef br -> if !br then raise (Duplicated_Definition i)
                        else br := true;
                             check_def 
                             (List.fold_left
                              (fun acc id -> enter id Vardef acc) def_env idl)
                             s'; check_def def_env s
    with Not_found -> check_def (List.fold_left (fun acc id -> enter id Vardef acc) def_env idl) s';
                      check_def (enter id (Fundef(ref true)) def_env) s) 
  | Exp(e,_) -> check_def_exp def_env e
  | Return(e,_) -> check_def_exp def_env e
  | Assign(_,e,_) -> check_def_exp def_env e 
  | Nop -> ()
and check_id_def id i def_env =
  let _ = lookup id def_env in raise (Duplicated_Definition i)
and check_id_isdef id i def_env = 
  (try let _ = lookup id def_env in () 
  with Not_found -> raise (Lack_Definition i))
and check_def_exp def_env = function 
  | Var(id,i) -> check_id_isdef id i def_env 
  | Bin(e1,_,e2,_) -> check_def_exp def_env e1; check_def_exp def_env e2
  | Un(_,e,_) -> check_def_exp def_env e
  | App(id,el,i) -> check_id_isdef id i def_env;
    List.iter (check_def_exp def_env) el
  | _ -> ()

let rec check_init = function 
  | Assign(_) | Nop | Exp(_) | Return(_) -> ()
  | If(_,s,sop,_) -> check_init s;
    (match sop with Some s' -> check_init s' | None -> ())
  | While(_,s,_) -> check_init s 
  | Seq(sl,_) -> List.iter check_init sl 
  | Vardecl(id,_,s,i) -> 
    check_init s; 
    if live id s then raise (No_Initialization i) else () 
  | Fundecl(_,_,s,_) -> check_init s 
  | Fundefn(_,_,_,s',s,_) -> check_init s'; check_init s
and live id = function 
  | Assign(_,e,_) -> use id e 
  | If(e,s,sop,_) ->
    use id e || live id s 
    || (match sop with Some s' -> live id s' | None -> false) 
  | While(e,s,_) -> 
    use id e || live id s 
  | Return(e,_) -> use id e 
  | Nop -> false 
  | Exp(e,_) -> use id e 
  | Seq(sl,_) -> live_seq id sl
  | Vardecl(id',_,s,_) -> live id s && not (id = id') 
  | Fundecl(_,_,s,_) -> live id s 
  | Fundefn(_,_,_,_,s,_) -> live id s
and live_seq id = function
  | [] -> false 
  | [s] -> live id s 
  | s::sl -> live id s || not (def id s) && live_seq id sl
and use id = function 
  | Var(id',_) -> id = id' 
  | Bin(e1,_,e2,_) -> use id e1 || use id e2 
  | Un(_,e,_) -> use id e 
  | App(_,el,_) -> List.fold_left (fun acc e -> acc || use id e) false el
  | _ -> false
and def id = function 
  | If(_,s,sop,_) -> 
    def id s && (match sop with Some s' -> def id s' | None -> true) 
  | Return(_) -> true 
  | Seq(sl,_) -> 
    List.fold_left (fun acc s -> acc || def id s) false sl 
  | Assign(id',_,_) -> id = id'
  | Vardecl(_,_,s,_) -> def id s 
  | Fundecl(_,_,s,_) -> def id s 
  | Fundefn(_,_,_,_,s,_) -> def id s 
  | _ -> false


let rec check_type ctx = function
  | Assign(id,e,i) ->
    let ty = lookup id ctx in 
    let ty' = check_type_exp ctx e in 
    if not (ty = ty') then raise (Ill_Typed i) else () 
  | If(e,s,sop,i) -> 
    let ty = check_type_exp ctx e in 
    if not (ty = Bool) then raise(Ill_Typed (extract_info_exp e))
    else check_type ctx s; 
    (match sop with Some s' -> check_type ctx s' | None -> ())
  | While(e,s,i) -> 
    let ty = check_type_exp ctx e in 
    if not (ty = Bool) then raise(Ill_Typed (extract_info_exp e)) 
    else check_type ctx s 
  | Seq(sl,_) -> List.iter (check_type ctx) sl 
  | Vardecl(id,t,s,_) -> check_type (enter id t ctx) s 
  | Fundecl(id,t,s,_) -> check_type (enter id t ctx) s 
  | Fundefn(id,idl,t,s',s,i) -> 
    (match t with 
    | Arrow(tyl, ty) as t -> 
      (try let t' = lookup id ctx in 
      if not (t = t') then raise (Fundec_Mismatched i) with 
      Not_found -> ());
      if not (List.length idl = List.length tyl) then raise (Arity_Mismatched i)
      else let ctx' = enter id t ctx in 
           let ctx'' = List.fold_left2 
           (fun acc id ty -> enter id ty acc) ctx' idl tyl in 
           check_type ctx'' s';
           check_type ctx' s 
    | _ -> assert false)
  | _ -> ()
and check_type_exp ctx = function 
  | Intconst(_) -> Int 
  | True(_) | False(_) -> Bool 
  | Var(id,_) -> lookup id ctx
  | Bin(e1,binop,e2,i) ->
    (match binop with 
    | Plus | Minus | Times | Div -> 
      check_int_binop ctx e1 e2 i; Int 
    | Lt | Gt -> 
      check_int_binop ctx e1 e2 i; Bool
    | And | Or -> check_bool_binop ctx e1 e2 i; Bool 
    | Eq -> 
      let ty = check_type_exp ctx e1 in 
      let ty' = check_type_exp ctx e2 in 
      if not (ty = ty') then raise (Ill_Typed(i))
      else ty
      )
  | Un(uop,e,i) -> 
    let ty = check_type_exp ctx e in 
    if not (ty = Bool) then raise (Ill_Typed(i))
    else Bool 
  | App(id,el,i) -> 
    (match lookup id ctx with 
    | Arrow(tyl, ty) -> 
      if not (List.length tyl = List.length el) 
      then raise (Arity_Mismatched(i))
      else List.iter2 (fun ty e -> if not (ty = check_type_exp ctx e) 
                                   then raise (Ill_Typed(extract_info_exp e))
                                   else ()) tyl el; ty 
    | _ -> raise (Not_Function(i)))
and check_int_binop ctx e1 e2 i = 
  let ty = check_type_exp ctx e1 in 
  let ty' = check_type_exp ctx e2 in 
  if not (ty = Int && ty' = Int) then raise (Ill_Typed(i))
  else ()
and check_bool_binop ctx e1 e2 i = 
  let ty = check_type_exp ctx e1 in 
  let ty' = check_type_exp ctx e2 in 
  if not (ty = Bool && ty' = Bool) then raise (Ill_Typed(i))
  else ()

let check s = 
  check_def empty s;
  check_init s;
  check_type empty s