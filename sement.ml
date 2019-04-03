
open Ast
open Symbol
open Support.Error
open Translate

exception Duplicated_Definition of info 
exception Lack_Definition of info
exception No_Initialization of info
exception Ill_Typed of info
exception Arity_Mismatched of info
exception Fundec_Mismatched of info
exception Not_Function of info
exception No_Fieldname of info
exception Not_Struct of info

module type env = sig 

  type ty
  (* struct env *)
  val base_senv : (Symbol.t, ty) Hashtbl.t Symbol.table
  (* variable env, maps vars to types *)
  val base_venv : ty Symbol.table
end

module Env : env with type ty = Ast.ty = struct 
  type ty = Ast.ty
  let base_senv = Symbol.empty 
  let base_venv = Symbol.empty
end

type def_bind = 
  | Vardef | Fundef of bool ref | Strucdef of bool ref * (Symbol.t, Ast.ty) Hashtbl.t

type def_env = def_bind Symbol.table 
type str_env = (Symbol.t, Ast.ty) Hashtbl.t Symbol.table

(**  check function declaration and variable declaration 
 **  create a global struct environment *)
let rec check_def : def_env -> Ast.stmt -> str_env = fun def_env -> function 
  | If(e,s,sop,_) -> 
    check_def_exp def_env e;
    ignore (check_def def_env s); 
    ignore (match sop with 
     | Some s' -> ignore (check_def def_env s)
     | None -> ());
    empty
  | While(e,s,i) -> check_def_exp def_env e; ignore (check_def def_env s); empty
  | Seq(sl,_) -> List.iter (fun s -> ignore (check_def def_env s)) sl;  empty
  | Vardecl(id,_,s,i) -> 
    (try check_id_def id i def_env
    with Not_found -> ignore (check_def (enter id Vardef def_env) s));
    empty
  | Fundecl(id,_,s,i) -> 
    (try check_id_def id i def_env; empty (* can't be reached *)
    with Not_found -> 
      let br = ref false in 
      (let res = check_def (enter id (Fundef(br)) def_env) s in
      (if !br then () else raise (Lack_Definition i)); res))
  | Fundefn(id,idl,_,s',s,i) -> 
    (try let d = lookup id def_env in 
         match d with 
         | Vardef -> raise (Duplicated_Definition i) 
         | Strucdef(_) -> raise (Duplicated_Definition i)
         | Fundef br -> if !br then raise (Duplicated_Definition i)
                        else br := true;
                             ignore (check_def 
                             (List.fold_left
                              (fun acc id -> enter id Vardef acc) def_env idl)
                             s'); check_def def_env s
    with Not_found -> ignore (check_def (List.fold_left (fun acc id -> enter id Vardef acc) def_env idl) s');
                      check_def (enter id (Fundef(ref true)) def_env) s) 
  | Exp(e,_) -> check_def_exp def_env e; empty
  | Return(e,_) -> check_def_exp def_env e; empty
  | Assign(_,e,_) -> check_def_exp def_env e; empty 
  | Structdecl(id,s,i) -> 
    (try check_id_def id i def_env; empty (* can't be reached *)
    with Not_found -> 
      let br = ref false in 
      let res = check_def (enter id (Strucdef(br, Hashtbl.create 20)) def_env) s in
      (if !br then () else raise (Lack_Definition i));
      res
    )
  | Structdefn(id,fl,s,i) -> 
    (try let d = lookup id def_env in 
         match d with 
         | Vardef -> raise (Duplicated_Definition i) 
         | Fundef(_) -> raise (Duplicated_Definition i)
         | Strucdef (br,tbl) -> if !br then raise (Duplicated_Definition i)
                        else br := true;
                             List.iter (fun (id,t) ->
                                        (match t with 
                                        | NameTy(tid) -> (try ignore (lookup tid def_env) with Not_found -> raise (Lack_Definition i))
                                        | _ -> ()); Hashtbl.add tbl id t)
                             fl; enter id tbl (check_def def_env s)
    with Not_found -> 
      let tbl = Hashtbl.create 20 in 
      List.iter (fun (id,t) -> 
                (match t with
                | NameTy(tid) -> (try ignore (lookup tid def_env) with Not_found -> raise(Lack_Definition i))
                | _ -> ()); Hashtbl.add tbl id t)
                fl; enter id tbl (check_def (enter id (Strucdef(ref true, tbl)) def_env) s) )
  | Nop -> empty
and check_id_def id i def_env : unit =
  let _ = lookup id def_env in raise (Duplicated_Definition i)
and check_id_isdef id i def_env = 
  (try let _ = lookup id def_env in () 
  with Not_found -> raise (Lack_Definition i))
and check_def_exp def_env = function 
  | Var(v) -> check_def_var_exp def_env v
  | Bin(e1,_,e2,_) -> check_def_exp def_env e1; check_def_exp def_env e2
  | Un(_,e,_) -> check_def_exp def_env e
  | App(id,el,i) -> check_id_isdef id i def_env;
    List.iter (check_def_exp def_env) el
  | ArrayAlloc(t,e,i) -> 
    (match t with 
    | NameTy(id) -> check_id_isdef id i def_env; check_def_exp def_env e 
    | _ -> ())
  | Alloc(t,i) -> 
    (match t with 
    | NameTy(id) -> check_id_isdef id i def_env 
    | _ -> ()) 
  | Intconst(_) | True(_) | False(_)
  | Nil -> ()
and check_def_var_exp def_env = function 
  | SimpVar(id,i) -> 
    (try ignore (lookup id def_env)
    with Not_found -> raise (Lack_Definition(i)))
  | FieldVar(v,_,_) -> check_def_var_exp def_env v 
  | SubscriptVar(v,_,_) -> check_def_var_exp def_env v

(* TODO: add init check for struct *)
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
  | Structdecl(_,s,_) -> check_init s 
  | Structdefn(_,_,s,_) -> check_init s
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
  | Structdecl(_,s,_) -> live id s 
  | Structdefn(_,_,s,_) -> live id s 
and live_seq id = function
  | [] -> false 
  | [s] -> live id s 
  | s::sl -> live id s || not (def id s) && live_seq id sl
and use id = function 
  | Var(SimpVar(id',_)) -> id = id' 
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
  | Assign(SimpVar(id',_),_,_) -> id = id'
  | Vardecl(_,_,s,_) -> def id s 
  | Fundecl(_,_,s,_) -> def id s 
  | Fundefn(_,_,_,_,s,_) -> def id s 
  | _ -> false


let rec check_type ctx = function
  | Assign(SimpVar(id,_),e,i) ->
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
  | Var(SimpVar(id,_)) -> lookup id ctx
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
