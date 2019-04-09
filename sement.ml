
open Ast
open Ast.Util
open Symbol
open Support.Error
open Translate
open Temp

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
exception Not_Proper_Ret of info 


module type env = sig 

  type ty
  type entry = Var of ty | Func of ty list * ty
  (* struct env *)
  val base_senv : (Symbol.t, ty) Hashtbl.t Symbol.table
  (* variable env, maps vars to types *)
  val base_venv : entry Symbol.table
end

module Env : env with type ty = Ast.ty = struct 
  type ty = Ast.ty
  type entry = Var of ty | Func of ty list * ty
  let base_senv = Symbol.empty 

  
  let base_venv = 
    let s = Symbol.symbol "malloc" in 
    Symbol.enter s (Func([Ast.Int], Ast.Int)) Symbol.empty
end

type def_bind = 
  | Vardef | Fundef of bool ref | Strucdef of bool ref * (Symbol.t, Ast.ty) Hashtbl.t

type def_env = def_bind Symbol.table 
type str_env = (Symbol.t, Ast.ty) Hashtbl.t Symbol.table
type var_env = Env.entry Symbol.table 

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
  | Assign(var,e,_) -> check_def_var_exp def_env var; check_def_exp def_env e; empty 
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
      let def_env' = enter id (Strucdef(ref true, tbl)) def_env in
      List.iter (fun (id,t) -> 
                (match t with
                | NameTy(tid) -> (try ignore (lookup tid def_env') with Not_found -> raise(Lack_Definition i))
                | _ -> ()); Hashtbl.add tbl id t)
                fl; enter id tbl (check_def def_env' s) )
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
  | Nil(_) | Void_exp -> ()
and check_def_var_exp def_env = function 
  | SimpVar(id,i) -> 
   (try (match lookup id def_env with 
        | Strucdef(_) -> raise (Type_Var_Misuse i)
        | _ -> ())
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



type exp_and_ty = { exp : Ir_3addr.var; ty : Ast.ty }

let trans_exp : var_env -> str_env -> Ast.exp -> exp_and_ty = 
  fun venv glb_senv -> 
  let check_int : exp_and_ty -> info -> Ir_3addr.var = 
    fun { exp; ty } i -> 
      (match ty with 
       | Ast.Int -> exp 
       | _ -> raise (Ill_Typed i)) in
  let check_bool : exp_and_ty -> info -> Ir_3addr.var = 
    fun { exp; ty } i -> 
      (match ty with 
       | Ast.Bool -> exp 
       | _ -> raise (Ill_Typed i)) in
  let rec trexp = function 
  | Intconst(num,_) -> 
    let t = newtemp() in 
    let () = emit (Ir_3addr.Assign(t, Ir_3addr.Const(num))) in
    { exp = t; ty = Ast.Int }
  | True(_) -> 
    let t = newtemp() in 
    let () = emit (Ir_3addr.Assign(t, Ir_3addr.Const(1))) in
    { exp = t; ty = Ast.Int }
  | False(_) -> 
    let t = newtemp() in 
    let () = emit (Ir_3addr.Assign(t, Ir_3addr.Const(0))) in
    { exp = t; ty = Ast.Int }
  | Bin(e1, Plus, e2, i) ->
    let t1 = check_int (trexp e1) i in
    let t2 = check_int (trexp e2) i in
    let t0 = newtemp() in
    let () = emit (Ir_3addr.Arith(t0, Ir_3addr.Var(t1), Ir_3addr.Plus, Ir_3addr.Var(t2))) in
    { exp = t0; ty = Ast.Int }
  | Bin(e1, Minus, e2, i) ->
    let t1 = check_int (trexp e1) i in
    let t2 = check_int (trexp e2) i in
    let t0 = newtemp() in
    let () = emit (Ir_3addr.Arith(t0, Ir_3addr.Var(t1), Ir_3addr.Minus, Ir_3addr.Var(t2))) in
    { exp = t0; ty = Ast.Int }
  | Bin(e1, Times, e2, i) ->
    let t1 = check_int (trexp e1) i in
    let t2 = check_int (trexp e2) i in
    let t0 = newtemp() in
    let () = emit (Ir_3addr.Arith(t0, Ir_3addr.Var(t1), Ir_3addr.Times, Ir_3addr.Var(t2))) in
    { exp = t0; ty = Ast.Int }
  | Bin(e1, Div, e2, i) ->
    let t1 = check_int (trexp e1) i in
    let t2 = check_int (trexp e2) i in
    let t0 = newtemp() in
    let () = emit (Ir_3addr.Arith(t0, Ir_3addr.Var(t1), Ir_3addr.Div, Ir_3addr.Var(t2))) in
    { exp = t0; ty = Ast.Int }
  | Bin(e1, Lt, e2, i) ->
    let t1 = check_int (trexp e1) i in
    let t2 = check_int (trexp e2) i in
    let t0 = newtemp() in
    let () = emit (Ir_3addr.Rel(t0, Ir_3addr.Var(t1), Ir_3addr.Lt, Ir_3addr.Var(t2))) in
    { exp = t0; ty = Ast.Bool }
  | Bin(e1, Gt, e2, i) -> 
    let t1 = check_int (trexp e1) i in
    let t2 = check_int (trexp e2) i in
    let t0 = newtemp() in
    let () = emit (Ir_3addr.Rel(t0, Ir_3addr.Var(t1), Ir_3addr.Gt, Ir_3addr.Var(t2))) in
    { exp = t0; ty = Ast.Bool }
  | Bin(e1, And, e2, i) ->
    let t1 = check_bool (trexp e1) i in
    let t2 = check_bool (trexp e2) i in
    let t0 = newtemp() in
    let () = emit (Ir_3addr.Rel(t0, Ir_3addr.Var(t1), Ir_3addr.And, Ir_3addr.Var(t2))) in
    { exp = t0; ty = Ast.Bool }
  | Bin(e1, Or, e2, i) ->
    let t1 = check_bool (trexp e1) i in
    let t2 = check_bool (trexp e2) i in
    let t0 = newtemp() in
    let () = emit (Ir_3addr.Rel(t0, Ir_3addr.Var(t1), Ir_3addr.Or, Ir_3addr.Var(t2))) in
    { exp = t0; ty = Ast.Bool }
  | Bin(e1, Eq, e2, i) -> 
    let t1 = check_int (trexp e1) i in
    let t2 = check_int (trexp e2) i in
    let t0 = newtemp() in
    let () = emit (Ir_3addr.Rel(t0, Ir_3addr.Var(t1), Ir_3addr.Eq, Ir_3addr.Var(t2))) in
    { exp = t0; ty = Ast.Bool }
  | Un(_, e, i) -> 
    let t1 = check_bool (trexp e) i in
    let t0 = newtemp() in 
    let () = emit (Ir_3addr.Arith(t0, Ir_3addr.Const(1), Ir_3addr.Minus, Ir_3addr.Var(t1))) in
    { exp = t0; ty = Ast.Bool }
  | App(id, el, i) -> 
    (try match lookup id venv with 
         | Env.Var(ty) -> raise (Not_Function i)
         | Env.Func(tyl, ty) -> 
           (try List.iter2 
                (fun e ty -> 
                let { exp = e; ty = ty'} = trexp e in 
                let () = emit (Ir_3addr.Push(e)) in
                if not (ty = ty') then raise (Ill_Typed i) else ()) 
                el tyl
            with Invalid_argument(_) -> raise (Arity_Mismatched i)); 
            (match ty with 
            | Ast.Void -> { exp = place_holder; ty = ty } 
            | _ -> let t = newtemp () in { exp = t; ty = ty })
    with Not_found -> assert false)
  | ArrayAlloc(ty, e, i) -> (* TODO: get the size of the type *)
    let t = check_int (trexp e) i in
    let t' = newtemp () in
    let () = emit (Ir_3addr.Push t) in (* Problematic *)
    let () = emit (Ir_3addr.Fun_call_ret(Symbol.symbol "malloc", t')) in
    { exp = t'; ty = Ast.ArrayTy(ty) }
  | Alloc(ty, i) -> 
    (match ty with 
    | NameTy(id) as ty -> (try let _ = lookup id glb_senv in 
                               let t = newtemp () in 
                               let t' = newtemp () in
                               let () = emit (Ir_3addr.Assign(t, Ir_3addr.Const(4))) in (* Problematic *)
                               let () = emit (Ir_3addr.Fun_call_ret(Symbol.symbol "malloc", t')) in
                               { exp = t'; ty = ty } 
                    with Not_found -> raise (Lack_Definition i) )
    | _ -> raise (Alloc_Non_Struct i))
  | Nil(_) -> 
    let t = newtemp () in 
    (* Lack something *)
    { exp = t; ty = Ast.Any }
  | Void_exp -> { exp = Temp.place_holder; ty = Ast.Void }
  | Var(var) -> trvar var 
  and trvar = function 
    | SimpVar(id, i) -> 
      (match lookup id venv with 
      | Env.Var(ty) -> { exp = newtemp ~hint:id (); ty = ty }
      | _ -> assert false)
    | FieldVar(var, fname, i) -> 
      let { exp = t; ty = ty } = trvar var in 
      (match ty with 
      | NameTy(id) -> let tbl = lookup id glb_senv in 
                      (match Hashtbl.find_opt tbl fname with 
                      | Some(ty) -> { exp = place_holder; ty = ty } (* Problematic *)
                      | None -> raise (No_Fieldname i))
      | _ -> raise (Not_Struct i))
    | SubscriptVar(var, e, i) -> 
      let { exp = e; ty = ty } = trvar var in 
      (match ty with 
      | ArrayTy(ty) -> { exp = place_holder; ty = ty } (* Problematic *)
      | _ -> raise (Ill_Typed i))
  in 
  fun e -> trexp e

type prop_ret = { ret : bool; ty : Ast.ty }

type exp_and_prop_ret = Translate.exp * prop_ret

(* Problematic *)
let rec trans_stmt : var_env -> str_env -> Ast.stmt -> exp_and_prop_ret = 
  fun venv glb_senv -> 
  let rec trvar = function 
    | SimpVar(id, i) -> 
      (match lookup id venv with 
      | Env.Var(ty) -> { exp = Temp.newtemp ~hint:id (); ty = ty }
      | _ -> assert false)
    | FieldVar(var, fname, i) -> 
      let { exp = t; ty = ty } = trvar var in 
      (match ty with 
      | NameTy(id) -> let tbl = lookup id glb_senv in 
                      (match Hashtbl.find_opt tbl fname with 
                      | Some(ty) -> { exp = Temp.place_holder; ty = ty } (* Problematic *)
                      | None -> raise (No_Fieldname i))
      | _ -> raise (Not_Struct i))
    | SubscriptVar(var, e, i) -> 
      let { exp = e; ty = ty } = trvar var in 
      (match ty with 
      | ArrayTy(ty) -> { exp = Temp.place_holder; ty = ty } (* Problematic *)
      | _ -> raise (Ill_Typed i)) in 
  let rec trstmt : Ast.stmt -> exp_and_prop_ret = function 
  | Assign(var, e, i) -> 
    let { exp ; ty = ty } = trvar var in 
    let { exp = exp'; ty = ty' } = trans_exp venv glb_senv e in 
    (match (ty, ty') with 
    | _ when ty = ty' -> ((), { ret = false; ty = Ast.Void })
    | (NameTy(_), Any) -> ((), { ret = false; ty = Ast.Void})
    | _ -> raise (Ill_Typed i))
  | If(e, s, Some(s'), i) -> 
    let { exp; ty } = trans_exp venv glb_senv e in 
    (match ty with 
    | Ast.Bool -> let (exp, { ret; ty }) = trstmt s in 
                  let (exp', { ret = ret'; ty = ty'} ) = trstmt s' in 
                  if ret && ret' && (ty = ty') then 
                  ((), { ret = true; ty = ty })
                  else ((), { ret = false; ty = Ast.Void })
    | _ -> raise (Ill_Typed i))
  | If(e, s, None, i) -> 
    let { exp; ty } = trans_exp venv glb_senv e in 
    (match ty with 
    | Ast.Bool -> trstmt s 
    | _ -> raise (Ill_Typed i))
  | While(e, s, i) -> 
    let { exp; ty } = trans_exp venv glb_senv e in 
    (match ty with 
    | Ast.Bool -> let (_, { ret = _; ty = _}) = trstmt s in 
                  ((), { ret = false; ty = Ast.Void })
    | _ -> raise (Ill_Typed i))
  | Return(e, i) ->
    let { exp = _; ty = ty } = trans_exp venv glb_senv e in 
    ((), { ret = true; ty = ty }) 
  | Nop -> ((), { ret = false; ty = Ast.Void} )
  | Exp(e, i) -> 
    let { exp = _; ty = _ } = trans_exp venv glb_senv e in 
    ((), { ret = false; ty = Ast.Void })
  | Seq(sl, i) -> 
    List.fold_left
    (fun (exp, { ret; ty }) s -> 
    let (exp', { ret = ret'; ty = ty'}) = trstmt s in 
    ((), (if ret then { ret = true; ty = ty} else { ret = ret'; ty = ty'})) )
    ((), { ret = false; ty = Ast.Void}) sl
  | Vardecl(id, t, s, i) -> 
    trans_stmt (enter id (Env.Var(t)) venv) glb_senv s 
  | Fundecl(id, Arrow(tyl, ty), s, i) -> 
    trans_stmt (enter id (Env.Func(tyl, ty)) venv) glb_senv s
  | Fundefn(id, idl, Arrow(tyl, ty), s', s, i) -> 
    let venv' = (enter id (Env.Func(tyl, ty)) venv) in 
    let venv'' = (try List.fold_left2 
                      (fun acc id ty -> enter id (Env.Var(ty)) acc) venv' idl tyl
                  with Invalid_argument _ -> assert false) in 
    let (_, { ret = ret'; ty = ty'} ) = trans_stmt venv'' glb_senv s' in 
      if not ret' || not (ty = ty') then raise (Not_Proper_Ret i)
      else
      trans_stmt venv' glb_senv s
  | Structdecl(_, s, _) -> 
    trstmt s 
  | Structdefn(_, _, s, _) -> 
    trstmt s 
  | _ -> assert false in 
  fun s -> trstmt s



let check s = 
  let glb_senv = (check_def empty s) in 
  check_init s;
  let (exp, _) = trans_stmt Env.base_venv glb_senv s in exp
