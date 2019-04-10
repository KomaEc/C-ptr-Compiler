
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
exception Null_Reference of info
exception Ignore_Non_Void of info


module type env = sig 

  type ty
  type entry = Local of ty | Parameter of int * ty | Global of ty | Func of ty list * ty 
  (* struct env, maps field name to its type and offset and the total size *)
  val base_senv : (int * (Symbol.t, ty * int) Hashtbl.t) Symbol.table
  (* variable env, maps vars to types *)
  val base_venv : entry Symbol.table
end

module Env : env with type ty = Ast.ty = struct 
  type ty = Ast.ty
  type entry = Local of ty | Parameter of int * ty | Global of ty | Func of ty list * ty 
  let base_senv = Symbol.empty 

  let base_venv = 
    let s = Symbol.symbol "malloc" in 
    Symbol.enter s (Func([Ast.Int], Ast.Int)) Symbol.empty
end

type def_bind = 
  | Vardef | Fundef of bool ref | Strucdef of bool ref * (Symbol.t, Ast.ty * int) Hashtbl.t

type def_env = def_bind Symbol.table 
type str_env = (int * (Symbol.t, Ast.ty * int) Hashtbl.t) Symbol.table
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
                        else begin br := true;
                             let off_set = ref 0 in
                             List.iter (fun (id,t) ->
                                        (match t with 
                                        | NameTy(tid) -> (try ignore (lookup tid def_env) with Not_found -> raise (Lack_Definition i))
                                        | _ -> ()); Hashtbl.add tbl id (t, !off_set); incr off_set;)
                             fl; enter id (!off_set + 1, tbl) (check_def def_env s) end
    with Not_found -> 
      let tbl = Hashtbl.create 20 in 
      let def_env' = enter id (Strucdef(ref true, tbl)) def_env in
      let off_set = ref 0 in
      List.iter (fun (id,t) -> 
                (match t with
                | NameTy(tid) -> (try ignore (lookup tid def_env') with Not_found -> raise(Lack_Definition i))
                | _ -> ()); Hashtbl.add tbl id (t, !off_set); incr off_set;)
                fl; enter id (!off_set + 1, tbl) (check_def def_env' s) )
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


(* calculate the offset of array. 
 * for struct, 8 bytes per field. The offset is in glb_senv *)


let size_of_ty : str_env -> ty -> int = 
  fun glb_senv -> function 
  | NameTy(tid) -> 8(* ?? should be an address! lookup tid glb_senv |> fst |> ( * ) 8 *)
  | ArrayTy(_) -> 8 (* size of <type>[] equals to the size of a loc *)
  | Int -> 4 
  | Bool -> 1
  | _ -> -1






let rec trans_stmt : var_env -> str_env -> stmt -> unit = 
  fun venv glb_senv -> 

  let module TranslateHelper = 
    struct
      type result_of_trans_exp = 
        | Ex of Mimple.rvalue 
        | Cx of (Temp.label -> Temp.label -> Mimple.rvalue)
    end in


  let rec trvar : var -> Mimple.var * ty * int = function 
    | SimpVar(id, i) -> 
      begin 
        match lookup id venv with 
          | Env.Local(ty) ->  
            let t = newtemp ~hint:id () in 
            (`Temp(t), ty, size_of_ty glb_senv ty)
          | Env.Parameter(pos, ty) -> 
            let t = newtemp () in 
            let () = emit (`Identity(`Temp(t), `Parameter_ref(pos))) in 
            (`Temp(t), ty, size_of_ty glb_senv ty) 
          | Env.Global(ty) -> 
            (`Static_field_ref(id), ty, size_of_ty glb_senv ty) 
          | _ -> assert false
      end 
    | FieldVar(var, fname, i) -> 
      let (var, ty, _) = trvar var in 
      let size = 
        begin 
          match ty with 
            | NameTy(tid) ->
              let (_, tbl) = lookup tid glb_senv in 
              begin
                try Hashtbl.find tbl fname |> snd 
                with Not_found -> raise (No_Fieldname i) 
              end 
            | _ -> raise (Not_Struct i) 
        end in 
      begin 
        match var with 
          | `Temp(t) -> (`Instance_field_ref(`Temp(t), fname), ty, size)
          | _ as var -> 
            let t' = newtemp () in 
            let () = emit (`Assign(`Temp(t'), Mimple.var_to_rvalue var)) in 
            (`Temp(t'), ty, size)
      end 
    | SubscriptVar(var, expr, i) ->  
      let (var, ty, size) = trvar var in 
      let ty = begin 
                 match ty with 
                   | ArrayTy(ty) -> ty 
                   | _ -> raise (Ill_Typed i)
               end in
      let size = size_of_ty glb_senv ty in
      let (rvalue, ty') = trexpr expr in 
      if not (ty' = Int) then raise (Ill_Typed i)
      else
      let t = (* tempory that stores var *)
        begin
          match var with 
            | `Temp(t) -> t 
            | _ as var -> 
              let t' = newtemp () in 
              let () = emit (`Assign(`Temp(t'), Mimple.var_to_rvalue var)) in 
              t'
        end in 
      let inter = construct_result rvalue in 
      (`Array_ref(`Temp(t), inter), ty, size)

  and construct_result : Mimple.rvalue -> Mimple.intermediate = function 
    | `Temp(t) -> `Temp(t) 
    | `Const(c) -> `Const(c) 
    | _ as rvalue -> 
      let t = newtemp () in 
      let () = emit (`Assign(`Temp(t), rvalue)) in 
      `Temp(t) 
  
  and check_int : exp -> Mimple.rvalue = fun e -> 
    let (t, ty) = trexpr e in 
    begin 
      match ty with 
        | Int -> t 
        | _ -> raise (Ill_Typed (extract_info_exp e))
    end
  
  and check_bool : exp -> Mimple.rvalue = fun e -> 
    let (t, ty) = trexpr e in 
    begin 
      match ty with 
        | Bool -> t 
        | _ -> raise (Ill_Typed (extract_info_exp e))
    end
  
  and construct_int_bin_int : exp -> binop -> exp -> info -> Mimple.rvalue * ty = fun expr1 bop expr2 i -> 
    let t1 = check_int expr1 |> construct_result in 
    let t2 = check_int expr2 |> construct_result in 
    let op = 
      begin 
        match bop with 
          | Plus -> `Plus
          | Minus -> `Minus
          | Times -> `Times
          | Div -> `Div
          | _ -> assert false
      end in 
    (`Expr(`Bin(t1, op, t2)), Int)

  and construct_int_bin_bool : exp -> binop -> exp -> info -> Mimple.rvalue * ty = fun expr1 bop expr2 i -> 
    let t1 = check_int expr1 |> construct_result in 
    let t2 = check_int expr2 |> construct_result in 
    let op = 
      begin 
        match bop with 
          | Lt -> `Lt 
          | Gt -> `Gt 
          | _ -> assert false
      end in 
    (`Expr(`Rel(t1, op, t2)), Bool)

  and construct_bool_bin_bool : exp -> binop -> exp -> info -> Mimple.rvalue * ty = fun expr1 bop expr2 i -> 
    let t1 = check_bool expr1 |> construct_result in 
    let t2 = check_bool expr2 |> construct_result in 
    let op = 
      begin 
        match bop with 
          | And -> `And 
          | Or -> `Or
          | _ -> assert false
      end in 
    (`Expr(`Rel(t1, op, t2)), Bool)

  and trexpr : exp -> Mimple.rvalue * ty = function 
    | Intconst(num, i) -> (`Const(`Int_const(num)), Int) 
    | True(i) -> (`Const(`Int_const(1)), Bool) 
    | False(i) -> (`Const(`Int_const(0)), Bool) 
    | Var(var) -> 
      let (var, ty, _) = trvar var in 
      (Mimple.var_to_rvalue var, ty)
    | Bin(expr1, Plus, expr2, i) -> 
      construct_int_bin_int expr1 Div expr2 i
    | Bin(expr1, Minus, expr2, i) -> 
      construct_int_bin_int expr1 Div expr2 i
    | Bin(expr1, Times, expr2, i) -> 
      construct_int_bin_int expr1 Div expr2 i
    | Bin(expr1, Div, expr2, i) -> 
      construct_int_bin_int expr1 Div expr2 i
    | Bin(expr1, Lt, expr2, i) -> 
      construct_int_bin_bool expr1 Lt expr2 i 
    | Bin(expr1, Gt, expr2, i) -> 
      construct_int_bin_bool expr1 Lt expr2 i 
    | Bin(expr1, Eq, expr2, i) -> 
      let (i1, ty1) = trexpr expr1 in 
      let (i2, ty2) = trexpr expr2 in 
        if not (ty1 = ty2) then raise (Ill_Typed i)
        else let t1 = construct_result i1 in 
             let t2 = construct_result i2 in 
              (`Expr(`Rel(t1, `Eq, t2)), Bool) 
    | Bin(expr1, bop, expr2, i) -> 
      construct_bool_bin_bool expr1 bop expr2 i 
    | Un(_, expr, i) -> 
      let rvalue = check_bool expr in 
      let t = construct_result rvalue in 
      (`Expr(`Bin(`Const(`Int_const(1)), `Minus, t)), Bool)
    | App(id, expr_list, i) -> 
      let (rval_list, ty_list) = List.map trexpr expr_list |> List.split in 
      let t_list = List.map construct_result rval_list in 
      let l = newlabel ~hint:id () in
      begin 
        match lookup id venv with 
          | Env.Func(ty_list', ty) -> 
            begin
              try begin 
                List.iter2 
                  (fun ty' ty -> if ty' = ty then () else raise (Ill_Typed i)) 
                    ty_list' ty_list;
                begin 
                  match ty with 
                    | Void -> 
                      let () = emit (`Static_invoke(l, t_list)) in 
                      (`Temp(place_holder), Void)
                    | _ as ty -> 
                      (`Expr(`Static_invoke(l, t_list)), ty)
                end
              end
              with Invalid_argument _ -> raise (Arity_Mismatched i)
            end  
          | _ -> raise (Not_Function i)
      end 
    | ArrayAlloc(ty, expr, i) -> 
      let t = check_int expr |> construct_result in 
      let size = size_of_ty glb_senv ty in 
      let t' = newtemp () in 
      let () = emit (`Assign(`Temp(t'), `Expr(`Bin(t, `Times, `Const(`Int_const(size)))))) in 
      (`Expr(`Alloc(`Temp(t'))), ArrayTy(ty))
    | Alloc(ty, i) -> 
      let size = size_of_ty glb_senv ty in 
      (`Expr(`Alloc(`Const(`Int_const(size)))), ty)
    | Nil(i) -> 
      (`Const(`Null_const), Any)
    | Void_exp -> 
      (`Temp(place_holder), Void)

  and result_of_var : Mimple.var -> [ `Temp of Temp.t ] = function 
    | `Temp(t) -> `Temp(t) 
    | _ as var -> 
      let t = newtemp () in 
      let () = emit (`Assign(`Temp(t), Mimple.var_to_rvalue var)) in 
      `Temp(t)

  and cond_trexp : exp -> Temp.label -> Temp.label -> unit = fun exp lt lf -> 
    begin 
      match exp with 
        | True(_) -> emit (`Goto lt) 
        | False(_) -> emit (`Goto lf)
        | Var(v) -> 
          let (var, ty, _) = trvar v in 
          begin
            match ty with 
              | Bool -> 
                let `Temp(t) = result_of_var var in 
                emit (`If(`Temp(t), lt));
                emit (`Goto(lf))
              | _ -> raise (Ill_Typed (extract_info_var v))
          end
        | Bin(expr1, And, expr2, _) -> 
          let cond1 = cond_trexp expr1 in 
          let cond2 = cond_trexp expr2 in 
          let l = newlabel () in 
          cond1 l lf;
          let () = emit (`Label(l)) in
          (* Note that we must emit here 
           * The side effect is enlosed by a closure! *)
          cond2 lt lf 
        | Bin(expr1, Or, expr2, _) -> 
          let cond1 = cond_trexp expr1 in 
          let cond2 = cond_trexp expr2 in 
          let l = newlabel () in 
          cond1 lt l;
          let () = emit (`Label(l)) in
          cond2 lt lf 
        | Bin(expr1, Lt, expr2, _) -> 
          let t1 = check_int expr1 |> construct_result in 
          let t2 = check_int expr2 |> construct_result in 
          let () = emit (`If(`Rel(t1, `Lt, t2), lt)) in 
          emit (`Goto lf)
        | Bin(expr1, Gt, expr2, _) -> 
          let t1 = check_int expr1 |> construct_result in 
          let t2 = check_int expr2 |> construct_result in 
          let () = emit (`If(`Rel(t1, `Gt, t2), lt)) in 
          emit (`Goto lf)
        | Bin(expr1, Eq, expr2, i) -> 
          let (i1, ty1) = trexpr expr1 in 
          let (i2, ty2) = trexpr expr2 in 
          if not (ty1 = ty2) then raise (Ill_Typed i)
          else let t1 = construct_result i1 in 
               let t2 = construct_result i2 in 
               let () = emit (`If(`Rel(t1, `Gt, t2), lt)) in 
               emit (`Goto lf)
        | Un(_, expr, _) -> 
          cond_trexp expr lf lt 
        | App(id, expr_list, i) -> 
            let (rval_list, ty_list) = List.map trexpr expr_list |> List.split in 
            let t_list = List.map construct_result rval_list in 
            let l = newlabel ~hint:id () in
            begin 
              match lookup id venv with 
                | Env.Func(ty_list', ty) -> 
                  begin
                    try 
                      begin 
                        List.iter2 
                          (fun ty' ty -> if ty' = ty then () else raise (Ill_Typed i)) 
                            ty_list' ty_list;
                        begin 
                          match ty with 
                            | Bool -> 
                              let t = newtemp() in 
                              let () = emit (`Assign(`Temp(t), `Expr(`Static_invoke(l, t_list)))) in 
                              let () = emit (`If(`Temp(t), lt)) in 
                              emit (`Goto lf)
                            | _ -> raise (Ill_Typed i)
                        end
                      end
                    with Invalid_argument _ -> raise (Arity_Mismatched i)
                  end  
                | _ -> raise (Not_Function i)
            end 
        | _ -> raise (Ill_Typed (extract_info_exp exp))
          
    end 
  
  (* TODO : check proper return *)
  and trstmt : stmt -> unit = function 
    | Assign(var, expr, i) -> 
      let (var, ty, _) = trvar var in 
      let (rvalue, ty') = trexpr expr in 
      begin 
        match ty = ty' with 
          | true -> 
            emit (`Assign(var, rvalue)) 
          | _ -> raise (Ill_Typed i)
      end 
    | If(expr, s, Some(s'), i) -> 
      let cond = cond_trexp expr in 
      let l1 = newlabel () in 
      let l2 = newlabel () in 
      let () = cond l1 l2 in 
      let () = emit (`Label(l1)) in 
      let () = trstmt s in 
      let () = emit (`Label(l2)) in 
      trstmt s
    | While(expr, s, i) -> 
      let cond = cond_trexp expr in 
      let l1 = newlabel () in
      let l2 = newlabel () in 
      let l3 = newlabel () in 
      let () = emit (`Label(l1)) in 
      let () = cond l2 l3 in 
      let () = emit (`Label(l2)) in 
      let () = trstmt s in 
      emit (`Label(l3))
    | Return(Void_exp, _) -> 
      emit `Ret_void 
    | Return(expr, i) ->
      let (rvalue, _) = trexpr expr in 
      emit (`Ret(construct_result rvalue))
    | Exp(expr, i) -> 
      let (rvalue, ty) = trexpr expr in 
      begin 
        match ty with 
          | Void -> () 
          | _ -> raise (Ignore_Non_Void i)
      end
    | Seq(stmt_list, i) ->
      List.iter trstmt stmt_list 
    | _ -> () 

  in trstmt
      

      
    

      
    
(*



(* TODO : change the result type into Mimple.var *)
and trans_var : var_env -> str_env -> var -> [ `Temp of Temp.t ] * ty = 
  fun venv glb_senv -> 
    let rec trvar : var -> [ `Temp of Temp.t ] * ty * int = function 
    | SimpVar(id, i) -> 
      (match lookup id venv with 
      | Env.Var(ty) -> let t = newtemp ~hint:id () in 
                       (`Temp(t), ty, size_of_ty glb_senv ty)
      | _ -> assert false)
    | FieldVar(var, id, i) -> 
      let (t, ty, _) = trvar var in 
      let size = (match ty with 
              | NameTy(tid) -> 
                let (_, tbl) = lookup tid glb_senv in 
                (try Hashtbl.find tbl id |> snd
                with Not_found -> raise (No_Fieldname i))
              | _ -> raise (Not_Struct i)) in
      let t' = `Temp(newtemp ()) in 
      let () = emit (`Assign(t', `Instance_field_ref(t, id))) in 
      (t', ty, size) (* Should've return the field directly *)
    | SubscriptVar(var, e, i) -> (* Java-style array, don't need to turn it 
                                  * into one-dimensional array *)
      let (t, ty, size) = trvar var in 
      let (t', ty') = trans_exp venv glb_senv e in
      if not (ty' = Int) then raise (Ill_Typed i)
      else
      let t'' = newtemp () in 
      let () = emit (`Assign(`Temp(t''), `Expr(`Bin(t', `Times, `Const(`Int_const(size)))))) in
      (`Temp(place_holder), Void, -1) (* Modify this *)
    in fun var -> let (t, ty, _) = trvar var in (t, ty)

and trans_exp : var_env -> str_env -> exp -> Mimple.intermediate * ty = 
  fun venv glb_senv -> 
  let rec check_int : exp -> Mimple.intermediate = fun e -> 
    let (t, ty) = trexp e in 
    (match ty with 
      Int -> t 
    | _ -> raise (Ill_Typed (extract_info_exp e))) 
  
  and check_bool : exp -> Mimple.intermediate = fun e -> 
    let (t, ty) = trexp e in 
    (match ty with 
      Bool -> t 
    | _ -> raise (Ill_Typed (extract_info_exp e))) 

(* Change return type to deal with conditional expr
 * maybe return Mimple.rvalue instead? *)
  and trexp : exp -> Mimple.intermediate * ty = function 
  | Intconst(num, _) -> (`Const(`Int_const(num)), Int) 
  | True(_) -> (`Const(`Int_const(1)), Bool)
  | False(_) -> (`Const(`Int_const(0)), Bool)
  | Var(var) -> let (`Temp(t), ty) = trans_var venv glb_senv var in 
                (`Temp(t), ty) 
  | Bin(e1, Plus, e2, i) -> 
    let t1 = check_int e1 in 
    let t2 = check_int e2 in 
    let t3 = newtemp () in 
    let () = emit (`Assign(`Temp(t3), `Expr(`Bin(t1, `Plus, t2)))) in 
    (`Temp(t3), Int)
  | Bin(e1, Minus, e2, i) -> 
    let t1 = check_int e1 in 
    let t2 = check_int e2 in 
    let t3 = newtemp () in 
    let () = emit (`Assign(`Temp(t3), `Expr(`Bin(t1, `Minus, t2)))) in 
    (`Temp(t3), Int)
  | Bin(e1, Times, e2, i) -> 
    let t1 = check_int e1 in 
    let t2 = check_int e2 in 
    let t3 = newtemp () in 
    let () = emit (`Assign(`Temp(t3), `Expr(`Bin(t1, `Times, t2)))) in 
    (`Temp(t3), Int)
  | Bin(e1, Div, e2, i) -> 
    let t1 = check_int e1 in 
    let t2 = check_int e2 in 
    let t3 = newtemp () in 
    let () = emit (`Assign(`Temp(t3), `Expr(`Bin(t1, `Div, t2)))) in 
    (`Temp(t3), Int)
  | Bin(e1, Lt, e2, i) -> 
    let t1 = check_int e1 in 
    let t2 = check_int e2 in 
    let t3 = newtemp () in 
    let () = emit (`Assign(`Temp(t3), `Expr(`Rel(t1, `Lt, t2)))) in 
    (`Temp(t3), Bool)
  | Bin(e1, Gt, e2, i) -> 
    let t1 = check_int e1 in 
    let t2 = check_int e2 in 
    let t3 = newtemp () in 
    let () = emit (`Assign(`Temp(t3), `Expr(`Rel(t1, `Gt, t2)))) in 
    (`Temp(t3), Bool)
  | Bin(e1, And, e2, i) -> 
    let t1 = check_bool e1 in 
    let t2 = check_bool e2 in 
    let t3 = newtemp () in 
    let () = emit (`Assign(`Temp(t3), `Expr(`Rel(t1, `And, t2)))) in 
    (`Temp(t3), Bool)
  | Bin(e1, Eq, e2, i) -> 
    let (t1, ty1) = trexp e1 in 
    let (t2, ty2) = trexp e2 in 
    (match ty1 = ty2 with 
    | true -> (* polish here *)
      let t3 = newtemp () in
      let () = emit (`Assign(`Temp(t3), `Expr(`Rel(t1, `Eq, t2)))) in 
      (`Temp(t3), Bool)
    | _ -> raise (Ill_Typed i))
  | Un(_, e, i) -> 
    let t = check_bool e in 
    let t' = newtemp () in 
    let () = emit (`Assign(`Temp(t'), `Expr(`Bin(`Const(`Int_const 1), `Minus, t)))) in 
    (`Temp(t'), Bool)
  | App(id, el, i) -> 
    (try match lookup id venv with 
         | Env.Var(ty) -> raise (Not_Function i)
         | Env.Func(tyl, ty) -> 
           let tl = 
           (try List.map2 
                (fun e ty -> 
                let (t, ty') = trexp e in 
                if not (ty = ty') then raise (Ill_Typed i) else t) 
                el tyl
            with Invalid_argument(_) -> raise (Arity_Mismatched i)) in
            (match ty with 
            | Ast.Void ->  
              let l = newlabel ~hint:id () in 
              let () = emit (`Static_invoke(l, tl)) in 
              (`Temp(place_holder), Void)
            | _ -> 
              let l = newlabel ~hint:id () in 
              let t0 = newtemp () in 
              let () = emit (`Assign(`Temp(t0), `Expr(`Static_invoke(l, tl)))) in 
              (`Temp(t0), ty))
    with Not_found -> assert false)
  | ArrayAlloc(ty, e, i) -> 
    let t = check_int e in
    let t' = newtemp () in
    let size = size_of_ty glb_senv ty in 
    let () = emit (`Assign(`Temp(t'), `Expr(`Bin(`Const(`Int_const(size)), `Times, t)))) in 
    let t'' = newtemp () in 
    let () = emit (`Assign(`Temp(t''), `Expr(`Alloc(`Temp(t'))))) in 
    (`Temp(t''), ArrayTy(ty))
  | Alloc(ty, i) -> 
    let () = (match ty with 
    | NameTy(_) -> () 
    | _ -> raise (Alloc_Non_Struct i)) in
    let size = size_of_ty glb_senv ty in 
    let t = newtemp () in 
    let () = emit (`Assign(`Temp(t), `Expr(`Alloc(`Const(`Int_const(size)))))) in 
    (`Temp(t), ty)
  | Nil(i) -> (`Const(`Null_const), Any)
  | _ -> (`Const(`Null_const), Void)
  in trexp 

*)
      







(*


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
      | NameTy(id) -> let (size, tbl) = lookup id glb_senv in 
                      (match Hashtbl.find_opt tbl fname with 
                      | Some(ty, _) -> { exp = place_holder; ty = ty } (* Problematic *)
                      | None -> raise (No_Fieldname i))
      | Any -> raise (Null_Reference i)
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
      | NameTy(id) -> let (size, tbl) = lookup id glb_senv in 
                      (match Hashtbl.find_opt tbl fname with 
                      | Some(ty, _) -> { exp = Temp.place_holder; ty = ty } (* Problematic *)
                      | None -> raise (No_Fieldname i))
      | Any -> raise (Null_Reference i)
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


*)






let check s = 
  let glb_senv = (check_def empty s) in 
  check_init s
  (*let (exp, _) = trans_stmt Env.base_venv glb_senv s in exp*)
