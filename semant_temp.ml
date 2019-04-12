open Ast
open Ast.Util
open Symbol
open Support.Error
open Translate
open Temp

module M = Mimple_temp

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
exception Glb_Const of info


module type env = sig 

  type ty
  type entry = Local of ty | Parameter of int * ty | Global of ty | Func of ty list * ty 
  (* struct env, maps field name to its type and offset and the total size *)
  val base_senv : (Symbol.t * ty) list Symbol.table
  (* variable env, maps vars to types *)
  val base_venv : entry Symbol.table
end

module Env : env with type ty = Ast.ty = struct 
  type ty = Ast.ty
  type entry = Local of ty | Parameter of int * ty | Global of ty | Func of ty list * ty 
  let base_senv = Symbol.empty 

  let base_venv = Symbol.empty
end


type def_bind = 
  | Vardef | Fundef of bool ref | Strucdef of bool ref

type def_env = def_bind Symbol.table 
type str_env = (Symbol.t * ty) list Symbol.table
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
      let res = check_def (enter id (Strucdef(br)) def_env) s in
      (if !br then () else raise (Lack_Definition i));
      res
    )
  | Structdefn(id,fl,s,i) -> 
    (try let d = lookup id def_env in 
         match d with 
         | Vardef -> raise (Duplicated_Definition i) 
         | Fundef(_) -> raise (Duplicated_Definition i)
         | Strucdef (br) -> 
           if !br then raise (Duplicated_Definition i)
           else 
             begin
              br := true;
              check_struct_entry def_env i fl;
              enter id fl (check_def def_env s)
             end
    with Not_found -> 
      let def_env' = enter id (Strucdef(ref true)) def_env in
      let () = check_struct_entry def_env' i fl in
      enter id fl (check_def def_env' s)) 
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

and check_struct_entry : def_env -> info -> (Symbol.t * ty) list -> unit = 
  fun def_env i -> 
    let check_dup = 
      function 
        | NameTy(ty_id) -> 
          begin
            try ignore (lookup ty_id def_env) 
            with Not_found -> raise (Lack_Definition i) 
          end
        | _ -> () in
    fun fl -> 
      List.iter
        (fun (id, ty) -> check_dup ty) fl
  

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



type status = Local | Global 


let rec trans_stmt : status -> var_env -> str_env -> stmt -> unit = 
  fun st venv glb_senv -> 

  let rec var_to_immediate : ty -> M.var -> M.immediate = fun ty ->
    function
      | `Temp(t) -> `Temp(t) 
      | _ as var ->
        let t = newtemp () in 
        let () = emit_local_def t ty in 
        let () = emit_stmt (`Assign(`Temp(t), M.var_to_rvalue var)) in
        `Temp(t)

  and rvalue_to_immediate : ty -> M.rvalue -> M.immediate = fun ty ->
    function 
      | `Temp(t) -> `Temp(t) 
      | `Const(c) -> `Const(c) 
      | _ as rvalue -> 
        let t = newtemp () in 
        let () = emit_local_def t ty in 
        let () = emit_stmt (`Assign(`Temp(t), rvalue)) in 
        `Temp(t)

  and var_to_rvalue : M.var -> M.rvalue = 
    function 
    | `Temp(t) -> `Temp(t) 
    | `Array_ref(t, i) -> `Array_ref(t, i)
    | `Instance_field_ref(t, fsig) -> `Instance_field_ref(t, fsig)
    | `Static_field_ref(id) -> `Static_field_ref(id)

  and var_to_local : ty -> M.var -> Temp.t = fun ty ->
    function 
      | `Temp(t) -> t 
      | _ as var -> 
        let t = newtemp () in 
        let () = emit_local_def t ty in 
        let () = emit_stmt (`Assign(`Temp(t), var_to_rvalue var)) in 
        t

  and assert_const : info -> M.rvalue -> M.const = fun i ->
    function 
      | `Const(c) -> c 
      | _ -> raise (Glb_Const i)

  and check_int : exp -> M.rvalue = fun e -> 
    let (t, ty) = trexpr e in 
    begin 
      match ty with 
        | Int -> t 
        | _ -> raise (Ill_Typed (extract_info_exp e))
    end
  
  and check_bool : exp -> M.rvalue = fun e -> 
    let (t, ty) = trexpr e in 
    begin 
      match ty with 
        | Bool -> t 
        | _ -> raise (Ill_Typed (extract_info_exp e))
    end

  and construct_int_bin_int : exp -> binop -> exp -> info -> M.rvalue * ty = fun expr1 bop expr2 i -> 
    let t1 = check_int expr1 |> rvalue_to_immediate Int in 
    let t2 = check_int expr2 |> rvalue_to_immediate Int in 
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

  and construct_int_bin_bool : exp -> binop -> exp -> info -> M.rvalue * ty = fun expr1 bop expr2 i -> 
    let t1 = check_int expr1 |> rvalue_to_immediate Int in 
    let t2 = check_int expr2 |> rvalue_to_immediate Int in 
    let op = 
      begin 
        match bop with 
          | Lt -> `Lt
          | Gt -> `Gt
          | _ -> assert false
      end in 
    (`Expr(`Rel(t1, op, t2)), Bool)
  
  and construct_bool_bin_bool : exp -> binop -> exp -> info -> M.rvalue * ty = fun expr1 bop expr2 i -> 
    let t1 = check_bool expr1 |> rvalue_to_immediate Bool in 
    let t2 = check_bool expr2 |> rvalue_to_immediate Bool in 
    let op = 
      begin 
        match bop with 
          | And -> `And 
          | Or -> `Or
          | _ -> assert false
      end in 
    (`Expr(`Rel(t1, op, t2)), Bool)

  and trvar : var -> M.var * ty = 
    function 
      | SimpVar(id, info) -> 
        begin 
          match lookup id venv with 
            | Env.Local(ty) ->
              let t = newtemp ~hint:id () in 
              (* don't emit declaration, since it must've been defined before *)
              (`Temp(t), ty) 
            | Env.Parameter(pos, ty) -> 
              let t = newtemp () in 
              let () = emit_local_def t ty in 
              let () = emit_stmt (`Identity(`Temp(t), `Parameter_ref(pos))) in 
              (`Temp(t), ty)
            | Env.Global(ty) -> 
              (`Static_field_ref(id), ty)
            | _ -> assert false
        end
      | FieldVar(var, fname, info) -> 
        let (var, ty) = trvar var in 
        let ty' = 
          begin
            match ty with 
              | NameTy(ty_id) -> 
                let tbl = lookup ty_id glb_senv in 
                begin 
                  try List.assoc fname tbl
                  with Not_found -> raise (No_Fieldname info)
                end
              | _ -> raise (Not_Struct info)
          end in 
        let t = var_to_immediate ty var in 
        (`Instance_field_ref(t, (fname, type_convert ty')), ty')
      | SubscriptVar(var, expr, info) -> 
        let (var, ty) = trvar var in 
        let ty' = 
          begin 
            match ty with 
              | ArrayTy(ty) -> ty 
              | _ -> raise (Ill_Typed info)
          end in
        let t = var_to_immediate ty var in
        let imm = check_int expr |> rvalue_to_immediate Int in 
        (`Array_ref(t, imm), ty')

  and trexpr : exp -> M.rvalue * ty = 
    function 
      | Intconst(num, i) -> (`Const(`Int_const(num)), Int) 
      | True(i) -> (`Const(`Int_const(1)), Bool) 
      | False(i) -> (`Const(`Int_const(0)), Bool) 
      | Var(var) -> 
        let (var, ty) = trvar var in 
        (var_to_rvalue var, ty)
      | Bin(expr1, Plus, expr2, i) -> 
        construct_int_bin_int expr1 Plus expr2 i
      | Bin(expr1, Minus, expr2, i) -> 
        construct_int_bin_int expr1 Minus expr2 i
      | Bin(expr1, Times, expr2, i) -> 
        construct_int_bin_int expr1 Times expr2 i
      | Bin(expr1, Div, expr2, i) -> 
        construct_int_bin_int expr1 Div expr2 i
      | Bin(expr1, Lt, expr2, i) -> 
        construct_int_bin_bool expr1 Lt expr2 i 
      | Bin(expr1, Gt, expr2, i) -> 
        construct_int_bin_bool expr1 Gt expr2 i 
      | Bin(expr1, Eq, expr2, i) -> 
        let (i1, ty1) = trexpr expr1 in 
        let (i2, ty2) = trexpr expr2 in 
          if not (ty1 = ty2) then raise (Ill_Typed i)
          else let t1 = rvalue_to_immediate ty1 i1 in 
               let t2 = rvalue_to_immediate ty2 i2 in 
                (`Expr(`Rel(t1, `Eq, t2)), Bool) 
      | Bin(expr1, bop, expr2, i) -> 
        construct_bool_bin_bool expr1 bop expr2 i 
      | Un(_, expr, i) -> 
        let t = check_bool expr |> rvalue_to_immediate Bool in 
        (`Expr(`Bin(`Const(`Int_const(1)), `Minus, t)), Bool) 
      | App(id, expr_list, i) -> 
        let (rval_list, ty_list) = List.map trexpr expr_list |> List.split in  
        let l = newlabel ~hint:id () in
        begin (* Please check again *)
          match lookup id venv with 
            | Env.Func(ty_list', ty) -> 
              begin
                try begin 
                  List.iter2 
                    (fun ty' ty -> if ty' = ty then () else raise (Ill_Typed i)) 
                      ty_list' ty_list;
                  let ty_list'' = List.map type_convert ty_list in 
                  let t_list = List.map2 rvalue_to_immediate ty_list rval_list in
                  begin 
                    match ty with 
                      | Void -> 
                        let () = 
                          emit_stmt (`Static_invoke((l, ty_list'', Primitive(`Void)), t_list)) in 
                        (`Temp(place_holder), Void)
                      | _ as ty -> 
                        (`Expr(`Static_invoke((l, ty_list'', type_convert ty), t_list)), ty)
                  end
                end
                with Invalid_argument _ -> raise (Arity_Mismatched i)
              end  
            | _ -> raise (Not_Function i)
        end 
      | ArrayAlloc(ty, expr, i) -> 
        let im = check_int expr |> rvalue_to_immediate Int in 
        let ty' = 
          begin
            try type_convert ty
            with Invalid_argument _ -> failwith "Multi-dimensional array not yet implemented"
          end in 
        (`Expr(`New_array_expr(ty', im)), ArrayTy(ty))
      | Alloc(ty, i) -> 
        begin
          match ty with 
            | NameTy(ty_id) -> 
              (`Expr(`New_expr(`ClassTy(ty_id))), ty)
            | _ -> raise (Alloc_Non_Struct i)
        end
      | Nil(i) -> 
        (`Const(`Null_const), Any)
      | Void_exp -> 
        (`Temp(place_holder), Void)

    and cond_trexp : exp -> Temp.label -> Temp.label -> unit = fun exp lt lf -> 
      match exp with 
        | True(_) -> emit_stmt (`Goto lt) 
        | False(_) -> emit_stmt (`Goto lf) 
        | Var(var') -> 
          let (var, ty) = trvar var' in 
          begin 
            match ty with 
              | Bool -> 
                let t = var_to_local Bool var in 
                let () = emit_stmt (`If(`Temp(t), lt)) in 
                emit_stmt (`Goto(lf))
              | _ -> raise (Ill_Typed (extract_info_var var'))
          end 
        | Bin(expr1, And, expr2, _) -> 
          let cond1 = cond_trexp expr1 in 
          let cond2 = cond_trexp expr2 in 
          let l = newlabel () in 
          cond1 l lf;
          let () = emit_stmt (`Label(l)) in
          (* Note that we must emit here 
           * The side effect is enlosed by a closure! *)
          cond2 lt lf 
        | Bin(expr1, Or, expr2, _) -> 
          let cond1 = cond_trexp expr1 in 
          let cond2 = cond_trexp expr2 in 
          let l = newlabel () in 
          cond1 lt l;
          let () = emit_stmt (`Label(l)) in
          cond2 lt lf 
        | Bin(expr1, Lt, expr2, _) -> 
          let t1 = check_int expr1 |> rvalue_to_immediate Int in 
          let t2 = check_int expr2 |> rvalue_to_immediate Int in 
          let () = emit_stmt (`If(`Rel(t1, `Lt, t2), lt)) in 
          emit_stmt (`Goto lf)
        | Bin(expr1, Gt, expr2, _) -> 
          let t1 = check_int expr1 |> rvalue_to_immediate Int in 
          let t2 = check_int expr2 |> rvalue_to_immediate Int in 
          let () = emit_stmt (`If(`Rel(t1, `Gt, t2), lt)) in 
          emit_stmt (`Goto lf)
        | Bin(expr1, Eq, expr2, i) -> 
          let (i1, ty1) = trexpr expr1 in 
          let (i2, ty2) = trexpr expr2 in 
          if not (ty1 = ty2) then raise (Ill_Typed i)
          else let t1 = rvalue_to_immediate ty1 i1 in 
               let t2 = rvalue_to_immediate ty2 i2 in 
               let () = emit_stmt (`If(`Rel(t1, `Gt, t2), lt)) in 
               emit_stmt (`Goto lf)
        | Un(_, expr, _) -> 
          cond_trexp expr lf lt 
        | App(id, expr_list, i) -> 
          let (rval_list, ty_list) = List.map trexpr expr_list |> List.split in  
          let l = newlabel ~hint:id () in
          begin (* Please check again *)
            match lookup id venv with 
              | Env.Func(ty_list', ty) -> 
                begin
                  try begin 
                    List.iter2 
                      (fun ty' ty -> if ty' = ty then () else raise (Ill_Typed i)) 
                        ty_list' ty_list;
                    let ty_list'' = List.map type_convert ty_list in 
                    let t_list = List.map2 rvalue_to_immediate ty_list rval_list in
                    begin 
                      match ty with 
                        | Bool -> 
                          let t = newtemp () in 
                          let () = emit_local_def t Bool in 
                          let () = emit_stmt (`Assign(`Temp(t), `Expr(`Static_invoke((l, ty_list'', type_convert ty), t_list)))) in 
                          let () = emit_stmt (`If(`Temp(t), lt)) in 
                          emit_stmt (`Goto lf)
                        | _ -> raise (Ill_Typed i)
                    end
                  end
                  with Invalid_argument _ -> raise (Arity_Mismatched i)
                end  
              | _ -> raise (Not_Function i)
          end 
        | _ -> raise (Ill_Typed (extract_info_exp exp)) 

  and trstmt : stmt -> unit = 
    function 
      | Assign(SimpVar(id, _), expr, i) when st = Global ->
        let c = trexpr expr |> fst |> assert_const i in 
        assign_glb_vars id (`Const(c))
      | Assign(var', expr, i) -> 
        let (var, ty) = trvar var' in 
        let (rvalue, ty') = trexpr expr in 
        begin 
          match (ty, ty') with 
            | _ when ty = ty' -> 
              emit_stmt (`Assign(var, rvalue)) 
            | (NameTy(ty_id), Any) -> 
              emit_stmt (`Assign(var, rvalue)) 
            | _ -> raise (Ill_Typed i)
        end
      | If(expr, s, Some(s'), i) -> 
        let cond = cond_trexp expr in 
        let l1 = newlabel () in 
        let l2 = newlabel () in
        let () = cond l1 l2 in 
        let () = emit_stmt (`Label(l1)) in 
        let () = trstmt s in 
        let () = emit_stmt (`Label(l2)) in 
        trstmt s 
      | While(expr, s, i) -> 
        let cond = cond_trexp expr in 
        let l1 = newlabel () in 
        let l2 = newlabel () in 
        let l3 = newlabel () in 
        let () = emit_stmt (`Label(l1)) in 
        let () = cond l2 l3 in
        let () = emit_stmt (`Label(l2)) in 
        let () = trstmt s in 
        let () = emit_stmt (`Goto(l1)) in 
        emit_stmt (`Label(l3))
      | Return(Void_exp, _) -> 
        emit_stmt `Ret_void 
      | Return(expr, i) -> 
        let (rvalue, ty) = trexpr expr in 
        emit_stmt (`Ret(rvalue_to_immediate ty rvalue))
      | Exp(expr, i) -> 
        let (rvalue, ty) = trexpr expr in 
        begin
          match ty with
            | Void -> () 
            | _ -> raise (Ignore_Non_Void i)
        end
      | Seq(stmt_list, i) -> 
        List.iter trstmt stmt_list
      | Vardecl(id, ty, s, i) -> 
        begin
          match st with 
            | Local -> 
              let venv' = enter id (Env.Local(ty)) venv in 
              trans_stmt Local venv' glb_senv s 
            | Global -> 
              let () = add_glb_vars id ty in
              (* Currently, global array is not allowed *)
              let venv' = enter id (Env.Global(ty)) venv in 
              trans_stmt Global venv' glb_senv s
          end
      | Fundecl(id, Arrow(ty_list, ty), s, i) -> 
        let venv' = enter id (Env.Func(ty_list, ty)) venv in 
        trans_stmt Global venv' glb_senv s 
      | Fundefn(id, id_list, Arrow(ty_list, ty), inner_s, s, i) -> 
        let l = newlabel ~hint:id () in 
        let () = emit_stmt (`Label(l)) in 
        let entry_list_ref = ref [] in
        let () = 
          List.iteri
            (fun i ty -> entry_list_ref := Env.Parameter(i, ty) :: !entry_list_ref)
              ty_list in 
        let entry_list = List.rev !entry_list_ref in
        let venv' = enter id (Env.Func(ty_list, ty)) venv in 
        let venv'' = 
          List.fold_left2
            (fun acc id entry -> enter id entry acc)
              venv' id_list entry_list in 
        let () = trans_stmt Local venv'' glb_senv inner_s in
        let () = end_function () in 
        trans_stmt Global venv' glb_senv s
      | Structdecl(_, s, _) -> 
        trstmt s 
      | Structdefn(_, _, s, _) -> 
        trstmt s
          
      | _ -> ()
  in trstmt

let check s = 
  let glb_senv =(check_def empty s) in 
  let () = check_init s in 
  let () = trans_stmt Global empty glb_senv s in 
  get_mimple ()