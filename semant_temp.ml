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
  val base_senv : (int * (ty * int) Symbol.table) Symbol.table
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
type str_env = (int * (ty * int) Symbol.table) Symbol.table
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
              enter id (create_struct_entry def_env i fl) (check_def def_env s)
             end
    with Not_found -> 
      let def_env' = enter id (Strucdef(ref true)) def_env in
      enter id (create_struct_entry def_env' i fl) (check_def def_env' s)) 
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

and size_of : ty -> int = 
  function 
    | NameTy(_) | ArrayTy(_) -> 8 
    | Int -> 8 
    | Bool -> 1 
    | _ -> 0

and create_struct_entry : def_env -> info -> (Symbol.t * ty) list -> int * (ty * int) Symbol.table = 
  fun def_env i -> 
    let check_dup = 
      function 
        | NameTy(ty_id) -> 
          begin
            try ignore (lookup ty_id def_env) 
            with Not_found -> raise (Lack_Definition i) 
          end
        | _ -> () in
    let off_set = ref 0 in
    let rec create' : int -> (Symbol.t * ty) list -> (ty * int) Symbol.table = 
      fun align ->
        function 
          | [] -> empty 
          | [fname, ty] -> 
            let () = check_dup ty in
            let size = size_of ty in 
            let real_size = max align size in 
            let () = off_set := !off_set + real_size in 
            enter fname (ty, real_size) empty 
          | (fname, ty) :: ((_, ty') :: _ as tl) -> 
            let () = check_dup ty in
            let size = size_of ty in 
            let size' = size_of ty' in 
            let real_size = max size size' |> max align in 
            let () = off_set := !off_set + real_size in 
            enter fname (ty, real_size) (create' real_size tl) in 
    fun fl -> 
      let tbl = create' 0 fl in 
      (!off_set, tbl)
  

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


