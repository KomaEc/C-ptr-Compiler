module Bs = Bvset
module M = Mimple
module S = Symbol
module T = Temp

type dir_type = 
  | D_Forward 
  | D_Backward

type may_must_type = 
  | K_May 
  | K_Must 

type 'a dfa = {
  instrs : M.stmt array;
  dir : dir_type; (* direction *)
  may_must : may_must_type; (* may -> union or must -> intersection *)
  transfer : int -> 'a Bs.t -> 'a Bs.t; (* Transfer function at position i *)
  entry_or_exit_facts : 'a Bs.t; (* facts assumed at program entry (fwd analysis) or exit (bkwd analysis) *)
  bottom : 'a Bs.t; (* initial sets of facts at all other program points *)
}

type 'a t = 'a dfa

let calculate_pred_succ (instrs : M.stmt array) : int list array * int list array = 
  let length = Array.length instrs in 
  let pred : int list array  = Array.make length [] in 
  let succ = Array.copy pred in 
  let label_to_index = 
    let tbl_ref = ref S.empty in 
    for i = 0 to length - 1 do 
      match instrs.(i) with 
        | `Label(l) -> tbl_ref := S.enter l i !tbl_ref 
        | _ -> () 
    done; !tbl_ref in 
  begin
    Array.iteri 
    (fun i stmt -> 
    match stmt with 
      | `Goto(l) -> 
        let j = S.lookup l label_to_index in 
        pred.(j) <- i :: pred.(j);
        succ.(i) <- j :: succ.(i) 
      | `If(_, l) -> 
        let j = S.lookup l label_to_index in 
        pred.(j) <- i :: pred.(j);
        succ.(i) <- [j];
        if i < length - 1 then
        begin 
          pred.(i+1) <- i :: pred.(i+1);
          succ.(i) <- (i+1) :: succ.(i) 
        end
      | `Ret(_) | `Ret_void -> ()      
      | _ when i < length - 1 -> 
        pred.(i+1) <- i :: pred.(i+1);
        succ.(i) <- [i+1]
      | _ -> ()) instrs
  end; pred, succ



module LiveVariable = struct 

  let rec temps_in_rvalue : M.rvalue -> T.t list = function 
    | `Temp(t) -> [t]
    | `Expr(expr) -> temps_in_expr expr 
    | `Array_ref(i1, i2) -> temps_in_immediate i1 @ temps_in_immediate i2 
    | `Instance_field_ref(i, _) -> temps_in_immediate i 
    | _ -> [] 
  
  and temps_in_expr : M.expr -> T.t list = function 
    | `Bin(i1, _, i2) | `Rel(i1, _, i2) -> 
      temps_in_immediate i1 @ temps_in_immediate i2 
    | `Static_invoke(_, i_list) -> 
      List.fold_left (fun acc i -> temps_in_immediate i @ acc) [] i_list 
    | `New_array_expr(_, i) -> temps_in_immediate i 
    | _ -> [] 

  and temps_in_immediate : M.immediate -> T.t list = 
    fun x -> temps_in_rvalue (x :> M.rvalue)

  and temps_in_var : M.var -> T.t list = 
    fun x -> temps_in_rvalue (x :> M.rvalue)

  and temps_in_condition : M.condition -> T.t list = function 
    | `Temp(t) -> [t] 
    | `Rel(x) -> temps_in_expr (`Rel(x))

  
  let gen : T.t Bs.t -> T.t list -> T.t Bs.t = 
    List.fold_left (fun acc t -> Bs.insert t acc)
  
  let kill : T.t Bs.t -> T.t list -> T.t Bs.t = 
    List.fold_left (fun acc t -> Bs.remove t acc)

  let transfer : M.stmt -> T.t Bs.t -> T.t Bs.t = 
    function
      | `Assign(var, rvalue) -> 
        let tvars = temps_in_var var in 
        let trvs = temps_in_rvalue rvalue in 
        fun pre_fact -> gen (kill pre_fact tvars) trvs
      | `If(cond, _) -> 
        let tconds = temps_in_condition cond in 
        fun pre_fact -> gen pre_fact tconds
      | `Static_invoke(x) -> 
        let texprs = temps_in_expr (`Static_invoke(x)) in
        fun pre_fact -> gen pre_fact texprs
      | `Ret(i) ->
        let tis = temps_in_immediate i in 
        fun pre_fact -> gen pre_fact tis
      | _ -> fun x -> x

end

let live_vars (instrs : M.stmt array) : Temp.t dfa = 
  let open LiveVariable in
  let bvs = Bs.mkempty [] in 
  let transfer_array : (T.t Bs.t -> T.t Bs.t) array = 
    Array.fold_left
    (fun acc stmt -> (transfer stmt) :: acc) [] instrs
    |> List.rev
    |> Array.of_list in
  let transfer = fun i -> transfer_array.(i) in
  {
    instrs = instrs;
    dir = D_Backward;
    may_must = K_May;
    transfer = transfer;
    entry_or_exit_facts = bvs;
    bottom = bvs;
  }


type 'a result = 'a Bs.t array

let do_dfa (_ : 'a dfa) : 'a result = 
  [||]




(* Utility *)


let rec string_of_list : int list -> string = 
  function 
    | [] -> "None" 
    | [x] -> string_of_int x 
    | x :: xs -> string_of_int x ^ ", " ^ string_of_list xs
  
  
let print_stmt_array : M.stmt array -> int list array -> int list array -> unit = 
  fun stmt_array pred succ ->
  Array.iteri
  (fun i stmt -> 
  print_int i; print_string 
  ("\tpred : " ^ (string_of_list pred.(i)) 
  ^ ". succ : " ^ (string_of_list succ.(i)) 
  ^ "\t\t\t" ^ M.string_of_stmt stmt ^ "\n"))
  stmt_array

let analysis_prog : M.prog -> unit = 
  fun prog -> 
  List.iter 
  (fun func -> 
  let instrs = M.(func.func_body) in 
  let instrs = Array.of_list instrs in
  let pred, succ = calculate_pred_succ instrs in 
  print_stmt_array instrs pred succ) prog
  
  
  
