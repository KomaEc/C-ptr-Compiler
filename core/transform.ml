module M = Mimple
module Dfa = Dfa
module Ty = Types 
module UF = Cm_util.Union_and_find
module S = Symbol
module T = Temp 
module Bs = Bvset
module U = Cm_util.Util
open U

open M 



let simplify_func_body : stmt list -> stmt list = fun stmt_list ->
  let label_to_point : S.t UF.point S.table = 
    List.fold_left 
    (fun prev stmt ->
    match stmt with 
      | `Label(l) -> S.enter l (UF.fresh l) prev
      | _ -> prev) S.empty stmt_list in
  let rec dedup : stmt list -> stmt list = 
    function 
      | [] -> []
      | [s] -> [s]
      | s :: (s' :: _ as sl) ->
        begin 
          match s, s' with 
          (* [!!!] Problematic, since this label can be function 
           * Consider reveal the label type in Temp ??? *)
          (* [!!!] Problematic!!!! Can [`Label l] and 
           * [`Goto l'] really be pruned?????? 
           * I think so. But what if there's a jump
           * -to-nowhere "goto" ?? Cyclic? *)
            | `Label l, `Label l' | `Label l, `Goto (`Label l') ->
              UF.union (S.lookup l label_to_point) 
              (S.lookup l' label_to_point);
              dedup sl
            | _ -> s :: dedup sl
        end in
  let get_repr : label -> label = fun l ->
    let point = S.lookup l label_to_point in 
    UF.find point in
  let substitute : stmt -> stmt = 
    function 
      | `If(cond, `Label(l)) -> `If(cond, `Label (get_repr l))
      | `Goto(`Label(l)) -> `Goto(`Label(get_repr l))
      | `Label(l) -> `Label(get_repr l)
      | _ as s -> s in
  List.map substitute (dedup stmt_list)

let rec simple_jump_peephole : stmt list -> stmt list =
    function 
    | [] -> [] 
    | [s] -> [s] 
    | s :: (s' :: sl' as sl) -> 
      match s, s' with 
        | `Goto(`Label(l)), `Label(l') when l = l' -> simple_jump_peephole sl' 
        | `Goto(_ as l), `Goto(_) -> (`Goto(l)) :: simple_jump_peephole sl' 
        | _ -> s :: simple_jump_peephole sl


let simplify_func : func -> func = fun func -> 
  { func 
    with func_body = func.func_body
                    |> simplify_func_body

                    |> simple_jump_peephole }





;;



type instrs = M.stmt array 

let transform_stmt_cp (to_var : Temp.t -> immediate) : stmt -> stmt * bool = 
  let flag = ref false in

  let rec transform_stmt : stmt -> stmt = 
    function 
      | `Assign(var, rvalue) -> 
        begin 
          match var with 
            | `Temp(t) -> 
              begin 
                match to_var t with 
                  | `Const(c) -> flag := true; `Assign(var, `Const(c))
                  | _ -> `Assign(transform_var var, transform_rvalue rvalue)
              end 
            | _ as var -> `Assign(transform_var var, transform_rvalue rvalue)
        end
      | `Static_invoke(msig, il) -> 
        `Static_invoke(msig, List.map transform_immediate il) 
      | `Ret(i) -> `Ret(transform_immediate i)
      | _ as s -> s 
  and transform_var = 
    function 
      | `Array_ref(i, i') -> `Array_ref(i, transform_immediate i') 
      | _ as var -> var
  and transform_rvalue  = 
    function 
      | `Temp(t) -> (transform_immediate (`Temp(t)) :> rvalue)
      | `Expr(expr) -> `Expr(transform_expr expr) 
      | `Array_ref(i, i') -> `Array_ref(i, transform_immediate i')
      | _ as v -> v 
  and transform_expr = 
    function 
      | `Bin(i1, op, i2) -> 
        `Bin(transform_immediate i1, op, transform_immediate i2) 
      | `Rel(i1, op, i2) -> 
        `Rel(transform_immediate i1, op, transform_immediate i2) 
      | `Static_invoke(msig, il) -> 
        `Static_invoke(msig, List.map transform_immediate il) 
      | `New_array_expr(ty, i) -> `New_array_expr(ty, transform_immediate i) 
      | _ as e -> e 
  and transform_immediate = 
    function 
      | `Temp(t) -> 
        begin
          match to_var t with 
            | `Const(_) as c -> flag := true; c 
            | _ as t -> t 
        end
      | _ as c -> c in 

    fun stmt -> transform_stmt stmt, !flag

let transform_stmt_cop (to_var : Temp.t -> immediate) : stmt -> stmt * bool = 
  let flag = ref false in

  let rec transform_stmt : stmt -> stmt = 
    function 
      | `Assign(var, rvalue) -> 
        `Assign(transform_var var, transform_rvalue rvalue)
      | `Static_invoke(msig, il) -> 
        `Static_invoke(msig, List.map transform_immediate il) 
      | `Ret(i) -> `Ret(transform_immediate i)
      | _ as s -> s 
  and transform_var = 
    function 
      | `Array_ref(i, i') -> `Array_ref(transform_immediate i, transform_immediate i') 
      | `Instance_field_ref(i, fsig) -> `Instance_field_ref(transform_immediate i, fsig)
      | _ as var -> var
  and transform_rvalue  = 
    function 
      | `Temp(t) -> (transform_immediate (`Temp(t)) :> rvalue)
      | `Expr(expr) -> `Expr(transform_expr expr) 
      | `Array_ref(i, i') -> `Array_ref(transform_immediate i, transform_immediate i')
      | _ as v -> v 
  and transform_expr = 
    function 
      | `Bin(i1, op, i2) -> 
        `Bin(transform_immediate i1, op, transform_immediate i2) 
      | `Rel(i1, op, i2) -> 
        `Rel(transform_immediate i1, op, transform_immediate i2) 
      | `Static_invoke(msig, il) -> 
        `Static_invoke(msig, List.map transform_immediate il) 
      | `New_array_expr(ty, i) -> `New_array_expr(ty, transform_immediate i) 
      | _ as e -> e 
  and transform_immediate = 
    function 
      | `Temp(t) -> 
        begin
          match to_var t with 
            | `Temp(t') when not (t = t') -> flag := true; `Temp(t') 
            | _ -> `Temp(t)
        end
      | _ as c -> c in 

    fun stmt -> transform_stmt stmt, !flag


let constant_propagation : M.func -> M.func * bool = fun func -> 
  let cp_dfa = Dfa.Cp.constant_propagation func in
  let pred, succ = Dfa.calculate_pred_succ cp_dfa.instrs in
  let cp_res = Dfa.do_dfa cp_dfa pred succ |> fst in 
  let to_var i = fun t -> 
    match FiniteMap.find cp_res.(i) t with 
      | Dfa.Cp.Const(c) -> 
        `Const(c) 
      | _ -> `Temp(t) in
  let res_func = ref [] in 
  let flag = ref false in
  Array.iteri 
  (fun i stmt -> 
  match transform_stmt_cp (to_var i) stmt with 
    (stmt, b) -> flag := !flag || b; res_func := stmt :: !res_func) cp_dfa.instrs;
  {func with func_body = List.rev !res_func}, !flag

let copy_propagation : M.func -> M.func * bool = fun func -> 
  let cop_dfa = Dfa.Cop.copy_propagation func in 
  let pred, succ = Dfa.calculate_pred_succ cop_dfa.instrs in 
  let cop_res = Dfa.do_dfa cop_dfa pred succ |> snd in 
  let to_var i = fun t -> 
    match FiniteMap.find cop_res.(i) t with 
      | `Copy(t') -> `Temp(t') 
      | _ -> `Temp(t) in 
  let res_func = ref [] in 
  let flag = ref false in 
  Array.iteri 
  (fun i stmt -> 
  match transform_stmt_cop (to_var i) stmt with 
    (stmt, b) -> flag := !flag || b; res_func := stmt :: !res_func) cop_dfa.instrs;
  {func with func_body = List.rev ! res_func}, !flag


let simplify_func2 : M.func -> M.func = 
  fun func -> fst (constant_propagation func)

let simplify_func3 : M.func -> M.func = 
  fun func -> fst (copy_propagation func)


let is_effectless : M.rvalue -> bool = function 
  | `Temp(_) 
  | `Const(_) 
  | `Array_ref(_) 
  | `Expr(`Bin(_)) 
  | `Expr(`Rel(_)) -> true 
  | _ -> false
  

let dead_code_elimination : M.func -> M.func * bool = fun func -> 
  let lv_dfa = Dfa.Lv.live_vars func in 
  let pred, succ = Dfa.calculate_pred_succ lv_dfa.instrs in 
  let lv_res = Dfa.do_dfa lv_dfa pred succ |> snd in 
  let instrs = 
    func.func_body 
    |> Array.of_list in 
  let flag = ref false in
  let () = Array.iteri 
    (fun i -> function 
    | `Assign(`Temp(t), rvalue) 
      when not (Bvset.mem t lv_res.(i)) && is_effectless rvalue -> 
      flag := true; instrs.(i) <- `Nop 
    | _ -> ()) instrs in 
  { func with 
    func_body = Array.to_list instrs }, !flag

let compact : M.func -> M.func = fun func -> 
  { func with 
    func_body = List.fold_right 
    (fun stmt acc -> 
    match stmt with 
      | `Nop -> acc 
      | _ as s -> s :: acc) func.func_body []}

(*
let rec optimize : M.func -> M.func = fun func -> 
  let (func, flag1) = constant_propagation func in 
  let (func, flag2) = copy_propagation func in 
  let (func, flag3) = dead_code_elimination func in 
  if flag1 || flag2 || flag3 then optimize func 
  else compact func

*)

(*  ALERT : cp and cop have wrong semantics. 
 * 'cause the transfer functions only gen don't kill ! *)

let rec optimize : M.func -> M.func = 
  let aux transfer (func, flag) = 
    let (func', flag') = transfer func in 
      (func', flag || flag') in 
  fun func -> 
    let (func, flag) = 
      aux constant_propagation (func, false) 
      |> aux copy_propagation   
      |> aux dead_code_elimination in 
    (if flag then optimize <-- compact
    else compact) func