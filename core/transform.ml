module M = Mimple
module Dfa = Dfa
module Ty = Types 
module UF = Cm_util.Union_and_find
module S = Symbol
module T = Temp 
module Bs = Bvset

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
        | _ -> s :: simple_jump_peephole sl


let simplify_func : func -> func = fun func -> 
  { func 
    with func_body = func.func_body
                    |> simplify_func_body

                    |> simple_jump_peephole }





;;



type instrs = M.stmt array 

(* How to achieve this? dfa! 
 * copy propagation needs a slightly tweaked version of it*)
let constant_propagation : instrs -> bool = 
  fun _ -> false

let copy_propagation : instrs -> bool = 
  fun _ -> false

let simplify : M.func -> M.func = fun func -> 
  let instrs = 
    func.func_body
    |> simplify_func_body 
    |> simple_jump_peephole 
    |> Array.of_list in
  let pred, succ = Dfa.calculate_pred_succ instrs in
  let dead_code_elimination lv_res (instrs : instrs) = 
    let flag = ref false in
    Array.iteri
    (fun i stmt -> 
      match stmt with 
        | `Assign(`Temp(t), _) -> 
          begin match List.fold_left (fun acc j -> acc || Bs.mem t lv_res.(j)) false succ.(i) with 
            | false -> instrs.(i) <- `Nop; flag := true
            | true -> () 
          end
        | _ -> ()) instrs; !flag in
  let flag = ref true in 
  while !flag do 
    let () = flag := false in
    let lv_dfa = Dfa.Lv.live_vars func in 
    let lv_res = Dfa.do_dfa lv_dfa pred succ in 
    begin 
      flag := constant_propagation instrs;
      flag := copy_propagation instrs || !flag;
      flag := dead_code_elimination lv_res instrs || !flag 
    end 
  done;
  {func with 
    func_body = Array.to_list instrs}



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
  let cp_res = Dfa.do_dfa cp_dfa pred succ in 
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
  let cop_res = Dfa.do_dfa cop_dfa pred succ in 
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