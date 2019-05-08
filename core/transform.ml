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