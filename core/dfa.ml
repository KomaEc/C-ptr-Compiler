module Bs = Bvset
module M = Mimple
module S = Symbol

type dir_type = 
  | D_Forward 
  | D_Backward

type may_must_type = 
  | K_May 
  | K_Must 

type 'a dfa = {
  instrs : M.stmt array;
  dir : dir_type; (* direction *)
  may_must : may_must_type; (* may or must *)
  gen_kill : int -> ('a Bs.t * 'a Bs.t); (* (gen, kill) *)
  entry_or_exit_facts : 'a Bs.t; (* facts assumed at program entry (fwd analysis) or exit (bkwd analysis) *)
  bottom : 'a Bs.t (* initial sets of facts at all other program points *)
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
      | _ when i < length - 1 -> 
        pred.(i+1) <- i :: pred.(i+1);
        succ.(i) <- [i+1]
      | _ -> ()) instrs
  end; pred, succ





let live_vars (instrs : M.stmt array) : Temp.t dfa = 
  let bvs = Bs.mkempty [] in 
  {
    instrs = instrs;
    dir = D_Backward;
    may_must = K_May;
    gen_kill = (fun _ -> bvs, bvs);
    entry_or_exit_facts = bvs;
    bottom = bvs
  }


type 'a result = 'a Bs.t array

let pred (instrs : M.stmt array) : int -> int list = 
  let pred, _ = calculate_pred_succ instrs in 
  Array.unsafe_get pred

let succ (instrs : M.stmt array) : int -> int list = 
  let _, succ = calculate_pred_succ instrs in 
  Array.unsafe_get succ

let do_dfa (_ : 'a dfa) : 'a result = 
  [||]

