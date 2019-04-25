module Bs = Bvset
module M = Mimple

type dir_type = 
  | D_Forward 
  | D_Backward

type may_must_type = 
  | K_May 
  | K_Must 

type 'a t = {
  instrs : M.stmt array;
  dir : dir_type; (* direction *)
  may_must : may_must_type; (* may or must *)
  gen_kill : int -> ('a Bs.t * 'a Bs.t); (* (gen, kill) *)
  entry_or_exit_facts : 'a Bs.t; (* facts assumed at program entry (fwd analysis) or exit (bkwd analysis) *)
  bottom : 'a Bs.t (* initial sets of facts at all other program points *)
}

let live_vars (instrs : M.stmt array) : int t = 
  let bvs = Bs.mkempty [] in 
  {
    instrs = instrs;
    dir = D_Backward;
    may_must = K_May;
    gen_kill = (fun _ -> bvs, bvs);
    entry_or_exit_facts = bvs;
    bottom = bvs
  }

