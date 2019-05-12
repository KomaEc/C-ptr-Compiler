module Bs = Bvset
module M = Mimple
module S = Symbol
module T = Temp
module Ty = Types
module P = Cm_util.Printing
module U = Cm_util.Util
open U

type dir_type = 
  | D_Forward 
  | D_Backward

type may_must_type = 
  | K_May 
  | K_Must 


type 'a t = 
{
  proc: Procdesc.t;
  dir: dir_type;
  meet: 'a -> 'a -> 'a;
  equal: 'a -> 'a -> bool;
  transfer: Procdesc.Node.t -> 'a -> 'a;
  entry_or_exit_facts: 'a;
  bottom: 'a;
}

type 'a result = (Procdesc.Node.t, 'a) Hashtbl.t

let do_dfa (dfa : 'a t) : 'a result = 
  
  let worklist : Procdesc.Node.t Queue.t = Queue.create()

  and res : 'a result = Hashtbl.create dfa.proc.node_num 

  and fold_pred, iter_succ  = 
    match dfa.dir with 
      | D_Forward -> Procdesc.Node.fold_pred, Procdesc.Node.iter_succ 
      | D_Backward -> Procdesc.Node.fold_succ, Procdesc.Node.iter_pred

  and ( <+> ) : 'a -> 'a -> 'a = dfa.meet 

  and ( <=> ) : 'a -> 'a -> bool = dfa.equal in 

  let init () =
    Procdesc.iter 
    (fun node -> Queue.add node worklist) dfa.proc 

  and run_worklist () = 
    while not (Queue.is_empty worklist) do 
      let node = Queue.pop worklist in 
      let this_input = 
        fold_pred 
        (fun acc node' ->
        acc <+> Hashtbl.find res node') dfa.bottom node in 
      let new_output = dfa.transfer node this_input in 
      match new_output <=> Hashtbl.find res node with 
        | true -> () 
        | false -> 
          Hashtbl.replace res node new_output;
          iter_succ (fun node' -> Queue.add node' worklist) node
    done in 

  begin
    init ();
    run_worklist ();
    res 
  end


module type S = 
sig 
  type abstract_value

  val is_backward : bool 

  val transfer : M.stmt -> abstract_value -> abstract_value 

  val meet : abstract_value -> abstract_value -> abstract_value 

  val equal : abstract_value -> abstract_value -> bool

  val bottom : abstract_value

  val entry_or_exit_facts : abstract_value

  val gen_bot_and_entry_or_exit_facts: Procdesc.t -> abstract_value * abstract_value


end

    
module Make
  (X : S) = 
struct 

  type abstract_value = X.abstract_value

  let transfer : Procdesc.Node.t -> abstract_value -> abstract_value = 
    fun node -> 
      let instrs = Procdesc.Node.get_instrs node in 
      if X.is_backward then
        Array.fold_left
        (fun trs stmt -> 
        X.transfer stmt <-- trs) (fun x -> x) instrs
      else 
        Array.fold_right
        (fun stmt trs -> 
        X.transfer stmt <-- trs) instrs (fun x -> x)

  let from_proc (proc : Procdesc.t) : abstract_value t = 
    let bottom, entry_or_exit_facts = X.gen_bot_and_entry_or_exit_facts proc in
    {
      proc;
      dir = (match X.is_backward with true -> D_Backward | false -> D_Forward);
      meet = X.meet;
      equal = X.equal;
      transfer;
      entry_or_exit_facts;
      bottom;
    }

end 
