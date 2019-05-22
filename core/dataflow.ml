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


type 'abstract_value t = 
{
  proc: Procdesc.t;
  dir: dir_type;
  meet: 'abstract_value -> 'abstract_value -> 'abstract_value;
  equal: 'abstract_value -> 'abstract_value -> bool;
  transfer: Procdesc.Node.t -> 'abstract_value -> 'abstract_value;
  entry_or_exit_facts: 'abstract_value;
  bottom: 'abstract_value;
}

type 'abstract_value result = (Procdesc.Node.id, 'abstract_value) Hashtbl.t

let do_dfa (dfa : 'abstract_value t) : 'abstract_value result = 
  
  let worklist : Procdesc.Node.t Queue.t = Queue.create()

  and res : 'abstract_value result = Hashtbl.create dfa.proc.node_num 

  and fold_pred, iter_succ  = 
    match dfa.dir with 
      | D_Forward -> Procdesc.Node.fold_pred, Procdesc.Node.iter_succ 
      | D_Backward -> Procdesc.Node.fold_succ, Procdesc.Node.iter_pred

  and ( <+> ) : 'abstract_value -> 'abstract_value -> 'abstract_value = dfa.meet 

  and ( <=> ) : 'abstract_value -> 'abstract_value -> bool = dfa.equal in 

  let init () =
    Procdesc.iter 
    (fun node -> Queue.add node worklist; Hashtbl.add res (Procdesc.Node.get_id node) dfa.bottom) dfa.proc 

  and run_worklist () = 
    while not (Queue.is_empty worklist) do 
      let node = Queue.pop worklist in 
      let this_input = 
        fold_pred 
        (fun acc node' ->
        acc <+> Hashtbl.find res (Procdesc.Node.get_id node')) dfa.bottom node in 
      let new_output = dfa.transfer node this_input in 
      match new_output <=> Hashtbl.find res (Procdesc.Node.get_id node) with 
        | true -> () 
        | false -> 
          Hashtbl.replace res (Procdesc.Node.get_id node) new_output;
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

  val gen_bot_and_entry_or_exit_facts: M.func -> abstract_value * abstract_value

  val string_of_result : abstract_value -> string

end

    
module Make
  (X : S) : 
  sig 
    type abstract_value
    val transfer : Procdesc.Node.t -> abstract_value -> abstract_value
    val from_func : M.func -> abstract_value t
    val string_of_result : abstract_value result -> abstract_value t -> string
  end with type abstract_value = X.abstract_value = 
struct 

  type abstract_value = X.abstract_value

  let transfer : Procdesc.Node.t -> abstract_value -> abstract_value = 
    fun node -> 
      let instrs = Procdesc.Node.get_instrs node in 
      if not X.is_backward then
        Array.fold_left
        (fun trs stmt -> 
        X.transfer stmt <-- trs) (fun x -> x) instrs
      else 
        Array.fold_right
        (fun stmt trs -> 
        X.transfer stmt <-- trs) instrs (fun x -> x)

  let from_func (func : M.func) : abstract_value t =
    let bottom, entry_or_exit_facts = X.gen_bot_and_entry_or_exit_facts func 
    and proc = Procdesc.from_func func in
    {
      proc;
      dir = (match X.is_backward with true -> D_Backward | false -> D_Forward);
      meet = X.meet;
      equal = X.equal;
      transfer;
      entry_or_exit_facts;
      bottom;
    }

  let string_of_result : abstract_value result -> abstract_value t -> string = fun res dfa -> 
    Procdesc.fold 
    (fun acc node -> 
      if X.is_backward then 
        begin
          acc
          ^ X.string_of_result (Hashtbl.find res (Procdesc.Node.get_id node))
          ^ "\n"
          ^ Procdesc.Node.string_of_node node
        end
      else 
        begin
          acc
          ^ Procdesc.Node.string_of_node node
          ^ X.string_of_result (Hashtbl.find res (Procdesc.Node.get_id node))
          ^"\n"
        end)
    "" dfa.proc

end 

module LV = Make(Dfa.LiveVariable)

let lv (func : M.func) : LV.abstract_value result = 
  LV.from_func func |> do_dfa

module Anticipate = Make(Dfa.Anticipated)

let print_result (prog : M.prog) : unit = 
(*
  List.iter
  (fun func ->
  let lv_dfa = LV.from_func func in
  let lv_res = do_dfa lv_dfa in 
  print_endline (LV.string_of_result lv_res lv_dfa)) prog
  *)
  List.iter
  (fun func ->
  let ant_dfa = Anticipate.from_func func in
  let ant_res = do_dfa ant_dfa in 
  print_endline (Anticipate.string_of_result ant_res ant_dfa)) prog