
module M = Mimple
module T = Temp
module U = Cm_util.Util
open U

module DfaExpr = Dfa.DfaExpr

module type S = 
sig
  type abstract_value

  type expr = [
      `Bin of M.immediate * M.binop * M.immediate
    | `Rel of M.immediate * M.relop * M.immediate
  ]

  val union : abstract_value -> abstract_value -> abstract_value

  val inter : abstract_value -> abstract_value -> abstract_value

  val diff : abstract_value -> abstract_value -> abstract_value

  val equal : abstract_value -> abstract_value -> bool

  val e_kill : M.stmt -> abstract_value -> abstract_value 

  val e_gen : M.stmt -> abstract_value -> abstract_value

  val gen_all_and_empty : M.func -> abstract_value * abstract_value
end


module Make 
  (X : S) = 
struct 
  type abstract_value = X.abstract_value

  let anticipated_transfer : M.stmt -> abstract_value -> abstract_value = 
    fun s -> (X.e_gen s) <-- (X.e_kill s)

  let lift : [`Is_Backward | `Is_Forward ] -> (M.stmt -> abstract_value -> abstract_value) -> Procdesc.Node.t -> abstract_value -> abstract_value = 
    fun dir transfer node ->
      let instrs = Procdesc.Node.get_instrs node in 
      if dir = `Is_Forward then
        Array.fold_left
        (fun trs stmt -> 
        transfer stmt <-- trs) (fun x -> x) instrs
      else 
        Array.fold_right
        (fun stmt trs -> 
        transfer stmt <-- trs) instrs (fun x -> x) 


  (* Step 1, backward analysis to ge anticipated expressions *)
  let step1_transfer : Procdesc.Node.t -> abstract_value -> abstract_value = 
    lift `Is_Backward anticipated_transfer


  let make_step2_transfer : abstract_value Dataflow.result -> Procdesc.Node.t -> abstract_value -> abstract_value = 
    fun anti_in node value -> 
      X.union (Hashtbl.find anti_in (Procdesc.Node.get_id node)) value |> (lift `Is_Forward X.e_kill) node

  let make_step3_transfer : abstract_value Dataflow.result -> Procdesc.Node.t -> abstract_value -> abstract_value = 
    fun earliest node value -> 
      X.union (Hashtbl.find earliest (Procdesc.Node.get_id node)) value |> (lift `Is_Forward X.e_kill) node

  let make_step4_transfer : abstract_value Dataflow.result -> Procdesc.Node.t -> abstract_value -> abstract_value = 
    fun latest node value -> 
      X.diff (X.union ((lift `Is_Backward X.e_gen) node value) value) (Hashtbl.find latest (Procdesc.Node.get_id node))

  

end

