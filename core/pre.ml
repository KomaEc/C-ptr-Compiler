
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

let augment (proc : Procdesc.t) : Procdesc.t = 
  let module Set = Set.Make(struct type t = Procdesc.Node.t let compare node1 node2 = compare (Procdesc.Node.get_id node1) (Procdesc.Node.get_id node2) end) in
  let new_nodes_ref = ref Set.empty in
  Procdesc.iter 
    (fun node -> 
      if Procdesc.Node.is_internal node then 
        begin 
          let node_set =
            Procdesc.Node.fold_succ
              (fun acc node' -> if List.length (Procdesc.Node.get_preds node') >= 2 then Set.add node' acc else acc
                ) Set.empty node in 
          Procdesc.Node.iter_succ
            (fun node' -> 
              begin
                match Set.find_opt node' node_set with 
                  | None -> () 
                  | Some _ -> 
                    let node'' = Procdesc.Node.make [||] (-10000) proc.pname in 
                    node.succ <- List.filter (fun node''' -> (Procdesc.Node.get_id node''') <> (Procdesc.Node.get_id node')) node.succ;
                    node'.pred <- List.filter (fun node''' -> (Procdesc.Node.get_id node''') <> (Procdesc.Node.get_id node)) node'.pred;
                    node.succ <- node'' :: node.succ;
                    node'.pred <- node'' :: node'.pred;
                    node''.succ <- [node'];
                    node''.pred <- [node];
                    new_nodes_ref := Set.add node'' !new_nodes_ref
              end) node
        end) proc; 
        let new_ones = Set.elements !new_nodes_ref in
        {
          proc with 
          nodes = new_ones @ proc.nodes;
          node_num =  proc.node_num + List.length new_ones;
        }


module Make 
  (X : S) = 
struct 
  type abstract_value = X.abstract_value

  type proc_info = {
    proc : Procdesc.t;
    all : abstract_value;
    empty : abstract_value;
  }

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


  let kill_trans : Procdesc.Node.t -> abstract_value -> abstract_value = 
    lift `Is_Forward X.e_kill

  let gen_trans : Procdesc.Node.t -> abstract_value -> abstract_value = 
    lift `Is_Forward X.e_gen


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

  let func_to_proc_info : M.func -> proc_info = 
    fun func -> 
      let proc = Procdesc.from_func_singleton func
      and (all, empty) = X.gen_all_and_empty func in
      {
        proc;
        all;
        empty;
      }

  type transfer = Procdesc.Node.t -> abstract_value -> abstract_value

  let config_dfa : proc_info -> [`Is_Backward | `Is_Forward ] -> [`May | `Must] -> transfer -> abstract_value Dataflow.t = 
    fun proc_info dir may_must transfer ->
      {
        proc = proc_info.proc;
        dir = (match dir with `Is_Backward -> Dataflow.D_Backward | `Is_Forward -> Dataflow.D_Forward);
        meet = (match may_must with `May -> X.inter | `Must -> X.union);
        equal = X.equal;
        transfer;
        entry_or_exit_facts = proc_info.empty;
        bottom = (match may_must with `May -> proc_info.empty | `Must -> proc_info.all);
      }

  let do_dfa (dfa : 'a Dataflow.t) : 'a Dataflow.result * 'a Dataflow.result = 
    let (res1, res2) = Dataflow.do_dfa dfa in 
      match dfa.dir with 
        | D_Forward -> (res1, res2)
        | D_Backward -> (res2, res1)
  (* result : result of in and out *)


  let run (func : M.func) : unit = 
    let proc_info = func_to_proc_info func in 

    let new_proc = augment proc_info.proc in

    let proc_info = {
      proc_info with proc = new_proc;
    } in
(*
    let kill = Hashtbl.create 16 in 

    let () = Procdesc.iter (fun node -> Hashtbl.add use (Procdesc.Node.get_id node) (gen_trans node proc_info.empty)) proc_info.proc in
*)
    let e_use = Hashtbl.create 16 in

    let () = Procdesc.iter (fun node -> Hashtbl.add e_use (Procdesc.Node.get_id node) (gen_trans node proc_info.empty)) proc_info.proc in

    let node_wise_op : (abstract_value -> abstract_value -> abstract_value) -> abstract_value Dataflow.result -> abstract_value Dataflow.result -> abstract_value Dataflow.result = 
    begin
    fun (<+>) res1 res2 -> 
      let newres = Hashtbl.create 16 in
      Procdesc.iter 
        (fun node -> 
          let id = Procdesc.Node.get_id node in
          let val1, val2 = Hashtbl.find res1 id, Hashtbl.find res2 id in 
          Hashtbl.add newres id (val1 <+> val2)) proc_info.proc ; newres end in

    let ( || ) = node_wise_op X.union in 
    let ( && ) = node_wise_op X.inter in 
    let ( - ) = node_wise_op X.diff in  
    let ( ! ) : abstract_value Dataflow.result -> abstract_value Dataflow.result = 
      begin
      fun res -> 
      let new_res = Hashtbl.create 16 in
        Procdesc.iter 
          (fun node -> 
            let id = Procdesc.Node.get_id node in
            let val1 = Hashtbl.find res id in 
            Hashtbl.add new_res id (X.diff proc_info.all val1)) proc_info.proc; new_res 
      end in
    


    let step1_dfa = config_dfa proc_info `Is_Backward `Must step1_transfer in 
    let (anti_in, _) = do_dfa step1_dfa in 
    let step2_transfer = make_step2_transfer anti_in in 
    let step2_dfa = config_dfa proc_info `Is_Forward `Must step2_transfer in
    let (aval_in, _) = do_dfa step2_dfa in 
    let earliest = anti_in - aval_in in
    let step3_transfer = make_step3_transfer earliest in
    let step3_dfa = config_dfa proc_info `Is_Forward `Must step3_transfer in 
    let (postponable_in, _) = do_dfa step3_dfa in

    let fold_succ_inter_inner_union res1 res2 = 
      begin
        let new_res = Hashtbl.create 16 in 
        Procdesc.iter
          (fun node -> 
            let id = Procdesc.Node.get_id node in 
            let new_val = Procdesc.Node.fold_succ
              (fun acc node' -> 
                let val_e = Hashtbl.find res1 (Procdesc.Node.get_id node') in 
                let val_p = Hashtbl.find res2 (Procdesc.Node.get_id node') in
                X.inter acc (X.union val_e val_p)) proc_info.all node in 
            Hashtbl.add new_res id new_val) proc_info.proc;
        new_res
      end in  
    (*
    let some_val : abstract_value Dataflow.result =
      begin
        let new_res = Hashtbl.create 16 in 
        Procdesc.iter
          (fun node -> 
            let id = Procdesc.Node.get_id node in 
            let new_val = Procdesc.Node.fold_succ
              (fun acc node' -> 
                let val_e = Hashtbl.find earliest (Procdesc.Node.get_id node') in 
                let val_p = Hashtbl.find postponable_in (Procdesc.Node.get_id node') in
                X.inter acc (X.union val_e val_p)) proc_info.all node in 
            Hashtbl.add new_res id new_val) proc_info.proc;
        new_res 
      end in
      *)
    let (<&>) = fold_succ_inter_inner_union in
    let latest = 
      (earliest || postponable_in) 
      && (e_use || !(earliest <&> postponable_in)) in
    let step4_transfer = make_step4_transfer latest in
    let step4_dfa = config_dfa proc_info `Is_Backward `May step4_transfer in
    let (_, _) = do_dfa step4_dfa in

    () 
  

end



module Test = 
struct 
  let from_prog (p : M.prog) = 
    List.iter 
      (fun func -> 
        Procdesc.from_func func 
        |> augment 
        |> Procdesc.string_of_proc
        |> print_endline) p
end