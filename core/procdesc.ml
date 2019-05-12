

module M = Mimple

module Node = struct 

  type id = int 

  type t = 
  {
    id: id;
    instrs: M.stmt array;
    loc: int; (* Start index in the original procedure *)
    pname: Symbol.t;
    mutable pred: t list;
    mutable succ: t list;
  }

  let compare node1 node2 = Pervasives.compare node1.id node2.id 

  let hash node = Hashtbl.hash node.id 

  let equal node1 node2 = node1.id = node2.id 

  let get_id node = node.id 

  let get_succs node = node.succ 

  let get_preds node = node.pred 

  let get_instrs node = node.instrs
  
  let get_loc node = node.loc 

  let fold_succ : ('acc -> t -> 'acc) -> 'acc -> t -> 'acc = 
    fun f acc node -> 
      List.fold_left f acc node.succ

  let fold_pred : ('acc -> t -> 'acc) -> 'acc -> t -> 'acc = 
    fun f acc node -> 
      List.fold_left f acc node.pred

  let iter_succ : (t -> unit) -> t -> unit = 
    fun f node -> List.iter f node.pred

  let iter_pred : (t -> unit) -> t -> unit = 
    fun f node -> List.iter f node.succ

end 


type t = 
{
  pname: Symbol.t;
  mutable nodes: Node.t list;
  mutable node_num: int;
  mutable start_node: Node.t;
  mutable exit_node: Node.t;
}


let iter : (Node.t -> unit) -> t -> unit = 
  fun f proc ->
    List.iter f proc.nodes

let fold : ('acc -> Node.t -> 'acc) -> 'acc -> t -> 'acc = 
  fun f acc proc -> 
    List.fold_left f acc proc.nodes