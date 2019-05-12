

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

  let dummy : t = 
  {
    id = -1;
    instrs = [||];
    loc = -1;
    pname = Symbol.symbol "dummy";
    pred = [];
    succ = [];
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

  let make id instrs loc pname = 
  {
    id;
    instrs;
    loc;
    pname;
    pred = [];
    succ = [];
  }

end 


type t = 
{
  pname: Symbol.t;
  locals: (Temp.t * Types.ty) list;
  formals: (Temp.t * Types.ty) list;
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


let from_func (func: M.func) : t = 
  let locals = M.get_locals func 
  and formals = M.get_formals func 
  and instrs = Array.of_list func.func_body in
  let convert_to_lnum () = 
    let tbl = Hashtbl.create 16 in 
    let offset = ref 0 in 
    Array.iteri 
    (fun i -> function 
    | `Label(l) when i > 0 -> 
      Hashtbl.add tbl l (i - !offset - 1);
      incr offset 
    | _ -> ()) instrs;
    let aux = Array.make (Array.length instrs - !offset - 1) `Nop in
    offset := 0;
    Array.iteri
    (fun i -> function 
    | _ when i = 0 -> () 
    | `Label(_) -> () 
    | `Goto(`Label(l)) -> 
      aux.(!offset) <- (`Goto(`Line_num(Hashtbl.find tbl l)));
      incr offset 
    | `If(cond, `Label(l)) -> 
      aux.(!offset) <- (`If(cond, `Line_num(Hashtbl.find tbl l)));
      incr offset
    | _ as stmt -> 
      aux.(!offset) <- stmt;
      incr offset) instrs;
    aux in 
  let instrs = convert_to_lnum() 
  and id = ref (-1) in 
  let length = Array.length instrs
  and is_leader = Array.make (Array.length instrs) false in
  Array.iteri 
  (fun i -> function 
    | _ when i = 0 -> is_leader.(i) <- true 
    | `Goto(`Line_num(j)) | `If(_, `Line_num(j)) -> 
      is_leader.(j) <- true;
      if i < length - 1 then is_leader.(i+1) <- true
    | _ -> ()) instrs;
  let nodes_ref = ref [] 
  and () = id := 0 
  and l = ref 0 in
  for r = 1 to length - 1 do 
    if is_leader.(r) then 
    let newinstrs = Array.make (r - !l) `Nop in 
    Array.blit instrs !l newinstrs 0 (r - !l);
    let node = Node.make !id newinstrs !l func.func_name in 
    nodes_ref := node :: !nodes_ref
  done;
  let nodes = !nodes_ref |> List.rev in 
  {
    pname = func.func_name;
    locals;
    formals;
    nodes;
    node_num = List.length nodes;
    start_node = Node.dummy;
    exit_node = Node.dummy;
  }