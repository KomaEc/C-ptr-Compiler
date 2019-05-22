
module P = Cm_util.Printing
module M = Mimple
open P

module Node = struct 

  type id = int 


  type t = {
    id: id;
    instrs: M.stmt array;
    loc: int; (* Start index in the original procedure *)
    pname: Symbol.t;
    mutable pred: t list;
    mutable succ: t list;
  } 

  let make_exit (pname : Symbol.t) : t =
  {
    id = -2;
    instrs = [||];
    loc = -2;
    pname;
    pred = [];
    succ = [];
  }

  let make_entry (pname : Symbol.t) : t =
  {
    id = -1;
    instrs = [||];
    loc = -1;
    pname;
    pred = [];
    succ = [];
  }

  let is_exit : t -> bool = fun node -> node.id = -2
  
  let is_entry : t -> bool = fun node -> node.id = -1

  let is_internal : t -> bool = fun node -> not (is_entry node) && not (is_exit node)


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
    fun f node -> List.iter f node.succ

  let iter_pred : (t -> unit) -> t -> unit = 
    fun f node -> List.iter f node.pred

  let make id instrs loc pname = 
  {
    id;
    instrs;
    loc;
    pname;
    pred = [];
    succ = [];
  }

  let string_of_node : t -> string = fun node -> 
    let res = ref ("=====Start node " ^ string_of_int node.id ^ "=====\n") in
    res := !res ^ "preds : " ^ (string_of_list string_of_int (List.map get_id node.pred)) ^ "\n";
    Array.iteri 
    (fun i stmt -> 
    res := !res ^ "line " ^ string_of_int (i + node.loc) ^ 
    M.string_of_stmt stmt ^ "\n") node.instrs; 
    res := !res ^ "succs : " ^ (string_of_list string_of_int (List.map get_id node.succ));
    res := !res ^ "\n======End node " ^ string_of_int node.id ^ "=====\n\n";
    !res

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
  let instrs = convert_to_lnum() in
  let length = Array.length instrs in
  let is_leader = Array.make (length + 1) false in
  is_leader.(length) <- true;
  Array.iteri 
  (fun i -> function 
    | _ when i = 0 -> is_leader.(i) <- true 
    | `Goto(`Line_num(j)) | `If(_, `Line_num(j)) -> 
      is_leader.(j) <- true;
      if i < length - 1 then is_leader.(i+1) <- true
    | _ -> ()) instrs;
  let nodes_ref = ref [] 
  and id = ref 0
  and l = ref 0
  and tbl : (int, int) Hashtbl.t = Hashtbl.create 16 (* maps starting instrs line number to node id*)
   in
  Hashtbl.add tbl 0 0 ;
  for r = 1 to length do 
    if is_leader.(r) then 
    let newinstrs = Array.make (r - !l) `Nop in 
    Array.blit instrs !l newinstrs 0 (r - !l);
    let node = Node.make !id newinstrs !l func.func_name in 
    Hashtbl.add tbl r (!id + 1);
    nodes_ref := node :: !nodes_ref; incr id; l := r
  done;
  let nodes = !nodes_ref |> List.rev in 
  let nodes_array = Array.of_list nodes in
  let length_of_nodes = List.length nodes in
  let open Node in
  Array.iteri (fun i node -> 
             match node.instrs.(Array.length node.instrs - 1) with 
               | `Goto(`Line_num(j)) -> 
                 let id' = Hashtbl.find tbl j in
                 node.succ <- [nodes_array.(id')];
                 nodes_array.(id').pred <- node :: nodes_array.(id').pred; 
               | `If(_, `Line_num(j)) -> 
                 let id' = Hashtbl.find tbl j in
                 node.succ <- [nodes_array.(id')];
                 nodes_array.(id').pred <- node :: nodes_array.(id').pred; 
                 if i < length_of_nodes - 1 then 
                 (node.succ <- nodes_array.(i+1) :: node.succ;
                 nodes_array.(i+1).pred <- node :: nodes_array.(i+1).pred)
               | _ -> 
                 if i < length_of_nodes - 1 then 
                 (node.succ <- nodes_array.(i+1) :: node.succ;
                 nodes_array.(i+1).pred <- node :: nodes_array.(i+1).pred)) nodes_array;
  let entry = Node.make_entry func.func_name
  and exit = Node.make_exit func.func_name in
  List.iter
  (fun node -> 
    begin 
      match get_preds node with 
        | [] -> node.pred <- [entry]; entry.succ <- node :: entry.succ
        | _ -> ()
    end;
    begin
      match get_succs node with 
        | [] -> node.succ <- [exit]; exit.pred <- node :: exit.pred
        | _ -> ()
    end) nodes;
  {
    pname = func.func_name;
    locals;
    formals;
    nodes = entry :: (nodes @ [exit]);
    node_num = List.length nodes + 2;
    start_node = entry;
    exit_node = exit;
  }

let string_of_proc : t -> string = fun procdesc -> 
  List.fold_left 
  (fun str node -> str ^ Node.string_of_node node) "" procdesc.nodes

let from_prog : M.prog -> t list = 
  List.map from_func

let string_of_t_list : t list -> string = 
  List.fold_left 
  (fun str proc -> str ^ "\n" ^ string_of_proc proc) "" 