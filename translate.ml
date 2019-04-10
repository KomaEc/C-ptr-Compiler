
open Mimple



let code_frag : prog ref = ref [] 

let emit : stmt -> unit =
  fun s -> code_frag := s :: !code_frag

let final : prog -> prog = List.rev

let get_final_mimple () = final !code_frag