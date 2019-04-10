
open Mimple



let code_frag : prog ref = ref [] 

let emit : stmt -> unit =
  fun s -> code_frag := s :: !code_frag
