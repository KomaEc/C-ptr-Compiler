open Ir_3addr

type exp = unit


let code_frag : Ir_3addr.stmt list ref = ref [] 

let emit : Ir_3addr.stmt -> unit =
  fun s -> code_frag := s :: !code_frag