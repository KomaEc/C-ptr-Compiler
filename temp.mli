
type t

val newtemp : ?hint:Symbol.t -> unit -> t 

val string_of_temp : t -> string

type label = Symbol.t 

val newlabel : ?hint:Symbol.t -> unit -> label 

val string_of_label : label -> string
