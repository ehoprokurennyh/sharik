type t = | Draw | Checkmate of Turn.t*int | Cp of int

val dive_in : t -> t

val dive_out : t -> t

val not : t -> t

val max_depth : int

val of_int : int -> t

val to_int : t -> int

val cmp : t -> t -> int

val clamp : t -> t -> t -> t

val min : t -> t -> t

val max : t -> t -> t

val worst : t

val best : t

val to_string : t -> string

