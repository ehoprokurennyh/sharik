type t = | Pawn | Knight | Bishop | Rook | Queen | King

val pawn : t

val knight : t

val bishop : t

val rook : t

val queen : t

val king : t

val of_int : int -> t

val to_int : t -> int

val of_string : string -> t

val to_string : t -> string

val show : t -> string

