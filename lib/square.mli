type t = private int

val of_int : int -> t

val to_int : t -> int

val file : t -> int

val rank : t -> int

val of_file_rank : int -> int -> t

val of_string : string -> t

val to_string : t -> string

val correct : t -> bool

val correct_file_rank : int -> int -> bool

val lsb : int64 -> t option

val scan : int64 -> t Seq.t

val to_bb : t -> int64

val check_bb : t -> int64 -> bool

val set_bb : t -> int64 -> int64

val unset_bb : t -> int64 -> int64

val set_bb_filerank : int -> int -> int64 -> int64

val fold_bb : ('a -> t -> 'a) -> 'a -> int64 -> 'a

val count_bb : int64 -> int

val between : t -> t -> t

val reachable : int64 -> t -> t -> bool

val slide : (int*int) list -> t -> int64

val look_around : (int*int) list -> t -> int64

val pawn_attacks : Side.t -> t -> int64

val pawn_moves : Side.t -> t -> int64

val knight_moves : t -> int64

val bishop_moves : t -> int64

val rook_moves : t -> int64

val queen_moves : t -> int64

val king_moves : t -> int64

val o_o_squares : Side.t -> int64

val o_o_o_squares : Side.t -> int64

val o_o_free_squares : Side.t -> int64

val o_o_o_free_squares : Side.t -> int64

val king_oo : t -> t

val king_ooo : t -> t

val rank_1 : int64

val rank_2 : int64

val rank_3 : int64

val rank_4 : int64

val rank_5 : int64

val rank_6 : int64

val rank_7 : int64

val rank_8 : int64

