type t

val startpos : t

val pieces : ?side:Side.t -> t -> int64

val pawns : ?side:Side.t -> t -> int64

val knights : ?side:Side.t -> t -> int64

val bishops : ?side:Side.t -> t -> int64

val rooks : ?side:Side.t -> t -> int64

val queens : ?side:Side.t -> t -> int64

val kings : ?side:Side.t -> t -> int64

val can_o_o : ?turn:Turn.t -> t -> bool

val can_o_o_o : ?turn:Turn.t -> t -> bool

val can_castle : ?turn:Turn.t -> t -> bool

val enp_square : t -> Square.t option

val move_order : t -> Side.t

val turn : Side.t -> t -> Turn.t

val half_moves_clock : t -> int

val hash : t -> int64

val move : Move.t -> t -> t

val reachable : t -> Square.t -> Square.t -> bool

val piece_at : Square.t -> t -> Piece.t

