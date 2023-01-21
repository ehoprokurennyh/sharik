val check_squares_safety : Side.t -> Board.t -> int64 -> bool

val is_check : Board.t -> bool

val gen_captures : Board.t -> (Move.t*Board.t) Seq.t

val gen_moves : Board.t -> (Move.t*Board.t) Seq.t

