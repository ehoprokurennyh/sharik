type t = private int

val of_int : int -> t

val of_piece_from_to : Piece.t -> (Square.t*Square.t) -> t

val to_int : t -> int

val from_square : t -> Square.t

val to_square : t -> Square.t

val piece : t -> Piece.t

val promotion : t -> Piece.t

val to_string : t -> string

val make_promotion : Piece.t -> (Square.t*Square.t) -> t

