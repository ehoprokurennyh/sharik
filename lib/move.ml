type t = int

let of_int = Fun.id

let to_int = Fun.id

let from_square m = Int.shift_right m 8 land 255 |> Square.of_int

let to_square m = m land 255 |> Square.of_int

let promotion m = Int.shift_right m 24 |> Piece.of_int

let of_piece_from_to p (f, t) = 
    let p' = Piece.to_int p in
    let f' = Square.to_int f in
    let t' = Square.to_int t in
    Int.shift_left p' 16 lor Int.shift_left f' 8 lor t'

let make_promotion p ft = Int.shift_left (Piece.to_int p) 24 lor of_piece_from_to Piece.Pawn ft

let to_string m = Square.to_string (from_square m) ^ Square.to_string (to_square m) ^ (let prom = promotion m in if prom = Piece.Pawn then "" else Piece.to_string prom)

let piece m = Int.shift_right_logical m 16 land 255 |> Piece.of_int

