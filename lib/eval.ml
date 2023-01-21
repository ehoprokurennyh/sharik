let piece_cp piece =
    match piece with
    | Piece.Pawn -> 100
    | Piece.Knight -> 350
    | Piece.Bishop -> 350
    | Piece.Rook -> 550
    | Piece.Queen -> 950
    | Piece.King -> 9000

let game_over b = MoveGen.gen_moves b |> Seq.is_empty

let cp_pieces b ((f : ?side:Side.t -> Board.t -> int64), piece) =
    (Square.count_bb (f ~side:Side.White b) - Square.count_bb (f ~side:Side.Black b)) * piece_cp piece

    (*
let first_rank_penalty b side =
    Int64.logor (Board.bishops ~side:side b) (Board.knights ~side:side b)
    |> Int64.logor (Board.queens ~side:side b)
    |> Int64.logand (if side = Side.White then Square.rank_1 else Square.rank_8)
    |> Square.count_bb |> ( * ) (-20)


*)
let pawn_promote_bonus b side =
    Board.pawns ~side:side b |> Square.scan
    |> Seq.fold_left (fun v s -> v + (if side = Side.White then Square.rank s else 7 - Square.rank s) * 2) 0

(*
let figure_blocks_pawn_penalty b side =
    if Board.pieces b |> Square.count_bb <= 20 then 0
    else let partner = Side.not side in
    let blocking_pieces = if side = Side.White then Square.rank_3 else Square.rank_6 in
    let pawns_rank = if side = Side.White then Square.rank_2 else Square.rank_7 in
    let blocked_pawns = Board.pawns ~side:side b |> Int64.logand pawns_rank in
    let shift = Fun.flip (if side = Side.White then Int64.shift_right else Int64.shift_left) 8 in
    let penalty bb = Int64.logand bb blocking_pieces |> shift |> Int64.logand blocked_pawns |> Square.count_bb in
    -(penalty (Board.pawns ~side:side b) * 15 + penalty (Board.pawns ~side:partner b) * 40
    + penalty (Board.knights ~side:side b) * 8 + penalty (Board.knights ~side:partner b) * 30
    + penalty (Board.bishops ~side:side b) * 11 + penalty (Board.bishops ~side:partner b) * 25)
*)

let center_bonus b side =
    Board.pieces ~side:side b |> Int64.logand 66229406269440L
    |> Square.count_bb |> ( * ) 30

let eval _ b =
    if game_over b then if MoveGen.is_check b then Val.worst else Val.Draw
    else let res = [Board.pawns, Piece.Pawn; Board.knights, Piece.Knight; Board.bishops, Piece.Bishop; Board.rooks, Piece.Rook; Board.queens, Piece.Queen]
    |> List.map (cp_pieces b) |> List.fold_left (+) 0 in
(*
    let res = res + first_rank_penalty b Side.White - first_rank_penalty b Side.Black in
    let res = res + figure_blocks_pawn_penalty b Side.White - figure_blocks_pawn_penalty b Side.Black in*)
    let res = res + pawn_promote_bonus b Side.White - pawn_promote_bonus b Side.Black in
    let res = res + center_bonus b Side.White - center_bonus b Side.Black in
    Val.Cp (if Board.move_order b = Side.White then res else -res)

