let gen_abstract ?(p = fun _ _ -> true) attackers targets attack =
    let gen_abstract_aux acc attacker =
        Square.scan (Int64.logand (attack attacker) targets) 
        |> Seq.filter_map (fun target -> if p attacker target then Some (attacker, target) else None)
        |> Seq.append acc
    in Square.fold_bb gen_abstract_aux Seq.empty attackers

let seq l = List.fold_left Seq.append Seq.empty l

let check_squares_safety side b bb =
    let partner = Side.not side in
    let q = Board.queens ~side:partner b in
    seq
    [
        gen_abstract ~p:(Board.reachable b) bb (Board.rooks ~side:partner b |> Int64.logor q) Square.rook_moves;
        gen_abstract ~p:(Board.reachable b) bb (Board.bishops ~side:partner b |> Int64.logor q) Square.bishop_moves;
        gen_abstract bb (Board.knights ~side:partner b) Square.knight_moves;
        gen_abstract bb (Board.pawns ~side:partner b) (Square.pawn_attacks side);
        gen_abstract bb (Board.kings ~side:partner b) Square.king_moves;
    ] |> Seq.is_empty

let check_king_safety side b = check_squares_safety side b (Board.kings ~side:side b)

let is_check b = check_king_safety (Board.move_order b) b |> not

let transform_moves b =
    Seq.filter_map (fun m -> let b = Board.move m b in
                    if check_king_safety (Side.not (Board.move_order b)) b then Some (m, b) else None)

let promote_if_need s =
    let rec promote_if_need_aux acc node =
        match node () with
        | Seq.Nil -> acc
        | Seq.Cons ((f, t), tl) ->
                let acc = if Square.rank t > 0 && Square.rank t < 7 then
                    Seq.cons (Move.of_piece_from_to Piece.Pawn (f, t)) acc
                else
                    Seq.cons (Move.make_promotion Piece.Queen (f, t)) (Seq.cons (Move.make_promotion Piece.Knight (f, t)) acc)
                in promote_if_need_aux acc tl
    in promote_if_need_aux Seq.empty s

let gen_pawn_captures b =
    let us = Board.move_order b in
    gen_abstract (Board.pawns ~side:us b) (Board.pieces ~side:(Side.not us) b) (Square.pawn_attacks us)
    |> promote_if_need

let gen_enp b =
    let us = Board.move_order b in
    match Board.enp_square b with
    | None -> Seq.empty
    | Some s -> gen_abstract (Square.to_bb s) (Board.pawns ~side:us b) (Square.pawn_attacks (Side.not us))
    |> Seq.map (fun (f, t) -> Move.of_piece_from_to Piece.pawn (t, f))

let gen_knight_captures b = 
    let us = Board.move_order b in
    gen_abstract (Board.knights ~side:us b) (Board.pieces ~side:(Side.not us) b) Square.knight_moves
    |> Seq.map (Move.of_piece_from_to Piece.Knight)

let gen_king_captures b = 
    let us = Board.move_order b in
    gen_abstract (Board.kings ~side:us b) (Board.pieces ~side:(Side.not us) b) Square.king_moves
    |> Seq.map (Move.of_piece_from_to Piece.King)

let gen_bishop_captures b = 
    let us = Board.move_order b in
    gen_abstract ~p:(Board.reachable b) (Board.bishops ~side:us b) (Board.pieces ~side:(Side.not us) b) Square.bishop_moves
    |> Seq.map (Move.of_piece_from_to Piece.Bishop)

let gen_rook_captures b = 
    let us = Board.move_order b in
    gen_abstract ~p:(Board.reachable b) (Board.rooks ~side:us b) (Board.pieces ~side:(Side.not us) b) Square.rook_moves
    |> Seq.map (Move.of_piece_from_to Piece.Rook)

let gen_queen_captures b = 
    let us = Board.move_order b in
    gen_abstract ~p:(Board.reachable b) (Board.queens ~side:us b) (Board.pieces ~side:(Side.not us) b) Square.queen_moves
    |> Seq.map (Move.of_piece_from_to Piece.Queen)

let gen_captures b =
    seq
    [
        gen_pawn_captures b; gen_enp b; gen_knight_captures b; gen_bishop_captures b;
        gen_rook_captures b; gen_queen_captures b; gen_king_captures b;
    ] |> transform_moves b

let gen_castles b =
    let us = Board.move_order b in
    let king = match Square.lsb (Board.kings ~side:us b) with
    | Some king -> king
    | None -> failwith "Checkers?" in
    let res = if Board.can_o_o_o b && Int64.logand (Square.o_o_o_free_squares us) (Board.pieces b) = 0L && check_squares_safety us b (Square.o_o_o_squares us) then Seq.return Square.king_ooo else Seq.empty in
    let res = if Board.can_o_o b && Int64.logand (Square.o_o_free_squares us) (Board.pieces b) = 0L && check_squares_safety us b (Square.o_o_squares us) then Seq.cons Square.king_oo res else res in
    Seq.map (fun f -> let m = Move.of_piece_from_to Piece.King (king, f king) in m, Board.move m b) res


let gen_pawn_moves b = 
    let us = Board.move_order b in
    gen_abstract ~p:(Board.reachable b) (Board.pawns ~side:us b) (Int64.lognot (Board.pieces b)) (Square.pawn_moves us)
    |> promote_if_need

let gen_knight_moves b = 
    let us = Board.move_order b in
    gen_abstract (Board.knights ~side:us b) (Int64.lognot (Board.pieces b)) Square.knight_moves
    |> Seq.map (Move.of_piece_from_to Piece.Knight)

let gen_king_moves b = 
    let us = Board.move_order b in
    gen_abstract (Board.kings ~side:us b) (Int64.lognot (Board.pieces b)) Square.king_moves
    |> Seq.map (Move.of_piece_from_to Piece.King)

let gen_bishop_moves b = 
    let us = Board.move_order b in
    gen_abstract ~p:(Board.reachable b) (Board.bishops ~side:us b) (Int64.lognot (Board.pieces b)) Square.bishop_moves
    |> Seq.map (Move.of_piece_from_to Piece.Bishop)

let gen_rook_moves b = 
    let us = Board.move_order b in
    gen_abstract ~p:(Board.reachable b) (Board.rooks ~side:us b) (Int64.lognot (Board.pieces b)) Square.rook_moves
    |> Seq.map (Move.of_piece_from_to Piece.Rook)

let gen_queen_moves b = 
    let us = Board.move_order b in
    gen_abstract ~p:(Board.reachable b) (Board.queens ~side:us b) (Int64.lognot (Board.pieces b)) Square.queen_moves
    |> Seq.map (Move.of_piece_from_to Piece.Queen)

let gen_quiet b =
    seq
    [
        gen_queen_moves b; gen_rook_moves b; gen_bishop_moves b;
        gen_knight_moves b; gen_pawn_moves b; gen_king_moves b;
    ] |> transform_moves b

let gen_moves b = seq [gen_captures b; gen_castles b; gen_quiet b]

