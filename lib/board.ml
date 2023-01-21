type t = { white_pawns: int64; black_pawns: int64; white_knights: int64; black_knights: int64;
           white_bishops: int64; black_bishops: int64; white_rooks: int64; black_rooks: int64;
           white_queens: int64; black_queens: int64; white_king: int64; black_king: int64;
           white_pieces: int64; black_pieces: int64;
           o_o_o_white: bool; o_o_o_black: bool; o_o_white: bool; o_o_black: bool; enp: Square.t option;
           move_order: Side.t; half_moves_clock: int; mutable hash: int64 option }

let startpos = { white_pawns = 65280L; black_pawns = 71776119061217280L; white_knights = 66L;
                 black_knights = 4755801206503243776L; white_bishops = 36L; black_bishops = 2594073385365405696L;
                 white_rooks = 129L; black_rooks = -9151314442816847872L; white_queens = 8L;
                 black_queens = 576460752303423488L; white_king = 16L; black_king = 1152921504606846976L;
                 o_o_o_white = true; o_o_o_black = true; o_o_white = true; o_o_black = true; enp = None;
                 move_order = Side.White; half_moves_clock = 0; white_pieces = 65535L; black_pieces = -281474976710656L; hash = None }

let pieces ?(side = Side.Both) b =
    match side with
    | Side.White -> b.white_pieces
    | Side.Black -> b.black_pieces
    | Side.Both -> Int64.logor b.white_pieces b.black_pieces

let pawns ?(side = Side.Both) b =
    match side with
    | Side.White -> b.white_pawns
    | Side.Black -> b.black_pawns
    | Both -> Int64.logor b.white_pawns b.black_pawns

let knights ?(side = Side.Both) b =
    match side with
    | Side.White -> b.white_knights
    | Side.Black -> b.black_knights
    | Side.Both -> Int64.logor b.white_knights b.black_knights

let bishops ?(side = Side.Both) b =
    match side with
    | Side.White -> b.white_bishops
    | Side.Black -> b.black_bishops
    | Side.Both -> Int64.logor b.white_bishops b.black_bishops

let rooks ?(side = Side.Both) b =
    match side with
    | Side.White -> b.white_rooks
    | Side.Black -> b.black_rooks
    | Side.Both -> Int64.logor b.white_rooks b.black_rooks

let queens ?(side = Side.Both) b =
    match side with
    | Side.White -> b.white_queens
    | Side.Black -> b.black_queens
    | Side.Both -> Int64.logor b.white_queens b.black_queens

let kings ?(side = Side.Both) b =
    match side with
    | Side.White -> b.white_king
    | Side.Black -> b.black_king
    | Side.Both -> Int64.logor b.white_king b.black_king

let can_o_o ?(turn = Turn.Us) b =
    match b.move_order with
    | Side.White -> if turn = Turn.Us then b.o_o_white else b.o_o_black
    | Side.Black -> if turn = Turn.Us then b.o_o_black else b.o_o_white
    | Side.Both -> failwith "Wrong side"

let can_o_o_o ?(turn = Turn.Us) b =
    match b.move_order with
    | Side.White -> if turn = Turn.Us then b.o_o_o_white else b.o_o_o_black
    | Side.Black -> if turn = Turn.Us then b.o_o_o_black else b.o_o_o_white
    | Side.Both -> failwith "Wrong side"

let can_castle ?(turn = Turn.Us) b =
    can_o_o ~turn:turn b || can_o_o_o ~turn:turn b

let enp_square b = b.enp

let move_order b = b.move_order

let turn us b = if b.move_order = us then Turn.Us else Turn.Partner

let half_moves_clock b = b.half_moves_clock

let hash b =
    match b.hash with
    | None -> let h = 42L in b.hash <- Some h; h
    | Some h -> h

let rec apply_bb ?(piece = None) f side b =
    match side, piece with
    | Side.Both, _ -> apply_bb ~piece:piece f Side.White b |> apply_bb ~piece:piece f Side.Black
    | _, None -> apply_bb ~piece:(Some Piece.pawn) f side b |> apply_bb ~piece:(Some Piece.knight) f side |> apply_bb ~piece:(Some Piece.bishop) f side
              |> apply_bb ~piece:(Some Piece.rook) f side |> apply_bb ~piece:(Some Piece.queen) f side |> apply_bb ~piece:(Some Piece.king) f side
    | Side.White, Some Piece.Pawn -> let v = f b.white_pawns in { b with white_pawns = v; white_pieces = Int64.logxor b.white_pieces b.white_pawns |> Int64.logxor v }
    | Side.Black, Some Piece.Pawn -> let v = f b.black_pawns in { b with black_pawns = v; black_pieces = Int64.logxor b.black_pieces b.black_pawns |> Int64.logxor v }
    | Side.White, Some Piece.Knight -> let v = f b.white_knights in { b with white_knights = v; white_pieces = Int64.logxor b.white_pieces b.white_knights |> Int64.logxor v }
    | Side.Black, Some Piece.Knight -> let v = f b.black_knights in { b with black_knights = v; black_pieces = Int64.logxor b.black_pieces b.black_knights |> Int64.logxor v }
    | Side.White, Some Piece.Bishop -> let v = f b.white_bishops in { b with white_bishops = v; white_pieces = Int64.logxor b.white_pieces b.white_bishops |> Int64.logxor v }
    | Side.Black, Some Piece.Bishop -> let v = f b.black_bishops in { b with black_bishops = v; black_pieces = Int64.logxor b.black_pieces b.black_bishops |> Int64.logxor v }
    | Side.White, Some Piece.Rook -> let v = f b.white_rooks in { b with white_rooks = v; white_pieces = Int64.logxor b.white_pieces b.white_rooks |> Int64.logxor v }
    | Side.Black, Some Piece.Rook -> let v = f b.black_rooks in { b with black_rooks = v; black_pieces = Int64.logxor b.black_pieces b.black_rooks |> Int64.logxor v }
    | Side.White, Some Piece.Queen -> let v = f b.white_queens in { b with white_queens = v; white_pieces = Int64.logxor b.white_pieces b.white_queens |> Int64.logxor v }
    | Side.Black, Some Piece.Queen -> let v = f b.black_queens in { b with black_queens = v; black_pieces = Int64.logxor b.black_pieces b.black_queens |> Int64.logxor v }
    | Side.White, Some Piece.King -> let v = f b.white_king in { b with white_king = v; white_pieces = Int64.logxor b.white_pieces b.white_king |> Int64.logxor v }
    | Side.Black, Some Piece.King -> let v = f b.black_king in { b with black_king = v; black_pieces = Int64.logxor b.black_pieces b.black_king |> Int64.logxor v }

let remove_piece ?(piece = None) square = Square.unset_bb square |> apply_bb ~piece:piece

let set_piece piece square = Square.set_bb square |> apply_bb ~piece:(Some piece)

let move m b =
    let piece = Move.piece m in
    let from_square = Move.from_square m in
    let to_square = Move.to_square m in
    let move_order = Side.not b.move_order in
    let enp =
        match piece with
        | Piece.Pawn when abs (Square.to_int from_square - Square.to_int to_square) = 16 ->
                let field = Square.between to_square from_square in
                let enemy_pawns = Int64.logand (pawns ~side:move_order b) (Square.pawn_attacks b.move_order field) in
                if enemy_pawns = 0L then None else Some field
        | _ -> None in
    let o_o =
        match piece with
        | Piece.King -> false
        | Piece.Rook when Square.to_int from_square = (if b.move_order = Side.White then 7 else 63) -> false
        | _ -> can_o_o b in
    let o_o_o =
        match piece with
        | Piece.King -> false
        | Piece.Rook when Square.to_int from_square = (if b.move_order = Side.White then 0 else 56) -> false | _ -> can_o_o_o b in
    let o_o_partner =
        match Square.to_int to_square, move_order with
        | 7, Side.White -> false
        | 63, Side.Black -> false
        | _ -> can_o_o ~turn:Turn.Partner b in
    let o_o_o_partner =
        match Square.to_int to_square, move_order with
        | 0, Side.White -> false
        | 56, Side.Black -> false
        | _ -> can_o_o_o ~turn:Turn.Partner b in
    let capture_square, enp_capture =
        match to_square, b.enp, piece with
        | s, Some s', Piece.Pawn when s = s' -> Some (Square.of_int (Square.to_int s + (if b.move_order = Side.White then -8 else 8))), true
        | s, _, _ when Square.check_bb s (pieces ~side:move_order b) -> Some s, false
        | _, _, _ -> None, false in
    let half_moves_clock =
        match piece, capture_square with
        | Piece.Pawn, _ -> 0
        | _, Some _ -> 0
        | _ -> b.half_moves_clock + 1 in
    let is_castle =
        match piece with
        | Piece.King when Square.to_int from_square - Square.to_int to_square |> abs = 2 -> true
        | _ -> false in
    let b' = remove_piece from_square b.move_order ~piece:(Some piece) b in
    let b' =
        match capture_square, is_castle with
        | None, false -> b'
        | None, true -> let rook_square = Square.of_file_rank (if to_square < from_square then 0 else 7 )(Square.rank to_square) in
                        remove_piece ~piece:(Some Piece.Rook) rook_square b.move_order b'
                        |> set_piece Piece.Rook (Square.between to_square from_square) b.move_order
        | Some s, _ -> remove_piece s move_order ~piece:(if enp_capture then Some Pawn else None) b' in
    let b' = set_piece (if piece = Piece.Pawn then Move.promotion m else piece) to_square b.move_order b' in
    { b' with o_o_o_white = if move_order = Side.Black then o_o_o else o_o_o_partner;
      o_o_o_black = if move_order = Side.White then o_o_o else o_o_o_partner;
      o_o_white = if move_order = Side.Black then o_o else o_o_partner;
      o_o_black = if move_order = Side.White then o_o else o_o_partner;
      enp = enp; move_order = move_order; half_moves_clock = half_moves_clock; hash = None }

let reachable b = Square.reachable (pieces b)

let piece_at s b =
    let check bb = if Int64.logand (Square.to_bb s) bb <> 0L then true else false in
    if check (pawns b) then Piece.Pawn
    else if check (knights b) then Piece.Knight
    else if check (bishops b) then Piece.Bishop
    else if check (rooks b) then Piece.Rook
    else if check (queens b) then Piece.Queen
    else if check (kings b) then Piece.King
    else if check (queens b) then Piece.Pawn
    else failwith "No piece"

