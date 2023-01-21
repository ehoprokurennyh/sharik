type t = int

let of_int = Fun.id

let to_int = Fun.id

let file s = s land 7

let rank s = Int.shift_right_logical s 3

let of_file_rank f r = Int.shift_left r 3 lor f

let of_string = function
    | s when String.length s = 2 -> of_file_rank (String.index "abcdefgh" (String.get s 0)) (int_of_string (String.sub s 1 1) - 1)
    | _ -> failwith "Incorrect square format"

let to_string s = String.sub "abcdefgh" (file s) 1 ^ string_of_int (rank s + 1)

let correct s = s >= 0 && s <= 63

let correct_file_rank f r = f >= 0 && f <= 7 && r >= 0 && r <= 7

let lsb = function
    | 0L -> None
    | bb -> Some (Ocaml_intrinsics.Int64.count_trailing_zeros_nonzero_arg bb)

let to_bb s = Int64.shift_left 1L (to_int s)

let unset_bb s bb =
    let mask = to_bb s in
    Int64.logand bb (Int64.lognot mask)

let scan bb =
    let scan_aux (bb : int64) =
        match bb with
        | 0L -> None
        | _ -> let s = Ocaml_intrinsics.Int64.count_trailing_zeros_nonzero_arg bb in
               Some (s, unset_bb s bb)
    in Seq.unfold scan_aux bb

let check_bb s bb =
    let mask = to_bb s in
    Int64.logand bb mask <> 0L

let set_bb s bb =
    let mask = to_bb s in
    Int64.logor bb mask

let set_bb_filerank f r bb = Int64.logor (to_bb (of_file_rank f r)) bb

let fold_bb f x bb =
    scan bb |> Seq.fold_left f x

let count_bb = Ocaml_intrinsics.Int64.count_set_bits;;

let between s1 s2 = (s1 + s2) / 2

let reachable bb from_square to_square =
    let df, dr =  file to_square - file from_square, rank to_square - rank from_square in
    let df, dr = compare df 0, compare dr 0 in
    let rec scan_aux = function
        | s when s = to_square -> true
        | s when Int64.logand bb (to_bb s) <> 0L -> false 
        | s -> scan_aux (of_file_rank (file s + df) (rank s + dr)) in
    scan_aux (of_file_rank (file from_square + df) (rank from_square + dr))

let slide dirs s =
    let rec slide_aux (f, r) bb (df, dr) =
        if correct_file_rank f r then
            slide_aux (f + df, r + dr) (set_bb_filerank f r bb) (df, dr) 
        else
            bb in
    List.fold_left (slide_aux (file s, rank s)) 0L dirs

let look_around dirs s =
    List.fold_left (fun bb (df, dr) -> let f, r = file s + df, rank s + dr in
                    if correct_file_rank f r then set_bb_filerank f r bb else bb) 0L dirs

let cached1 f =
    let cache = Array.init 64 f in
    fun s -> cache.(s)

let cache2 f =
    let cache = Array.init 128 (fun i -> f (if i land 64 <> 0 then Side.Black else Side.White) (i land 63)) in
    fun side s -> match side with
    | Side.White -> cache.(s)
    | Side.Black -> cache.(64 lor s)
    | _ -> failwith "oops"

let pawn_attacks = cache2 (fun side -> look_around (if side = Side.White then [1, 1; -1, 1] else [1, -1; -1, -1]))

let pawn_moves = cache2 (fun side s ->
    let init_rank = if side = Side.White then 1 else 6 in
    let dr = if side = Side.White then 1 else -1 in
    look_around (if rank s = init_rank then [0, dr; 0, 2 * dr] else [0, dr]) s)

let knight_moves = look_around [1, 2; 2, 1; 1, -2; 2, -1; -1, 2; -2, 1; -2, -1; -1, -2] |> cached1

let bishop_moves = slide [1, 1; 1, -1; -1, 1; -1, -1] |> cached1

let rook_moves = slide [1, 0; -1, 0; 0, 1; 0, -1] |> cached1

let queen_moves = slide [1, 0; -1, 0; 0, 1; 0, -1; 1, 1; 1, -1; -1, 1; -1, -1] |> cached1

let king_moves = look_around [1, 0; -1, 0; 0, 1; 0, -1; 1, 1; 1, -1; -1, 1; -1, -1] |> cached1

let set_squares = List.fold_right set_bb

let o_o_squares = function
    | Side.White -> set_squares [4; 5; 6] 0L
    | Side.Black -> set_squares [60; 61; 62] 0L
    | _ -> failwith "Wrong side"

let o_o_o_squares = function
    | Side.White -> set_squares [2; 3; 4] 0L
    | Side.Black -> set_squares [58; 59; 60] 0L
    | _ -> failwith "Wrong side"

let o_o_free_squares = function
    | Side.White -> set_squares [5; 6] 0L
    | Side.Black -> set_squares [61; 62] 0L
    | _ -> failwith "Wrong side"

let o_o_o_free_squares = function
    | Side.White -> set_squares [1; 2; 3] 0L
    | Side.Black -> set_squares [57; 58; 59] 0L
    | _ -> failwith "Wrong side"

let king_oo = (+) 2

let king_ooo k = k - 2

let rank_n n = Int64.shift_left 255L ((n - 1) * 8)

let rank_1 = rank_n 1

let rank_2 = rank_n 2

let rank_3 = rank_n 3

let rank_4 = rank_n 4

let rank_5 = rank_n 5

let rank_6 = rank_n 6

let rank_7 = rank_n 7

let rank_8 = rank_n 8

