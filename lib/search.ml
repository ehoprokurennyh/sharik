let rec quiesce alpha beta ctx b =
    let rec quiesce_aux best bs =
        match bs () with
        | Seq.Nil -> best
        | Seq.Cons (b, bs') -> let v = quiesce (Val.not beta) (Val.not best) ctx b |> Val.dive_out in
        if Val.cmp v beta > -1 then beta else quiesce_aux (Val.max best v) bs' in
    let v = Eval.eval ctx b in
    if Val.cmp v beta > -1 then beta
    else MoveGen.gen_captures b
    |> Seq.filter (fun (m, b) ->
            let piece, to_square = Move.piece m, Move.to_square m in
            match piece with
            | Piece.Pawn -> true
            | _ when Eval.piece_cp (Board.piece_at to_square b) >= Eval.piece_cp piece -> true
            | _ -> MoveGen.check_squares_safety (Board.move_order b) b (Square.to_bb to_square))
    |> Seq.map snd |> quiesce_aux (Val.max v alpha)

exception Timeout

let rec search ?(alpha = Val.worst) ?(beta = Val.best) ctx d b =
    if d <= 0 then quiesce alpha beta ctx b
    else let rec search_aux best bs =
        match bs () with
        | Seq.Nil -> best
        | Seq.Cons (b, bs') -> let v = search ~alpha:(Val.not beta) ~beta:(Val.not best) ctx (d - 1) b |> Val.dive_out in
        if Val.cmp v beta > -1 then beta else search_aux (Val.max best v) bs' in
    if not (Ctx.continue_search ctx) then raise Timeout
    else MoveGen.gen_moves b |> Seq.map snd |> search_aux alpha

let root_search ctx =
    let res, yield = Lwt.wait () in
    let moves = Ctx.board ctx |> MoveGen.gen_moves |> List.of_seq in
    let rec root_aux (best_score, best_move, depth, rest_moves) =
        match rest_moves with
        | [] -> let info = { Info.score = Val.dive_out best_score; pv = best_move; depth = depth } in
                let moves = match best_move with
                | None -> moves
                | Some m -> List.stable_sort (fun x y -> compare (fst x <> m) (fst y <> m)) moves in
                Lwt.return_some (info, (Val.best, best_move, depth + 1, moves))
        | (m, b)::rest ->
                try
                    let score = search ~alpha:Val.worst ~beta:best_score ctx depth b in
                    let best_score', best_move' =
                        match Val.cmp score best_score with
                        | -1 -> score, Some m
                        | _ -> best_score, best_move in
                    root_aux (best_score', best_move', depth, rest)
                with Timeout -> Lwt.wakeup yield best_move; Lwt.return_none in
    res, Lwt_seq.unfold_lwt root_aux (Val.best, None, 2, moves)

let perft ctx d =
    let rec aux b depth =
        if depth = 0 then 1 else MoveGen.gen_moves b
        |> Seq.map snd
        |> Seq.map (depth - 1 |> Fun.flip aux)
        |> Seq.fold_left (+) 0 in
    aux (Ctx.board ctx) d

