open Lwt.Infix
open Lwt_seq

let detach f v = Lwt.return (f v)

let move_of_string b = function
    | s when String.length s = 4 -> let f = Square.of_string (String.sub s 0 2) in
    Move.of_piece_from_to (Board.piece_at f b) (f, (Square.of_string (String.sub s 2 2)))
    | s when String.length s = 5 -> Move.make_promotion (Piece.of_string (String.sub s 4 1)) (Square.of_string (String.sub s 0 2), Square.of_string (String.sub s 2 2))
    | _ -> failwith "Wrong move format"

let rec parse_moves b = function
    | [] -> b
    | (move::tl) -> parse_moves (Board.move (move_of_string b move) b) tl

let parse_pos = function
    | ["startpos"] -> Board.startpos
    | ("startpos"::"moves"::moves) -> parse_moves (Board.startpos) moves
    | _ -> failwith "Wrong pos format"

let uci_best_move = function
    | None -> "(none)"
    | Some m -> Move.to_string m
    |> Printf.sprintf "bestmove %s\n"

let parse_time side args =
    let rec aux time side args =
        match side, args with
        | _, [] -> time
        | Side.White, "wtime"::t::rest -> aux { time with Time.time = float_of_string t /. 1000. } side rest
        | Side.Black, "btime"::t::rest -> aux { time with time = float_of_string t /. 1000. } side rest
        | Side.White, "winc"::t::rest -> aux { time with inc = float_of_string t /. 1000. } side rest
        | Side.Black, "binc"::t::rest -> aux { time with inc = float_of_string t /. 1000. } side rest
        | _, "movestogo"::t::rest -> aux { time with movestogo = Some (float_of_string t) } side rest
        | _, _::rest -> aux time side rest in
    aux { Time.time = 100.; inc = 0.; movestogo = None } side args

let loop in_seq =
    let ctx = Ctx.create () in ignore ctx;
    let rec iter ctx x () =
        Lwt.apply x () >>= function
        | Nil -> Lwt.return Lwt_seq.Nil
        | Cons (cmd, tl) -> try
            match Str.split (Str.regexp "[ \t\n\r]") cmd with
            | ["uci"] -> Lwt_seq.Cons ("id name Sharik 0.1 alpha\nid author x86monkey\nuciok\n", iter ctx tl) |> Lwt.return
            | ["isready"] -> Lwt.return (Cons ("readyok\n", iter ctx tl))
            | ["quit"] -> Lwt.return Lwt_seq.Nil
            | ["go"; "perft"; n] -> Search.perft ctx
                                    |> Fun.flip detach (int_of_string n)
                                    >|= Printf.sprintf "Nodes searched: %d\n"
                                    >|= fun x -> Cons (x, iter ctx tl)
            | "go"::args -> let tm = parse_time (Board.move_order (Ctx.board ctx)) args in
                            Ctx.start_search tm ctx; let best, info = Search.root_search ctx in
                            Lwt_seq.iter_s (fun x -> Lwt_io.print (Info.to_string x)) info
                            >>= Fun.const best >>= fun x -> Lwt.return (Cons (uci_best_move x, iter ctx tl))
            | ("position"::pos) -> iter (Ctx.set_board (parse_pos pos) ctx) tl ()
            | _ -> Lwt.return (Cons ("Unknown command\n", iter ctx tl))
        with Failure s -> Lwt.return (Cons (s, iter ctx tl))
    in iter ctx in_seq

