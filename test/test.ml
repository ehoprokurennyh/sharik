open Sharik_core

let test_perft moves depth perft =
    let b = List.fold_left (fun b s -> Board.move (Uci.move_of_string b s) b) Board.startpos moves in
    let n = Search.perft (Ctx.create () |> Ctx.set_board b) depth in
    Alcotest.(check int) "same nodes count" perft n

let test_movegen_startpos_2 () =
    test_perft [] 2 400

let test_movegen_startpos_3 () =
    test_perft [] 3 8902

let test_movegen_startpos_4 () =
    test_perft [] 4 197281

let test_movegen_startpos_5 () =
    test_perft [] 5 4865609

let test_movegen_custompos () =
    test_perft ["e2e4"; "e7e5"; "b1c3"; "g8f6"; "d2d3"; "a7a6"; "c1g5"; "f8e7"] 5 36369022

let () =
    Alcotest.run "Perft" [
        "startpos", [
            Alcotest.test_case "Depth 2" `Quick test_movegen_startpos_2;
            Alcotest.test_case "Depth 3" `Quick test_movegen_startpos_3;
            Alcotest.test_case "Depth 4" `Quick test_movegen_startpos_4;
            Alcotest.test_case "Depth 5" `Slow test_movegen_startpos_5;
        ];
        "custompos", [
            Alcotest.test_case "Pos 1 Depth 5" `Slow test_movegen_custompos;
        ]
    ]

