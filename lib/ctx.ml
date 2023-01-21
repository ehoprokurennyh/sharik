type t = { board: Board.t; ht: Hash.t; fm: ForceMove.t; mutable time: float; mutable time_left: Time.t }

let create () =
    { board = Board.startpos; ht = Hash.create (); fm = ForceMove.create (); time = 0.;
      time_left = { Time.time = 0.; inc = 0.; movestogo = None } }

let start_search time_left t = t.time <- Unix.gettimeofday (); t.time_left <- time_left

let continue_search t =
    let n = Option.value t.time_left.movestogo ~default:15. +. 1. in
    let move_time = min (t.time_left.time /. 1.5) (t.time_left.time /. n +. t.time_left.inc) in
    Unix.gettimeofday () -. t.time < move_time

let board x = x.board

let set_board b x = { x with board = b }

let ht x = x.ht

let forceMove x = x.fm

