val search : ?alpha:Val.t -> ?beta:Val.t -> Ctx.t -> int -> Board.t -> Val.t

val root_search : Ctx.t -> Move.t option Lwt.t * Info.t Lwt_seq.t

val perft : Ctx.t -> int -> int

