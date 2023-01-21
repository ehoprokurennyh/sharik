type t

val create : unit -> t

val start_search : Time.t -> t -> unit

val continue_search : t -> bool

val board : t -> Board.t

val set_board : Board.t -> t -> t

val ht : t -> Hash.t

val forceMove : t -> ForceMove.t

