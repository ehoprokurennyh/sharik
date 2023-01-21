type t = | Draw | Checkmate of Turn.t*int | Cp of int

let dive_in v =
    match v with
    | Draw -> Draw
    | Checkmate (turn, depth) -> Checkmate (Turn.not turn, depth - 1)
    | Cp v -> Cp (-v)

let dive_out v =
    match v with
    | Draw -> Draw
    | Checkmate (turn, depth) -> Checkmate (Turn.not turn, depth + 1)
    | Cp v -> Cp (-v)

let not v =
    match v with
    | Draw -> Draw
    | Checkmate (turn, depth) -> Checkmate (Turn.not turn, depth)
    | Cp v -> Cp (-v)

let checkmate_val = 1000000000

let max_depth = 255

let of_int v =
    match v with
    | 0 -> Draw
    | x when abs x > checkmate_val - max_depth -> Checkmate ((if x > 0 then Partner else Us), abs x - max_depth)
    | x -> Cp x

let to_int v =
    match v with
    | Draw -> 0
    | Checkmate (Us, depth) -> -checkmate_val + depth
    | Checkmate (Partner, depth) -> checkmate_val - depth
    | Cp v -> v

let cmp x y = compare (to_int x) (to_int y)

let clamp x y z = if cmp y x = -1 then x else if cmp y z = 1 then z else y

let min x y = if cmp x y = -1 then x else y

let max x y = if cmp x y = 1 then x else y

let worst = Checkmate (Us, 0)

let best = Checkmate (Partner, 0)

let to_string = function
    | Draw -> "cp 0"
    | Checkmate (Us, depth) -> "mate " ^ string_of_int (-(depth + 1) / 2)
    | Checkmate (Partner, depth) -> "mate " ^ string_of_int ((depth + 1) / 2)
    | Cp x -> "cp " ^ string_of_int x
