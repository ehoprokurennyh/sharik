type t = | Pawn | Knight | Bishop | Rook | Queen | King

let pawn = Pawn

let knight = Knight

let bishop = Bishop

let rook = Rook

let queen = Queen

let king = King

let of_int = function
    | 0 -> Pawn
    | 1 -> Knight
    | 2 -> Bishop
    | 3 -> Rook
    | 4 -> Queen
    | 5 -> King
    | _ -> failwith "Wrong piece int value"

let to_int = function
    | Pawn -> 0
    | Knight -> 1
    | Bishop -> 2
    | Rook -> 3
    | Queen -> 4
    | King -> 5

let show = function
    | Pawn -> "p"
    | Knight -> "n"
    | Bishop -> "b"
    | Rook -> "r"
    | Queen -> "q"
    | King -> "k"

let of_string = function
    | "p" -> Pawn
    | "n" -> Knight
    | "b" -> Bishop
    | "r" -> Rook
    | "q" -> Queen
    | "k" -> King
    | _ -> failwith "Wrong piece format"

let to_string = function
    | Pawn -> "p"
    | Knight -> "n"
    | Bishop -> "b"
    | Rook -> "r"
    | Queen -> "q"
    | King -> "k"

