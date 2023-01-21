type t = | White | Black | Both

let not side =
    match side with
    | White -> Black
    | Black -> White
    | Both -> Both

