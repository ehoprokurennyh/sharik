type t = { depth: int; score: Val.t; pv: Move.t option }

let to_string { depth; score; pv } =
    let res = "info depth " ^ string_of_int depth ^ " score " ^ Val.to_string score in
    match pv with
    | None -> res ^ "\n"
    | Some m -> res ^ " pv " ^ Move.to_string m ^ "\n"

