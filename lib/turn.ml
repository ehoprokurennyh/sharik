type t = | Us | Partner

let not turn =
    match turn with
    | Us -> Partner
    | Partner -> Us

