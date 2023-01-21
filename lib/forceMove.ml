type t = bool Atomic.t
let create () = Atomic.make false
let set v = Atomic.set v true
let unset v = Atomic.set v false
let check = Atomic.get

