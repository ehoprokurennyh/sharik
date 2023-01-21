open Sharik_core
open Lwt.Infix

let main () =
     Lwt_seq.unfold_lwt (fun () -> Lwt_io.read_line Lwt_io.stdin >|= fun x -> Some (x, ())) ()
     |> Uci.loop
     |> Lwt_seq.iter_s Lwt_io.print

let () = main () |> Lwt_main.run

