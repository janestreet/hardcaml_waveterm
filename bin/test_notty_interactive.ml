(* Test the notty based waveterm interactive viewer. *)
open! Import
open! Core
open! Async

let () =
  Command.async
    ~summary:"Test fullscreen interactive viewer"
    (let open Command.Let_syntax in
     let%map_open () = return () in
     fun () -> Widget.run_waves Test_data.(create ~length:1000 ~num_signals:100))
  |> Command.run
;;
