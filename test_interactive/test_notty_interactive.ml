(* Test the notty based waveterm interactive viewer. *)
open! Import
open! Core
open! Async

let () =
  Command.async
    ~summary:"Test fullscreen interactive viewer"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and signals_width =
       flag
         "-signals-width"
         (optional_with_default 20 int)
         ~doc:"<int> signals window width"
     and values_width =
       flag
         "-values-width"
         (optional_with_default 20 int)
         ~doc:"<int> values window width"
     in
     fun () ->
       Random.init 42;
       let waves =
         Test_data.create ~prefix:(fun _ -> "") ~length:1000 ~num_signals:1000
       in
       run_waves { waves with cfg = { waves.cfg with signals_width; values_width } })
    ~behave_nicely_in_pipeline:false
  |> Command_unix.run
;;
