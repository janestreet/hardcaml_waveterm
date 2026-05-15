(* Quick test of notty rendering to the terminal *)
open! Import

let ctx =
  Render.Static.draw
    ~signals:true
    ~values:true
    ~waves:true
    ~style:Window_styles.colour_on_black
    ~rows:20
    ~cols:80
    { cfg = Waves.Config.default
    ; waves =
        Test_data.create ~prefix:(fun _ -> "") ~length:20 ~num_signals:10 ~max_bits:64
        |> Waveform.sort_ports_and_formats _ (Some [ Default ])
    }
;;

let image = Draw_notty.to_image ctx
let () = Notty_unix.output_image image
