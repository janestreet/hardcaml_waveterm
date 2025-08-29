open Base
open Hardcaml_waveterm_kernel
module M = Signals_window_intf.M

module Make
    (Data : Hardcaml_waveterm_kernel.Expert.Data.S)
    (M : Hardcaml_waveterm_kernel.Expert.M(Data).S)
    (Hierarchy : Hierarchy.M(Data)(M).S) =
struct
  open M

  type t =
    { hierarchy : Hierarchy.t
    ; max_signal_name_width : int
    ; num_signals : int
    ; style : Style.t
    }
  [@@deriving sexp_of, fields ~getters]

  let create ~waves ~hierarchy =
    { hierarchy
    ; max_signal_name_width = Render.get_max_signal_width_in_chars waves
    ; num_signals = Render.total_signals_in_waveform waves
    ; style = Window_styles.(colour white_on_black).signals
    }
  ;;

  let draw ~ctx ~bounds ~selected_wave_index t =
    Render.draw_signals
      ~style:t.style
      ~ctx
      ~bounds
      ~selected_wave_index
      (Hierarchy.get_currently_rendered_waves t.hierarchy)
  ;;
end
