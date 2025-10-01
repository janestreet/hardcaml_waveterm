open! Base
open Hardcaml_waveterm_kernel
module M = Waves_window_intf.M

module Make
    (Data : Expert.Data.S)
    (M : Expert.M(Data).S)
    (Hierarchy : Hierarchy.M(Data)(M).S) =
struct
  open M

  type t =
    { hierarchy : Hierarchy.t
    ; style : Style.t
    }
  [@@deriving sexp_of]

  let style = Window_styles.(colour white_on_black).waves
  let create ~hierarchy () = { hierarchy; style }

  let draw ~ctx ~bounds ~selected_wave_index t =
    Render.draw_wave
      ~wave_cursors:(Cursors.get_active_cursors t.hierarchy.cursors)
      ~style:t.style
      ~ctx
      ~bounds
      ~selected_wave_index
      (Hierarchy.get_currently_rendered_waves t.hierarchy)
  ;;
end
