open Base
open Hardcaml_waveterm_kernel
module M = Values_window_intf.M

module Make
    (Data : Expert.Data.S)
    (M : Expert.M(Data).S)
    (Hierarchy : Hierarchy.M(Data)(M).S) =
struct
  open M

  type t =
    { hierarchy : Hierarchy.t
    ; mutable max_value_width : int
    ; style : Style.t
    }
  [@@deriving sexp_of, fields ~getters]

  let create ~waves ~hierarchy =
    let max_value_width = Render.get_estimated_max_value_width waves in
    { hierarchy; max_value_width; style = Window_styles.(colour white_on_black).values }
  ;;

  let draw ~ctx ~bounds ~selected_wave_index t =
    let offset = t.hierarchy.cfg.value_scroll in
    t.hierarchy.cfg.value_scroll
    <- max 0 (min (t.max_value_width - 1) (t.max_value_width - offset));
    t.max_value_width
    <- Render.draw_values
         ?wave_cursor:(Cursors.get_offset t.hierarchy.cursors)
         ~style:t.style
         ~selected_wave_index
         ~ctx
         ~bounds
         (Hierarchy.get_currently_rendered_waves t.hierarchy);
    t.hierarchy.cfg.value_scroll <- offset
  ;;
end
