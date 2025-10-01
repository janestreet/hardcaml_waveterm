open Base
open Hardcaml_waveterm_kernel

module M (Data : Expert.Data.S) (M : Expert.M(Data).S) = struct
  module type S = sig
    module Hierarchy : Hierarchy.M(Data)(M).S

    type t [@@deriving sexp_of]

    val create
      :  ?waveform:t
      -> rows:int
      -> cols:int
      -> ui_state_file:string
      -> M.Waves.t
      -> Key_actions.Key.t Key_actions.t
      -> t

    val draw : ctx:Draw.ctx -> t -> unit

    val handler
      :  t
      -> [ `Key of Notty.Unescape.key | `Mouse of Notty.Unescape.mouse ]
      -> Event_response.t

    val ui_state_file : t -> string
    val default_waves_style : Style.t
    val hierarchy : t -> Hierarchy.t
  end
end

module type Waveform_window = sig
  module M = M
  module Make (Data : Expert.Data.S) (Modl : Expert.M(Data).S) : M(Data)(Modl).S
end
