open! Base
open Hardcaml_waveterm_kernel

module M (Data : Data.S) (Hierarchy : Hierarchy.M(Data).S) = struct
  module type S = sig
    type t [@@deriving sexp_of]

    val create : waves:Data.t Waves.t -> hierarchy:Hierarchy.t -> t
    val max_value_width : t -> int

    val draw
      :  ctx:Draw.ctx
      -> bounds:Rect.t
      -> selected_wave_index:int option
      -> t
      -> unit
  end
end

module type Values_window = sig
  module M = M

  module Make
      (Data : Data.S)
      (Render : Hardcaml_waveterm_kernel.Render.M(Data).S)
      (Hierarchy : Hierarchy.M(Data).S) : M(Data)(Hierarchy).S
end
