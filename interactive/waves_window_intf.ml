open! Base
open Hardcaml_waveterm_kernel

module M
    (Data : Expert.Data.S)
    (M : Expert.M(Data).S)
    (Hierarchy : Hierarchy.M(Data)(M).S) =
struct
  module type S = sig
    type t [@@deriving sexp_of]

    val style : Style.t
    val create : hierarchy:Hierarchy.t -> unit -> t

    val draw
      :  ctx:Draw.ctx
      -> bounds:Rect.t
      -> selected_wave_index:int option
      -> t
      -> unit
  end
end

module type Waves_window = sig
  module M = M

  module Make
      (Data : Expert.Data.S)
      (Modl : Expert.M(Data).S)
      (Hierarchy : Hierarchy.M(Data)(Modl).S) : M(Data)(Modl)(Hierarchy).S
end
