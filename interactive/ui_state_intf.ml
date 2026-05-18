open Base

module M (Data : Hardcaml_waveterm_kernel.Data.S) (Hierarchy : Hierarchy.M(Data).S) =
struct
  module type S = sig
    module Module : sig
      type t = { expanded : bool } [@@deriving sexp]
    end

    type t =
      { cfg : Hardcaml_waveterm_kernel.Waves.Config.t
      ; modules : Module.t Base.Map.M(Base.String).t
      ; cursors : Cursors.t
      }
    [@@deriving sexp]

    val of_hierarchy : Hierarchy.t -> t
    val apply_to : t -> Hierarchy.t -> unit
  end
end

module type Ui_state = sig
  module M = M

  module Make (Data : Hardcaml_waveterm_kernel.Data.S) (Hierarchy : Hierarchy.M(Data).S) :
    M(Data)(Hierarchy).S
end
