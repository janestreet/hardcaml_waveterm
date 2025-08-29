open Base

module M
    (Data : Hardcaml_waveterm_kernel.Expert.Data.S)
    (M : Hardcaml_waveterm_kernel.Expert.M(Data).S)
    (Hierarchy : Hierarchy.M(Data)(M).S) =
struct
  module type S = sig
    module Module : sig
      type t = { expanded : bool } [@@deriving sexp]
    end

    type t =
      { cfg : M.Waves.Config.t
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

  module Make
      (Data : Hardcaml_waveterm_kernel.Expert.Data.S)
      (Modl : Hardcaml_waveterm_kernel.Expert.M(Data).S)
      (Hierarchy : Hierarchy.M(Data)(Modl).S) : M(Data)(Modl)(Hierarchy).S
end
