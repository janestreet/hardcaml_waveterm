open Base

module type S = sig
  module Config : sig
    type t =
      { mutable signals_width : int
      ; mutable values_width : int
      ; mutable wave_width : int
      ; mutable start_cycle : int
      ; mutable start_signal : int
      ; mutable selected_signal : int
      ; mutable signal_scroll : int
      ; mutable value_scroll : int
      }
    [@@deriving sexp]

    val default : t
  end

  type 'data t =
    { cfg : Config.t
    ; waves : 'data Wave.t array
    }
  [@@deriving sexp_of]

  val total_rows : _ t -> int
end

module type Waves = sig
  include S
end
