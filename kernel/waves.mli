open Base

module Config : sig
  type t =
    { mutable signals_width : int
    ; mutable values_width : int
    ; mutable wave_width : int
    ; mutable wave_height : int
    ; mutable start_cycle : int
    ; mutable start_signal : int
    ; mutable selected_signal : int
    ; mutable wave_cursor : int
    ; mutable signal_scroll : int
    ; mutable value_scroll : int
    }
  [@@deriving sexp]

  val default : t
end

type t =
  { cfg : Config.t
  ; waves : Wave.t array
  }
[@@deriving sexp_of]
