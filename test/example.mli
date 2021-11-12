(** Example circuit and testbench **)

open! Import

module I : sig
  type 'a t =
    { clk : 'a
    ; clr : 'a
    ; a : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module O : sig
  type 'a t =
    { b : 'a
    ; output_c_with_a_long_name : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

val create : Signal.t I.t -> Signal.t O.t
val testbench : unit -> Waveform.t
