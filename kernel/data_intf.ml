(** Waveform data interface. *)

module type S = sig
  type t [@@deriving sexp_of, equal]

  val width : t -> int
  val length : t -> int
  val get : t -> int -> Hardcaml.Bits.t
  val get_digestible_string : t -> Bytes.t * int
end

module type Data = sig
  module type S = S
end
