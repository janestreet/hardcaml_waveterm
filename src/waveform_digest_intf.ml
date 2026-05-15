open Base

module type S = sig
  type t

  val create : Hardcaml.Wave_data.t -> t
  val to_hex_string : t -> string
end

module type Waveform_digest = sig
  module type S = S

  include S
end
