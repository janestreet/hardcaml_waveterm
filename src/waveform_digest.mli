open Base

type t

val create : Hardcaml_waveterm_kernel.Waveform.t -> t
val to_hex_string : t -> string
