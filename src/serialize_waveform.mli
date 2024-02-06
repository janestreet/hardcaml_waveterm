open Hardcaml_waveterm_kernel.Waveform

val marshall : t -> string -> unit
val unmarshall : string -> t

(** For testing marshalling functions. *)
val equal : t -> t -> bool
