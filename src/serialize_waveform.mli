open Hardcaml_waveterm_kernel.Waveform

val marshall : t -> string -> unit
val unmarshall : string -> t

(** Print waveforms in expect tests. This is very similar to [print] except it always
    outputs to [stdout] and will optionally serialize the waveform to disk for offline
    viewing if the environment variable [EXPECT_TEST_WAVEFORM=1] and the [serialize_to]
    filename is set.

    Simulation digests are shown by default.
*)
val expect : (?show_digest:bool -> ?serialize_to:string -> t -> unit) with_options

(** For testing marshalling functions. *)
val equal : t -> t -> bool
