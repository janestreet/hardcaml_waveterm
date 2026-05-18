open Base
open Hardcaml_waveterm_kernel

module type S = sig
  (** Print waveforms in expect tests. This is very similar to [print] except it always
      outputs to [stdout] and will optionally serialize the waveform to disk for offline
      viewing.

      Waveforms are serialized if:

      - environment variable [EXPECT_TEST_WAVEFORM=1] and the [serialize_to] filename is
        set.
      - [EXPECT_TEST_WAVEFORM=digest] then the waveform file will be named by the digest
        of the waveform ([serialize_to] is not used regardless of if it is set or not).

      Simulation digests are shown by default. *)
  val expect
    : (?show_digest:bool -> ?serialize_to:string -> Hardcaml.Wave_data.t -> unit)
        Waveform.with_options

  (** Same as [expect] expect it prints a newline first, and defaults to 90 chars wide.
      For use with [%expect_exact] in tests. *)
  val expect_exact
    : (?show_digest:bool -> ?serialize_to:string -> Hardcaml.Wave_data.t -> unit)
        Waveform.with_options
end

module type Expect = sig
  module type S = S

  include S
end
