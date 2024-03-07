open Base
open Hardcaml_waveterm_kernel.Expert

module M
  (Data : Data.S)
  (Wave : Wave.M(Data).S)
  (Waves : Waves.M(Data)(Wave).S)
  (Render : Render.M(Data)(Wave)(Waves).S)
  (Waveform : Waveform.M(Data)(Wave)(Waves)(Render).S) =
struct
  module type S = sig
    (** Print waveforms in expect tests. This is very similar to [print] except it always
        outputs to [stdout] and will optionally serialize the waveform to disk for offline
        viewing.

        Waveforms are serialized if:

        - environment variable [EXPECT_TEST_WAVEFORM=1] and the [serialize_to] filename is
          set.
        - [EXPECT_TEST_WAVEFORM=digest] then the waveform file will be named by the digest
          of the waveform ([serialize_to] is not used regardless of if it is set or not).

        Simulation digests are shown by default.
    *)
    val expect
      : (?show_digest:bool -> ?serialize_to:string -> Waveform.t -> unit)
        Waveform.with_options
  end
end

module type Expect = sig
  module M = M

  module Make
    (Data : Data.S)
    (Wave : Wave.M(Data).S)
    (Waves : Waves.M(Data)(Wave).S)
    (Render : Render.M(Data)(Wave)(Waves).S)
    (Waveform : Waveform.M(Data)(Wave)(Waves)(Render).S) :
    M(Data)(Wave)(Waves)(Render)(Waveform).S
end
