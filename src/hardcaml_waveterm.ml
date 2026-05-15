(** {!Hardcaml_waveterm} is a library for displaying terminal-based waveforms from
    hardcaml simulations. *)

include Hardcaml_waveterm_kernel

module Waveform = struct
  include Waveform
  module Serialize = Serialize_waveform
  module Digest = Waveform_digest

  let expect = Expect.expect
  let expect_exact = Expect.expect_exact
end
