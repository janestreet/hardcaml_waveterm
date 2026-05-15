include Base
include Hardcaml
include Expect_test_helpers_core
include Hardcaml_waveterm_kernel
include Hardcaml_waveterm
include Hardcaml_waveterm_interactive
module Data = Hardcaml.Wave_data_in_cycles
module Render = Hardcaml_waveterm_kernel.Render.Make (Data)
