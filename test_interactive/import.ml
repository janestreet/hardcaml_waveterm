include Base
include Expect_test_helpers_core
include Hardcaml_waveterm_kernel
include Hardcaml_waveterm_interactive
module Test_data = Hardcaml_waveterm_test.Test_data
module Example = Hardcaml_waveterm_test.Example
module Data = Hardcaml.Wave_data_in_cycles
module Render = Hardcaml_waveterm_kernel.Render.Make (Data)
