include Base
include Hardcaml
include Expect_test_helpers_core
include Hardcaml_waveterm_kernel
include Hardcaml_waveterm
include Hardcaml_waveterm_interactive

module Data = struct
  include Hardcaml.Wave_data_in_mutable_events

  let init length ~width ~f =
    let t = create width (ref (length - 1)) in
    if length > 0
    then (
      let previous = ref (f 0) in
      insert t 0 !previous;
      for time = 1 to length - 1 do
        let current = f time in
        if not (Bits.equal current !previous)
        then (
          insert t time current;
          previous := current)
      done);
    t
  ;;
end

module Render = Hardcaml_waveterm_kernel.Render.Make (Data)
