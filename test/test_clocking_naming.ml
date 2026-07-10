open! Import
module Clocking = Clocking

module I = struct
  type 'a t = { clocking : 'a Clocking.t } [@@deriving hardcaml ~rtlmangle:"$"]
end

module O = struct
  type 'a t = { ctr : 'a [@bits 16] } [@@deriving hardcaml]
end

let create (i : Signal.t I.t) =
  let open Signal in
  let ctr = Clocking.reg_fb i.clocking ~width:16 ~f:(fun fb -> fb +:. 1) in
  { O.ctr }
;;

let%expect_test "clock renders as a clock" =
  let module Sim = Cyclesim.With_interface (I) (O) in
  let sim = Sim.create ~config:Cyclesim.Config.trace_all create in
  let i = Cyclesim.inputs sim in
  let waveform, sim = Cyclesim.Waveform.create sim in
  i.clocking.clear := Bits.vdd;
  Cyclesim.cycle sim;
  i.clocking.clear := Bits.gnd;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  Waveform.print waveform;
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │clock          ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌──│
    │               ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘  │
    │clear          ││────────┐                                          │
    │               ││        └───────────────────────────────           │
    │               ││────────────────┬───────┬───────┬───────           │
    │ctr            ││ 0000           │0001   │0002   │0003              │
    │               ││────────────────┴───────┴───────┴───────           │
    └───────────────┘└───────────────────────────────────────────────────┘
    |}]
;;
