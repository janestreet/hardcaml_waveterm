open! Import

module I = struct
  type 'a t = { i : 'a [@bits 4] } [@@deriving hardcaml]
end

module O = struct
  type 'a t = { o : 'a [@bits 4] } [@@deriving hardcaml]
end

let create (i : Signal.t I.t) =
  let open Signal in
  let tmp1 = i.i +: i.i -- "tmp" in
  let tmp2 = tmp1 +: i.i -- "tmp" in
  let tmp3 = tmp2 +: i.i in
  { O.o = tmp3 }
;;

let%expect_test "rtl" =
  let module Circuit = Circuit.With_interface (I) (O) in
  Rtl.print Verilog (Circuit.create_exn ~name:"test" create);
  [%expect
    {|
    module test (
        i,
        o
    );

        input [3:0] i;
        output [3:0] o;

        wire [3:0] signal_wire;
        wire [3:0] tmp;
        wire [3:0] tmp_1;
        wire [3:0] signal_add;
        assign signal_wire = i;
        assign tmp = signal_wire + signal_wire;
        assign tmp_1 = tmp + signal_wire;
        assign signal_add = tmp_1 + signal_wire;
        assign o = signal_add;

    endmodule
    |}]
;;

let%expect_test "waveform" =
  let module Sim = Cyclesim.With_interface (I) (O) in
  let sim = Sim.create ~config:Cyclesim.Config.trace_all create in
  let i = Cyclesim.inputs sim in
  let waveform, sim = Cyclesim.Waveform.create sim in
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  i.i := Bits.of_int_trunc ~width:4 1;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  i.i := Bits.of_int_trunc ~width:4 2;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  Waveform.print waveform;
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │               ││────────────────┬───────────────┬───────────────   │
    │i              ││ 0              │1              │2                 │
    │               ││────────────────┴───────────────┴───────────────   │
    │               ││────────────────┬───────────────┬───────────────   │
    │o              ││ 0              │4              │8                 │
    │               ││────────────────┴───────────────┴───────────────   │
    │               ││────────────────┬───────────────┬───────────────   │
    │tmp            ││ 0              │2              │4                 │
    │               ││────────────────┴───────────────┴───────────────   │
    │               ││────────────────┬───────────────┬───────────────   │
    │tmp_1          ││ 0              │3              │6                 │
    │               ││────────────────┴───────────────┴───────────────   │
    └───────────────┘└───────────────────────────────────────────────────┘
    |}]
;;
