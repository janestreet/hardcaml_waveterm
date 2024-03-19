open! Import

module Or_with_enable = struct
  open Signal

  module I = struct
    type 'a t =
      { enable : 'a
      ; a : 'a
      ; b : 'a
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { enable_passthrough : 'a
      ; result : 'a
      }
    [@@deriving hardcaml]
  end

  let create scope (i : _ I.t) =
    Assertions.add scope "enabled means it is being used" (i.enable -->: (i.a |: i.b));
    let o = { O.enable_passthrough = i.enable; result = i.enable &: (i.a |: i.b) } in
    Assertions.add scope "enable passthrough" (i.enable ==: o.enable_passthrough);
    Assertions.add scope "result implies an input is one" (o.result -->: (i.a |: i.b));
    o
  ;;

  module Sim = Cyclesim.With_interface (I) (O)
end

let%expect_test "scope with assertions" =
  let scope = Scope.create ~flatten_design:true ~trace_properties:true () in
  let sim =
    Or_with_enable.Sim.create
      ~config:Cyclesim.Config.trace_all
      ~circuit_config:(Assertions.circuit_config_with_assertions scope)
      (Or_with_enable.create scope)
  in
  let inputs = Cyclesim.inputs sim in
  let waveform, sim = Waveform.create sim in
  let assertions, sim = Assertions.trace sim (Scope.assertion_manager scope) in
  inputs.a := Bits.of_int ~width:1 0;
  inputs.b := Bits.of_int ~width:1 1;
  Cyclesim.cycle sim;
  inputs.enable := Bits.of_int ~width:1 1;
  Cyclesim.cycle sim;
  inputs.b := Bits.of_int ~width:1 0;
  Cyclesim.cycle sim;
  Waveform.expect ~serialize_to:"scope_with_assertions" waveform;
  Stdio.print_s [%message (assertions : Assertions.t)];
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │enable         ││        ┌───────────────                           │
    │               ││────────┘                                          │
    │a              ││                                                   │
    │               ││────────────────────────                           │
    │b              ││────────────────┐                                  │
    │               ││                └───────                           │
    │enable passthro││────────────────────────                           │
    │               ││                                                   │
    │enable_passthro││        ┌───────────────                           │
    │               ││────────┘                                          │
    │enabled means i││────────────────┐                                  │
    │               ││                └───────                           │
    │result         ││        ┌───────┐                                  │
    │               ││────────┘       └───────                           │
    │result implies ││────────────────────────                           │
    │               ││                                                   │
    │               ││                                                   │
    │               ││                                                   │
    └───────────────┘└───────────────────────────────────────────────────┘
    53e55d03bc60aca3f7151f9b3094123e
    (assertions
     ((("enable passthrough" Not_violated)
       ("enabled means it is being used" (Violated (2)))
       ("result implies an input is one" Not_violated))))
    |}]
;;

module Operator_operation = struct
  open Signal

  module I = struct
    type 'a t =
      { enable : 'a
      ; op : 'a [@bits 2]
      ; a : 'a
      ; b : 'a
      ; foo : 'a
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { enable_passthrough : 'a
      ; result : 'a
      }
    [@@deriving hardcaml]
  end

  let create_with_always scope (i : _ I.t) =
    let result = Always.Variable.wire ~default:gnd in
    let enable_passthrough = Always.Variable.wire ~default:gnd in
    Always.(
      compile
        [ if_
            i.enable
            [ switch
                i.op
                [ ( Signal.of_int ~width:2 0
                  , [ result <-- (i.a |: i.b)
                    ; Assertions.Always.add scope "assert (nested false)" Signal.gnd
                    ] )
                ; Signal.of_int ~width:2 1, [ result <-- (i.a &: i.b) ]
                ; Signal.of_int ~width:2 2, [ result <-- i.a ^: i.b ]
                ; ( Signal.of_int ~width:2 3
                  , [ result <-- ~:(i.a &: i.b)
                    ; Assertions.Always.add scope "assert (nested true)" Signal.vdd
                    ] )
                ]
            ; Assertions.Always.add scope "enable -> ~foo" ~:(i.foo)
            ]
            [ result <-- i.foo; Assertions.Always.add scope "~enable -> foo" i.foo ]
        ; enable_passthrough <-- i.enable
        ; Assertions.Always.add scope "assert enable" i.enable
        ; Assertions.Always.add scope "assert false" Signal.gnd
        ]);
    { O.enable_passthrough = Always.Variable.value enable_passthrough
    ; result = Always.Variable.value result
    }
  ;;

  module Sim = Cyclesim.With_interface (I) (O)
end

let%expect_test "scope always with assertions" =
  let scope = Scope.create ~flatten_design:true ~trace_properties:true () in
  let sim =
    Operator_operation.Sim.create
      ~circuit_config:(Assertions.circuit_config_with_assertions scope)
      (Operator_operation.create_with_always scope)
  in
  let inputs = Cyclesim.inputs sim in
  let waveform, sim = Waveform.create sim in
  let assertions, sim = Assertions.trace sim (Scope.assertion_manager scope) in
  inputs.a := Bits.of_int ~width:1 0;
  inputs.b := Bits.of_int ~width:1 1;
  Cyclesim.cycle sim;
  inputs.enable := Bits.of_int ~width:1 1;
  inputs.op := Bits.of_int ~width:2 0;
  Cyclesim.cycle sim;
  inputs.op := Bits.of_int ~width:2 1;
  Cyclesim.cycle sim;
  inputs.op := Bits.of_int ~width:2 2;
  Cyclesim.cycle sim;
  inputs.enable := Bits.of_int ~width:1 0;
  inputs.foo := Bits.of_int ~width:1 1;
  Cyclesim.cycle sim;
  Waveform.expect
    ~display_width:70
    ~display_height:34
    ~serialize_to:"scope_with_assertions_using_always_api"
    waveform;
  Stdio.print_s [%message (assertions : Assertions.t)];
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │enable         ││        ┌───────────────────────┐                  │
    │               ││────────┘                       └───────           │
    │a              ││                                                   │
    │               ││────────────────────────────────────────           │
    │b              ││────────────────────────────────────────           │
    │               ││                                                   │
    │foo            ││                                ┌───────           │
    │               ││────────────────────────────────┘                  │
    │               ││────────────────┬───────┬───────────────           │
    │op             ││ 0              │1      │2                         │
    │               ││────────────────┴───────┴───────────────           │
    │assert (nested ││────────┐       ┌───────────────────────           │
    │               ││        └───────┘                                  │
    │assert (nested ││────────────────────────────────────────           │
    │               ││                                                   │
    │assert enable  ││        ┌───────────────────────┐                  │
    │               ││────────┘                       └───────           │
    │assert false   ││                                                   │
    │               ││────────────────────────────────────────           │
    │enable -> ~foo ││────────────────────────────────────────           │
    │               ││                                                   │
    │enable_passthro││        ┌───────────────────────┐                  │
    │               ││────────┘                       └───────           │
    │result         ││        ┌───────┐       ┌───────────────           │
    │               ││────────┘       └───────┘                          │
    │~enable -> foo ││        ┌───────────────────────────────           │
    │               ││────────┘                                          │
    │               ││                                                   │
    │               ││                                                   │
    │               ││                                                   │
    │               ││                                                   │
    │               ││                                                   │
    └───────────────┘└───────────────────────────────────────────────────┘
    d55aa079985ec7093fb9c721f9db0b34
    (assertions
     ((("assert (nested false)" (Violated (1)))
       ("assert (nested true)" Not_violated) ("assert enable" (Violated (0 4)))
       ("assert false" (Violated (0 1 2 3 4))) ("enable -> ~foo" Not_violated)
       ("~enable -> foo" (Violated (0))))))
    |}]
;;

let%expect_test "assertions checked to be 1 bit" =
  let scope = Scope.create ~flatten_design:true ~trace_properties:true () in
  require_does_raise [%here] (fun () ->
    Assertions.add scope "oops - 2 bit assertion" (Signal.zero 2));
  [%expect
    {|
    ("Assertion signals must be 1 bit"
      (name "oops - 2 bit assertion")
      (assertion (
        const
        (width 2)
        (value 0b00))))
    |}];
  let scope = Scope.create ~flatten_design:true ~trace_properties:true () in
  require_does_raise [%here] (fun () ->
    Always.(
      compile [ Assertions.Always.add scope "oops - 2 bit assertion" (Signal.zero 2) ]));
  [%expect
    {|
    ("Assertion signals must be 1 bit"
      (name "oops - 2 bit assertion")
      (assertion (
        const
        (width 2)
        (value 0b00))))
    |}]
;;
