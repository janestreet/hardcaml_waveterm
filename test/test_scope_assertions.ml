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
    Assertions.add scope "enabled_means_it_is_being_used" (i.enable -->: (i.a |: i.b));
    let o = { O.enable_passthrough = i.enable; result = i.enable &: (i.a |: i.b) } in
    Assertions.add scope "enable_passthrough_assert" (i.enable ==: o.enable_passthrough);
    Assertions.add scope "result_implies_an_input_is_one" (o.result -->: (i.a |: i.b));
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
  let waveform, sim = Cyclesim.Waveform.create sim in
  let assertions, sim = Assertions.trace sim (Scope.assertion_manager scope) in
  inputs.a := Bits.of_int_trunc ~width:1 0;
  inputs.b := Bits.of_int_trunc ~width:1 1;
  Cyclesim.cycle sim;
  inputs.enable := Bits.of_int_trunc ~width:1 1;
  Cyclesim.cycle sim;
  inputs.b := Bits.of_int_trunc ~width:1 0;
  Cyclesim.cycle sim;
  Waveform.expect ~signals_width:30 ~serialize_to:"scope_with_assertions" waveform;
  Stdio.print_s [%message (assertions : Assertions.t)];
  [%expect
    {|
    ┌Signals─────────────────────┐┌Waves─────────────────────────────────┐
    │enable                      ││        ┌───────────────              │
    │                            ││────────┘                             │
    │a                           ││                                      │
    │                            ││────────────────────────              │
    │b                           ││────────────────┐                     │
    │                            ││                └───────              │
    │enable_passthrough          ││        ┌───────────────              │
    │                            ││────────┘                             │
    │result                      ││        ┌───────┐                     │
    │                            ││────────┘       └───────              │
    │enable_passthrough_assert   ││────────────────────────              │
    │                            ││                                      │
    │enabled_means_it_is_being_us││────────────────┐                     │
    │                            ││                └───────              │
    │result_implies_an_input_is_o││────────────────────────              │
    │                            ││                                      │
    └────────────────────────────┘└──────────────────────────────────────┘
    506341df454732fc001f2b86fa8ca25a
    (assertions
     (((enable_passthrough_assert Not_violated)
       (enabled_means_it_is_being_used (Violated (2)))
       (result_implies_an_input_is_one Not_violated))))
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
    let result = Always.Variable.wire ~default:gnd () in
    let enable_passthrough = Always.Variable.wire ~default:gnd () in
    Always.(
      compile
        [ if_
            i.enable
            [ switch
                i.op
                [ ( Signal.of_int_trunc ~width:2 0
                  , [ result <-- (i.a |: i.b)
                    ; Assertions.Always.add scope "assert_(nested_false)" Signal.gnd
                    ] )
                ; Signal.of_int_trunc ~width:2 1, [ result <-- (i.a &: i.b) ]
                ; Signal.of_int_trunc ~width:2 2, [ result <-- i.a ^: i.b ]
                ; ( Signal.of_int_trunc ~width:2 3
                  , [ result <-- ~:(i.a &: i.b)
                    ; Assertions.Always.add scope "assert_(nested_true)" Signal.vdd
                    ] )
                ]
            ; Assertions.Always.add scope "enable_->_~foo" ~:(i.foo)
            ]
            [ result <-- i.foo; Assertions.Always.add scope "~enable_->_foo" i.foo ]
        ; enable_passthrough <-- i.enable
        ; Assertions.Always.add scope "assert_enable" i.enable
        ; Assertions.Always.add scope "assert_false" Signal.gnd
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
  let waveform, sim = Cyclesim.Waveform.create sim in
  let assertions, sim = Assertions.trace sim (Scope.assertion_manager scope) in
  inputs.a := Bits.of_int_trunc ~width:1 0;
  inputs.b := Bits.of_int_trunc ~width:1 1;
  Cyclesim.cycle sim;
  inputs.enable := Bits.of_int_trunc ~width:1 1;
  inputs.op := Bits.of_int_trunc ~width:2 0;
  Cyclesim.cycle sim;
  inputs.op := Bits.of_int_trunc ~width:2 1;
  Cyclesim.cycle sim;
  inputs.op := Bits.of_int_trunc ~width:2 2;
  Cyclesim.cycle sim;
  inputs.enable := Bits.of_int_trunc ~width:1 0;
  inputs.foo := Bits.of_int_trunc ~width:1 1;
  Cyclesim.cycle sim;
  Waveform.expect
    ~signals_width:20
    ~display_width:70
    ~serialize_to:"scope_with_assertions_using_always_api"
    waveform;
  Stdio.print_s [%message (assertions : Assertions.t)];
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────┐
    │enable            ││        ┌───────────────────────┐               │
    │                  ││────────┘                       └───────        │
    │                  ││────────────────┬───────┬───────────────        │
    │op                ││ 0              │1      │2                      │
    │                  ││────────────────┴───────┴───────────────        │
    │a                 ││                                                │
    │                  ││────────────────────────────────────────        │
    │b                 ││────────────────────────────────────────        │
    │                  ││                                                │
    │foo               ││                                ┌───────        │
    │                  ││────────────────────────────────┘               │
    │enable_passthrough││        ┌───────────────────────┐               │
    │                  ││────────┘                       └───────        │
    │result            ││        ┌───────┐       ┌───────────────        │
    │                  ││────────┘       └───────┘                       │
    │assert_(nested_fal││────────┐       ┌───────────────────────        │
    │                  ││        └───────┘                               │
    │assert_(nested_tru││────────────────────────────────────────        │
    │                  ││                                                │
    │assert_enable     ││        ┌───────────────────────┐               │
    │                  ││────────┘                       └───────        │
    │assert_false      ││                                                │
    │                  ││────────────────────────────────────────        │
    │enable_->_~foo    ││────────────────────────────────────────        │
    │                  ││                                                │
    │~enable_->_foo    ││        ┌───────────────────────────────        │
    │                  ││────────┘                                       │
    └──────────────────┘└────────────────────────────────────────────────┘
    d55aa079985ec7093fb9c721f9db0b34
    (assertions
     ((("assert_(nested_false)" (Violated (1)))
       ("assert_(nested_true)" Not_violated) (assert_enable (Violated (0 4)))
       (assert_false (Violated (0 1 2 3 4))) (enable_->_~foo Not_violated)
       (~enable_->_foo (Violated (0))))))
    |}]
;;

let%expect_test "assertions checked to be 1 bit" =
  let scope = Scope.create ~flatten_design:true ~trace_properties:true () in
  require_does_raise (fun () ->
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
  require_does_raise (fun () ->
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
