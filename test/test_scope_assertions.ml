open! Import

module Or_with_enable = struct
  open Signal

  module I = struct
    type 'a t =
      { enable : 'a
      ; a : 'a
      ; b : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { enable_passthrough : 'a
      ; result : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create scope (i : _ I.t) =
    Scope.add_assertion scope "enabled means it is being used" (i.enable -->: (i.a |: i.b));
    let o = { O.enable_passthrough = i.enable; result = i.enable &: (i.a |: i.b) } in
    Scope.add_assertion scope "enable passthrough" (i.enable ==: o.enable_passthrough);
    Scope.add_assertion scope "result implies an input is one" (o.result -->: (i.a |: i.b));
    o
  ;;

  module Sim = Cyclesim.With_interface (I) (O)
end

let%expect_test "scope with assertions" =
  let scope = Scope.create ~flatten_design:true ~trace_properties:true () in
  let sim =
    Or_with_enable.Sim.create
      ~config:Cyclesim.Config.trace_all
      ~circuit_config:
        { Circuit.Config.default with
          assertions = Some (Scope.assertion_manager scope)
        ; port_checks = Relaxed
        }
      (Or_with_enable.create scope)
  in
  let inputs = Cyclesim.inputs sim in
  let waveform, sim = Waveform.create sim in
  inputs.a := Bits.of_int ~width:1 0;
  inputs.b := Bits.of_int ~width:1 1;
  Cyclesim.cycle sim;
  inputs.enable := Bits.of_int ~width:1 1;
  Cyclesim.cycle sim;
  inputs.b := Bits.of_int ~width:1 0;
  Cyclesim.cycle sim;
  Waveform.expect ~serialize_to:"scope_with_assertions" waveform;
  Stdio.print_s
    [%message
      (Cyclesim.results_of_assertions sim : Cyclesim.Violated_or_not.t Map.M(String).t)];
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
    2566b737c917e3e75e1471ad3443df93
    ("Cyclesim.results_of_assertions sim"
     (("enable passthrough" Not_violated)
      ("enabled means it is being used" (Violated (2)))
      ("result implies an input is one" Not_violated))) |}]
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
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { enable_passthrough : 'a
      ; result : 'a
      }
    [@@deriving sexp_of, hardcaml]
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
                    ; Scope.assert_signal_in_always
                        scope
                        "assert (nested false)"
                        Signal.gnd
                    ] )
                ; Signal.of_int ~width:2 1, [ result <-- (i.a &: i.b) ]
                ; Signal.of_int ~width:2 2, [ result <-- i.a ^: i.b ]
                ; ( Signal.of_int ~width:2 3
                  , [ result <-- ~:(i.a &: i.b)
                    ; Scope.assert_signal_in_always
                        scope
                        "assert (nested true)"
                        Signal.vdd
                    ] )
                ]
            ; Scope.assert_signal_in_always scope "enable -> ~foo" ~:(i.foo)
            ]
            [ result <-- i.foo
            ; Scope.assert_signal_in_always scope "~enable -> foo" i.foo
            ]
        ; enable_passthrough <-- i.enable
        ; Scope.assert_signal_in_always scope "assert enable" i.enable
        ; Scope.assert_signal_in_always scope "assert false" Signal.gnd
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
      ~circuit_config:
        { Circuit.Config.default with
          assertions = Some (Scope.assertion_manager scope)
        ; port_checks = Relaxed
        }
      (Operator_operation.create_with_always scope)
  in
  let inputs = Cyclesim.inputs sim in
  let waveform, sim = Waveform.create sim in
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
  Stdio.print_s
    [%message
      (Cyclesim.results_of_assertions sim : Cyclesim.Violated_or_not.t Map.M(String).t)];
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
    2e164e77e11658e1d5f15740141feb1d
    ("Cyclesim.results_of_assertions sim"
     (("assert (nested false)" (Violated (1)))
      ("assert (nested true)" Not_violated) ("assert enable" (Violated (0 4)))
      ("assert false" (Violated (0 1 2 3 4))) ("enable -> ~foo" Not_violated)
      ("~enable -> foo" (Violated (0))))) |}]
;;

let%expect_test "assertions checked to be 1 bit" =
  let scope = Scope.create ~flatten_design:true ~trace_properties:true () in
  require_does_raise [%here] (fun () ->
    Always.(
      compile
        [ Scope.assert_signal_in_always scope "oops - 2 bit assertion" (Signal.zero 2) ]));
  [%expect
    {|
    ("attempt to assign expression to [Always.variable] of different width"
     (variable_name ())
     (guared_variable_width 1)
     (expression_width      2)
     (expression (
       const
       (width 2)
       (value 0b00)))) |}]
;;
