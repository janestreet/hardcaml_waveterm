open! Import

let testbench = lazy (Example.testbench ())

let test
  ?display_rules
  ?display_width
  ?display_height
  ?wave_width
  ?signals_width
  ?signals_alignment
  ()
  =
  Waveform.expect
    (Lazy.force testbench)
    ?display_rules
    ?display_width
    ?display_height
    ?wave_width
    ?signals_width
    ?signals_alignment
;;

let%expect_test "default" =
  test ();
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │clk            ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌──│
    │               ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘  │
    │clr            ││────────┐                                          │
    │               ││        └───────────────────────────────           │
    │               ││────────┬───────┬───────────────────────           │
    │a              ││ 0000   │0017   │002D                              │
    │               ││────────┴───────┴───────────────────────           │
    │               ││────────────────┬───────┬───────────────           │
    │b              ││ 0000           │0018   │002E                      │
    │               ││────────────────┴───────┴───────────────           │
    │output_c_with_a││                                                   │
    │               ││────────────────────────────────────────           │
    └───────────────┘└───────────────────────────────────────────────────┘
    8810362927e35cce94c7652659a13137
    |}]
;;

let%expect_test "display height" =
  test () ~display_height:6;
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │clk            ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌──│
    │               ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘  │
    │clr            ││────────┐                                          │
    │               ││        └───────────────────────────────           │
    └───────────────┘└───────────────────────────────────────────────────┘
    8810362927e35cce94c7652659a13137
    |}]
;;

let%expect_test "display width" =
  test () ~display_width:40;
  [%expect
    {|
    ┌Signals─┐┌Waves───────────────────────┐
    │clk     ││┌───┐   ┌───┐   ┌───┐   ┌───│
    │        ││    └───┘   └───┘   └───┘   │
    │clr     ││────────┐                   │
    │        ││        └───────────────────│
    │        ││────────┬───────┬───────────│
    │a       ││ 0000   │0017   │002D       │
    │        ││────────┴───────┴───────────│
    │        ││────────────────┬───────┬───│
    │b       ││ 0000           │0018   │002│
    │        ││────────────────┴───────┴───│
    │output_c││                            │
    │        ││────────────────────────────│
    └────────┘└────────────────────────────┘
    8810362927e35cce94c7652659a13137
    |}]
;;

let%expect_test "negative wave width" =
  test () ~wave_width:(-1) ~display_height:13;
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │clk            ││╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥│
    │               ││╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨│
    │clr            ││─┐                                                 │
    │               ││ └───                                              │
    │               ││─┬┬──                                              │
    │a              ││ ││0.                                              │
    │               ││─┴┴──                                              │
    │               ││──┬┬─                                              │
    │b              ││ .││.                                              │
    │               ││──┴┴─                                              │
    │output_c_with_a││                                                   │
    └───────────────┘└───────────────────────────────────────────────────┘
    8810362927e35cce94c7652659a13137
    |}]
;;

let%expect_test "display rules" =
  let display_rules =
    Display_rule.
      [ port_name_is "clk" ~wave_format:Binary
      ; port_name_matches
          (Re.Posix.compile (Re.Posix.re ".d.*"))
          ~wave_format:Unsigned_int
      ; port_name_is_one_of [ "b"; "a" ] ~wave_format:Int
      ; port_name_is "clr" ~wave_format:(Index [ "run"; "clear" ])
      ]
  in
  print_s [%message "" (display_rules : Display_rules.t)];
  [%expect
    {|
    (display_rules (
      (Names
        (names       (clk))
        (wave_format (Binary))
        (alignment Left))
      (Regexp (re <opaque>) (wave_format (Unsigned_int)) (alignment Left))
      (Names (names (b a)) (wave_format (Int)) (alignment Left))
      (Names (names (clr)) (wave_format ((Index (run clear)))) (alignment Left))))
    |}];
  test () ~display_rules ~display_height:16;
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │clk            ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌──│
    │               ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘  │
    │               ││────────┬───────┬───────────────────────           │
    │a              ││ 0      │23     │45                                │
    │               ││────────┴───────┴───────────────────────           │
    │               ││────────────────┬───────┬───────────────           │
    │b              ││ 0              │24     │46                        │
    │               ││────────────────┴───────┴───────────────           │
    │               ││────────┬───────────────────────────────           │
    │clr            ││ clear  │run                                       │
    │               ││────────┴───────────────────────────────           │
    │               ││                                                   │
    │               ││                                                   │
    │               ││                                                   │
    └───────────────┘└───────────────────────────────────────────────────┘
    8810362927e35cce94c7652659a13137
    |}]
;;

let%expect_test "config with outputs then inputs" =
  let names (module X : Interface.S) = X.to_list X.port_names in
  let map_format wave_format =
    List.map ~f:(fun name -> Display_rule.port_name_is name ~wave_format)
  in
  let o = names (module Example.O) |> map_format Unsigned_int in
  let i = names (module Example.I) |> map_format Hex in
  let display_rules = o @ i in
  test () ~display_rules ~display_height:13;
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │               ││────────────────┬───────┬───────────────           │
    │b              ││ 0              │24     │46                        │
    │               ││────────────────┴───────┴───────────────           │
    │               ││────────────────────────────────────────           │
    │output_c_with_a││ 0                                                 │
    │               ││────────────────────────────────────────           │
    │clk            ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌──│
    │               ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘  │
    │               ││────────┬───────────────────────────────           │
    │clr            ││ 1      │0                                         │
    │               ││────────┴───────────────────────────────           │
    └───────────────┘└───────────────────────────────────────────────────┘
    8810362927e35cce94c7652659a13137
    |}]
;;

let%expect_test "single bits" =
  let display_rules = [ Display_rule.port_name_is "clr" ~wave_format:Binary ] in
  test () ~display_rules ~display_height:5;
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │               ││────────┬───────────────────────────────           │
    │clr            ││ 1      │0                                         │
    │               ││────────┴───────────────────────────────           │
    └───────────────┘└───────────────────────────────────────────────────┘
    8810362927e35cce94c7652659a13137
    |}];
  let display_rules = [ Display_rule.port_name_is "clr" ~wave_format:Bit ] in
  test () ~display_rules ~display_height:4;
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │clr            ││────────┐                                          │
    │               ││        └───────────────────────────────           │
    └───────────────┘└───────────────────────────────────────────────────┘
    8810362927e35cce94c7652659a13137
    |}]
;;

let%expect_test "Bit_or constructor" =
  let display_rules =
    Display_rule.
      [ port_name_is "clr" ~wave_format:(Bit_or Hex)
      ; port_name_is "a" ~wave_format:(Bit_or Hex)
      ; port_name_is "b" ~wave_format:(Bit_or Unsigned_int)
      ]
  in
  test () ~display_rules ~display_height:10;
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │clr            ││────────┐                                          │
    │               ││        └───────────────────────────────           │
    │               ││────────┬───────┬───────────────────────           │
    │a              ││ 0000   │0017   │002D                              │
    │               ││────────┴───────┴───────────────────────           │
    │               ││────────────────┬───────┬───────────────           │
    │b              ││ 0              │24     │46                        │
    │               ││────────────────┴───────┴───────────────           │
    └───────────────┘└───────────────────────────────────────────────────┘
    8810362927e35cce94c7652659a13137
    |}]
;;

let%expect_test "Alignment" =
  let display_rules alignment =
    Display_rule.
      [ port_name_is "clr" ~wave_format:(Bit_or Hex)
      ; port_name_is "a" ~wave_format:Hex ~alignment
      ; port_name_is "b" ~wave_format:(Bit_or Unsigned_int)
      ]
  in
  test () ~display_rules:(display_rules Right) ~display_height:10 ~wave_width:1;
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │clr            ││────┐                                              │
    │               ││    └───────────────                               │
    │               ││────┬───┬───────────                               │
    │a              ││ .00│.17│002D                                      │
    │               ││────┴───┴───────────                               │
    │               ││────────┬───┬───────                               │
    │b              ││ 0      │24 │46                                    │
    │               ││────────┴───┴───────                               │
    └───────────────┘└───────────────────────────────────────────────────┘
    8810362927e35cce94c7652659a13137
    |}];
  test () ~display_rules:(display_rules Left) ~display_height:10 ~wave_width:1;
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │clr            ││────┐                                              │
    │               ││    └───────────────                               │
    │               ││────┬───┬───────────                               │
    │a              ││ 00.│00.│002D                                      │
    │               ││────┴───┴───────────                               │
    │               ││────────┬───┬───────                               │
    │b              ││ 0      │24 │46                                    │
    │               ││────────┴───┴───────                               │
    └───────────────┘└───────────────────────────────────────────────────┘
    8810362927e35cce94c7652659a13137
    |}]
;;

let%expect_test "minimum display size" =
  test () ~display_width:7 ~display_height:3;
  [%expect
    {|
    ┌S┐┌Wa┐
    │c││┌─│
    └─┘└──┘
    8810362927e35cce94c7652659a13137
    |}]
;;

let%expect_test "Custom signals width" =
  test () ~signals_width:10;
  [%expect
    {|
    ┌Signals─┐┌Waves─────────────────────────────────────────────────────┐
    │clk     ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌─│
    │        ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘ │
    │clr     ││────────┐                                                 │
    │        ││        └───────────────────────────────                  │
    │        ││────────┬───────┬───────────────────────                  │
    │a       ││ 0000   │0017   │002D                                     │
    │        ││────────┴───────┴───────────────────────                  │
    │        ││────────────────┬───────┬───────────────                  │
    │b       ││ 0000           │0018   │002E                             │
    │        ││────────────────┴───────┴───────────────                  │
    │output_c││                                                          │
    │        ││────────────────────────────────────────                  │
    └────────┘└──────────────────────────────────────────────────────────┘
    8810362927e35cce94c7652659a13137
    |}];
  test () ~signals_width:25;
  [%expect
    {|
    ┌Signals────────────────┐┌Waves──────────────────────────────────────┐
    │clk                    ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌──│
    │                       ││    └───┘   └───┘   └───┘   └───┘   └───┘  │
    │clr                    ││────────┐                                  │
    │                       ││        └───────────────────────────────   │
    │                       ││────────┬───────┬───────────────────────   │
    │a                      ││ 0000   │0017   │002D                      │
    │                       ││────────┴───────┴───────────────────────   │
    │                       ││────────────────┬───────┬───────────────   │
    │b                      ││ 0000           │0018   │002E              │
    │                       ││────────────────┴───────┴───────────────   │
    │output_c_with_a_long_na││                                           │
    │                       ││────────────────────────────────────────   │
    └───────────────────────┘└───────────────────────────────────────────┘
    8810362927e35cce94c7652659a13137
    |}]
;;

let%expect_test "configuration exceptions" =
  show_raise (fun () -> test () ~display_height:2);
  [%expect {| (raised ("Invalid display height.  Must be >= 3." (display_height 2))) |}];
  show_raise (fun () -> test () ~display_width:6);
  [%expect {| (raised ("Invalid display width.  Must be >= 7." (display_width 6))) |}];
  show_raise (fun () -> test () ~signals_width:28 ~display_width:28);
  [%expect
    {|
    (raised (
      "Invalid signals_width. Require signals_width < display_width."
      (signals_width 28)
      (display_width 28)))
    |}]
;;

let%expect_test "right signals alignment" =
  test () ~signals_alignment:Right ~display_height:16;
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │            clk││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌──│
    │               ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘  │
    │            clr││────────┐                                          │
    │               ││        └───────────────────────────────           │
    │               ││────────┬───────┬───────────────────────           │
    │              a││ 0000   │0017   │002D                              │
    │               ││────────┴───────┴───────────────────────           │
    │               ││────────────────┬───────┬───────────────           │
    │              b││ 0000           │0018   │002E                      │
    │               ││────────────────┴───────┴───────────────           │
    │ith_a_long_name││                                                   │
    │               ││────────────────────────────────────────           │
    │               ││                                                   │
    │               ││                                                   │
    └───────────────┘└───────────────────────────────────────────────────┘
    8810362927e35cce94c7652659a13137
    |}]
;;

let%expect_test "auto wave format of input, output and internal ports" =
  let open Signal in
  let format = Wave_format.Index [ ":("; ":)" ] in
  let a = input "a" 1 --$ format in
  let a_n = ~:a -- "a_n" --$ format in
  let b = output "b" a_n --$ format in
  let circ = Circuit.create_exn ~name:"whimsy" [ b ] in
  let sim = Cyclesim.create ~config:Cyclesim.Config.trace_all circ in
  let waves, sim = Waveform.create sim in
  let a = Cyclesim.in_port sim "a" in
  for i = 0 to 5 do
    a := Bits.of_int_trunc ~width:1 (i land 1);
    Cyclesim.cycle sim
  done;
  Waveform.print ~display_height:12 waves;
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │               ││────────┬───────┬───────┬───────┬───────┬───────   │
    │a              ││ :(     │:)     │:(     │:)     │:(     │:)        │
    │               ││────────┴───────┴───────┴───────┴───────┴───────   │
    │               ││────────┬───────┬───────┬───────┬───────┬───────   │
    │b              ││ :)     │:(     │:)     │:(     │:)     │:(        │
    │               ││────────┴───────┴───────┴───────┴───────┴───────   │
    │               ││────────┬───────┬───────┬───────┬───────┬───────   │
    │a_n            ││ :)     │:(     │:)     │:(     │:)     │:(        │
    │               ││────────┴───────┴───────┴───────┴───────┴───────   │
    │               ││                                                   │
    └───────────────┘└───────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "expect_exact" =
  Waveform.expect_exact (Lazy.force testbench);
  [%expect_exact
    {|
┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────┐
│clk               ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───│
│                  ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   │
│clr               ││────────┐                                                           │
│                  ││        └───────────────────────────────                            │
│                  ││────────┬───────┬───────────────────────                            │
│a                 ││ 0000   │0017   │002D                                               │
│                  ││────────┴───────┴───────────────────────                            │
│                  ││────────────────┬───────┬───────────────                            │
│b                 ││ 0000           │0018   │002E                                       │
│                  ││────────────────┴───────┴───────────────                            │
│output_c_with_a_lo││                                                                    │
│                  ││────────────────────────────────────────                            │
└──────────────────┘└────────────────────────────────────────────────────────────────────┘
8810362927e35cce94c7652659a13137
|}]
;;

let%expect_test "auto display rules" =
  let module A = struct
    type 'a t = { a : 'a } [@@deriving hardcaml]
  end
  in
  let module B = struct
    type 'a t =
      { a : 'a A.t
      ; b : 'a A.t [@rtlprefix "b$"]
      ; c : 'a A.t [@rtlsuffix "$c"]
      }
    [@@deriving hardcaml ~rtlmangle:false]
  end
  in
  let module Sim = Cyclesim.With_interface (Interface.Empty) (B) in
  let sim = Sim.create (fun _ -> B.Of_signal.zero ()) in
  let waves, sim = Waveform.create sim in
  Cyclesim.cycle ~n:10 sim;
  let module A = Display_rules.With_interface (A) in
  Waveform.expect_exact waves ~display_rules:(List.concat [ A.default () ]);
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────┐
    │a                 ││                                                                    │
    │                  ││────────────────────────────────────────────────────────────────────│
    └──────────────────┘└────────────────────────────────────────────────────────────────────┘
    ba2e36d1d08d114b90957c7b1fa80e3a
    |}];
  Waveform.expect_exact waves ~display_rules:(List.concat [ A.default ~prefix:"b$" () ]);
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────┐
    │b$a               ││                                                                    │
    │                  ││────────────────────────────────────────────────────────────────────│
    └──────────────────┘└────────────────────────────────────────────────────────────────────┘
    ba2e36d1d08d114b90957c7b1fa80e3a
    |}];
  Waveform.expect_exact waves ~display_rules:(List.concat [ A.default ~suffix:"$c" () ]);
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────┐
    │a$c               ││                                                                    │
    │                  ││────────────────────────────────────────────────────────────────────│
    └──────────────────┘└────────────────────────────────────────────────────────────────────┘
    ba2e36d1d08d114b90957c7b1fa80e3a
    |}]
;;
