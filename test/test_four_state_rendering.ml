open! Import
module Events = Hardcaml.Wave_data_in_mutable_events_four_state

let make_wave ?max_time ~name ?(wave_format = Hardcaml.Wave_format.default) ~width events =
  let max_time = Option.value max_time ~default:(ref 0) in
  let wave_data = Events.create width max_time in
  List.iter events ~f:(fun (time, string) ->
    let value = Four_state_logic.of_string string in
    Events.insert
      wave_data
      time
      ~bits:(Four_state_logic.bits value)
      ~mask:(Four_state_logic.mask value);
    max_time := max !max_time time);
  { Hardcaml.Wave_data.Wave.name
  ; width
  ; typ = Hardcaml.Wave_data.Type.Internal
  ; wave_format
  ; wave_data
  }
;;

(* A 1 bit wave with one event per time step, each character of [values] giving the value
   held during that time step. *)
let make_bit_wave ?max_time ~name values =
  make_wave
    ?max_time
    ~name
    ~width:1
    (List.mapi (String.to_list values) ~f:(fun time value -> time, String.of_char value))
;;

let print ?display_width ?wave_scale ?display_values waves =
  Waveform.print
    ?display_width
    ?wave_scale
    ?display_values
    (Hardcaml.Wave_data.By_mutable_event_four_state (Array.of_list waves))
;;

let%expect_test "Wave_format.to_string_four_state" =
  let test wave_format values =
    let to_str = Staged.unstage (Hardcaml.Wave_format.to_string_four_state wave_format) in
    List.iter values ~f:(fun value ->
      print_s [%message value ~_:(to_str (Four_state_logic.of_string value) : string)])
  in
  (* Hex with a width that isn't a multiple of 4: the top digit covers 2 bits. *)
  test Hex [ "101010"; "ZZ1010"; "10XX10"; "Z01010"; "ZZZZZZ"; "XZXZXZ" ];
  [%expect
    {|
    (101010 2A)
    (ZZ1010 ZA)
    (10XX10 2X)
    (Z01010 ZA)
    (ZZZZZZ ZZ)
    (XZXZXZ XX)
    |}];
  (* Bit_or dispatches on width. *)
  test (Bit_or Hex) [ "X" ];
  test (Bit_or Hex) [ "XX10" ];
  [%expect
    {|
    (X X)
    (XX10 X)
    |}];
  (* Custom formats have no mapping for impure values. *)
  test (Custom (fun bits -> [%string "custom %{bits#Bits}"])) [ "1010"; "10Z0"; "ZZZZ" ];
  [%expect
    {|
    (1010 "custom 1010")
    (10Z0 Z)
    (ZZZZ Z)
    |}];
  test Unsigned_int [ "1010"; "1Z10"; "ZZZZ" ];
  [%expect
    {|
    (1010 10)
    (1Z10 Z)
    (ZZZZ Z)
    |}]
;;

let%expect_test "binary waves annotate X and Z on the line" =
  (* X displays on the high line (its bits plane is 1) and Z on the low line. Letters mark
     every character of an impure region, skipping characters that hold a transition edge. *)
  let waves =
    [ make_wave
        ~name:"a"
        ~width:1
        [ 0, "X"; 4, "0"; 8, "Z"; 12, "1"; 16, "Z"; 20, "0"; 24, "X"; 28, "1" ]
    ]
  in
  print ~wave_scale:1 ~display_width:70 waves;
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │a              ││XXXXXXXX┐               ┌───────┐               ┌XX│
    │               ││        └───────ZZZZZZZZ┘       └ZZZZZZZ────────┘  │
    └───────────────┘└───────────────────────────────────────────────────┘
    |}];
  (* Zoomed out: 2 time steps per char; the transition-edge char of a region is not
     annotated. *)
  print ~wave_scale:(-1) ~display_width:70 waves;
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │a              ││XX┐   ┌─┐   ┌X─                                    │
    │               ││  └─ZZ┘ └Z──┘                                      │
    └───────────────┘└───────────────────────────────────────────────────┘
    |}];
  (* One time step per char. *)
  print ~wave_scale:0 ~display_width:40 waves;
  [%expect
    {|
    ┌Signals─┐┌Waves───────────────────────┐
    │a       ││XXXX┐       ┌───┐       ┌XXX│
    │        ││    └───ZZZZ┘   └ZZZ────┘   │
    └────────┘└────────────────────────────┘
    |}]
;;

let%expect_test "narrow impure regions entered via an edge are annotated" =
  (* An impure region that is entered via a level change must still show its letter, even
     when the only character it gets is the one holding the transition edge. *)
  print
    ~wave_scale:0
    ~display_width:40
    [ make_bit_wave ~name:"x_in_0" "0000X000"
    ; make_bit_wave ~name:"x_in_1" "1111X111"
    ; make_bit_wave ~name:"z_in_0" "0000Z000"
    ; make_bit_wave ~name:"z_in_1" "1111Z111"
    ];
  [%expect
    {|
    ┌Signals─┐┌Waves───────────────────────┐
    │x_in_0  ││    X┐                      │
    │        ││────┘└──                    │
    │x_in_1  ││────X───                    │
    │        ││                            │
    │z_in_0  ││                            │
    │        ││────Z───                    │
    │z_in_1  ││────┐┌──                    │
    │        ││    Z┘                      │
    └────────┘└────────────────────────────┘
    |}];
  (* Zoomed out to 2 time steps per char. The region is placed on an odd time step so that
     it straddles a character boundary: a region wholly inside one character makes two
     transitions there and so renders as fuzz. *)
  print
    ~wave_scale:(-1)
    ~display_width:40
    [ make_bit_wave ~name:"x_in_0" "00000X000"
    ; make_bit_wave ~name:"x_in_1" "11111X111"
    ; make_bit_wave ~name:"z_in_0" "00000Z000"
    ; make_bit_wave ~name:"z_in_1" "11111Z111"
    ];
  [%expect
    {|
    ┌Signals─┐┌Waves───────────────────────┐
    │x_in_0  ││  X┐                        │
    │        ││──┘└─                       │
    │x_in_1  ││──X──                       │
    │        ││                            │
    │z_in_0  ││                            │
    │        ││──Z──                       │
    │z_in_1  ││──┐┌─                       │
    │        ││  Z┘                        │
    └────────┘└────────────────────────────┘
    |}]
;;

let%expect_test "narrow impure regions at the window edges are annotated" =
  print
    ~wave_scale:0
    ~display_width:40
    [ make_bit_wave ~name:"first" "X0000000"; make_bit_wave ~name:"last" "0000000X" ];
  [%expect
    {|
    ┌Signals─┐┌Waves───────────────────────┐
    │first   ││X┐                          │
    │        ││ └──────                    │
    │last    ││       X                    │
    │        ││───────┘                    │
    └────────┘└────────────────────────────┘
    |}];
  print
    ~wave_scale:(-1)
    ~display_width:40
    [ make_bit_wave ~name:"first" "X0000000"; make_bit_wave ~name:"last" "0000000X" ];
  [%expect
    {|
    ┌Signals─┐┌Waves───────────────────────┐
    │first   ││X                           │
    │        ││└───                        │
    │last    ││   X                        │
    │        ││───┘                        │
    └────────┘└────────────────────────────┘
    |}];
  (* The region runs off the right hand side of the window, so the last visible character
     must carry the letter itself. *)
  print
    ~wave_scale:0
    ~display_width:40
    [ make_bit_wave ~name:"clipped" (String.make 27 '0' ^ "XXX") ];
  [%expect
    {|
    ┌Signals─┐┌Waves───────────────────────┐
    │clipped ││                           X│
    │        ││───────────────────────────┘│
    └────────┘└────────────────────────────┘
    |}]
;;

let%expect_test "characters that contain an impure value but end pure are annotated" =
  print
    ~wave_scale:(-1)
    ~display_width:40
    [ make_bit_wave ~name:"x_then_1" "XXX11111"
    ; make_bit_wave ~name:"x_then_0" "XXX00000"
    ; make_bit_wave ~name:"z_then_0" "ZZZ00000"
    ; make_bit_wave ~name:"z_then_1" "ZZZ11111"
    ];
  [%expect
    {|
    ┌Signals─┐┌Waves───────────────────────┐
    │x_then_0││XX                          │
    │        ││ └──                        │
    │x_then_1││XX──                        │
    │        ││                            │
    │z_then_0││                            │
    │        ││ZZ──                        │
    │z_then_1││ ┌──                        │
    │        ││ZZ                          │
    └────────┘└────────────────────────────┘
    |}]
;;

let%expect_test "a character holding both X and Z is annotated with X" =
  print
    ~wave_scale:(-1)
    ~display_width:40
    [ make_bit_wave ~name:"z_then_x" "ZZZX1111"
    ; make_bit_wave ~name:"x_then_z" "XXXZ0000"
    ];
  [%expect
    {|
    ┌Signals─┐┌Waves───────────────────────┐
    │x_then_z││XX                          │
    │        ││ └──                        │
    │z_then_x││ X──                        │
    │        ││Z┘                          │
    └────────┘└────────────────────────────┘
    |}]
;;

let%expect_test "rapid X/Z activity within a char fuzzes" =
  let waves =
    [ make_wave
        ~name:"a"
        ~width:1
        (List.init 16 ~f:(fun i -> i, [| "0"; "X"; "1"; "Z" |].(i % 4)))
    ]
  in
  (* 4 time steps per char: each char covers several distinct values. *)
  print ~wave_scale:(-2) ~display_width:40 waves;
  [%expect
    {|
    ┌Signals─┐┌Waves───────────────────────┐
    │a       ││╥╥╥╥╥┐                      │
    │        ││╨╨╨╨╨Z                      │
    └────────┘└────────────────────────────┘
    |}];
  (* A single impure time step wholly inside one character makes two transitions there, so
     the character fuzzes and is deliberately left unannotated - rapid activity reads as
     fuzz whether or not the values are impure. *)
  print
    ~wave_scale:(-1)
    ~display_width:40
    [ make_bit_wave ~name:"x_in_0" "0000X000"; make_bit_wave ~name:"z_in_1" "1111Z111" ];
  [%expect
    {|
    ┌Signals─┐┌Waves───────────────────────┐
    │x_in_0  ││  ╥                         │
    │        ││──╨─                        │
    │z_in_1  ││──╥─                        │
    │        ││  ╨                         │
    └────────┘└────────────────────────────┘
    |}]
;;

let%expect_test "hex buses show per-digit X/Z" =
  let waves =
    [ make_wave
        ~name:"data"
        ~width:8
        [ 0, "XXXXXXXX"; 4, "0101ZZZZ"; 8, "10100101"; 12, "010X0101"; 16, "ZZZZXXXX" ]
    ]
  in
  print ~wave_scale:1 ~display_width:70 ~display_values:true waves;
  [%expect
    {|
    ┌Signals──┐┌Values───┐┌Waves─────────────────────────────────────────┐
    │         ││         ││────────┬───────┬───────┬───────┬─            │
    │data     ││    8'hXX││ XX     │5Z     │A5     │X5     │.            │
    │         ││         ││────────┴───────┴───────┴───────┴─            │
    └─────────┘└─────────┘└──────────────────────────────────────────────┘
    |}]
;;

let%expect_test "int and index formats show whole-value X or Z" =
  let waves =
    [ make_wave
        ~name:"int"
        ~wave_format:Int
        ~width:4
        [ 0, "XXXX"; 4, "ZZZZ"; 8, "0101"; 12, "010Z" ]
    ; make_wave
        ~name:"idx"
        ~wave_format:(Index [ "alpha"; "beta"; "gamma" ])
        ~width:4
        [ 0, "XXXX"; 4, "0001"; 8, "00Z0"; 12, "0010" ]
    ]
  in
  print ~wave_scale:1 ~display_width:70 ~display_values:true waves;
  [%expect
    {|
    ┌Signals──┐┌Values───┐┌Waves─────────────────────────────────────────┐
    │         ││         ││────────┬───────┬───────┬─                    │
    │idx      ││        X││ X      │beta   │Z      │.                    │
    │         ││         ││────────┴───────┴───────┴─                    │
    │         ││         ││────────┬───────┬───────┬─                    │
    │int      ││     4'DX││ X      │Z      │5      │Z                    │
    │         ││         ││────────┴───────┴───────┴─                    │
    └─────────┘└─────────┘└──────────────────────────────────────────────┘
    |}]
;;

let%expect_test "zoomed out buses, with a values column" =
  (* 2 time steps per char. The values column shows the value at the start of the window:
     1 bit waves display it per bit, buses through their wave format. *)
  print
    ~wave_scale:(-1)
    ~display_width:60
    ~display_values:true
    [ make_bit_wave ~name:"bit" "XX0011ZZ"
    ; make_wave
        ~name:"bus"
        ~width:8
        [ 0, "XXXXXXXX"; 2, "0101ZZZZ"; 4, "10100101"; 6, "ZZZZXXXX" ]
    ];
  [%expect
    {|
    ┌Signals─┐┌Values──┐┌Waves─────────────────────────────────┐
    │bit     ││       X││X┐┌┐                                  │
    │        ││        ││ └┘Z                                  │
    │        ││        ││─┬┬┬                                  │
    │bus     ││   8'hXX││ │││                                  │
    │        ││        ││─┴┴┴                                  │
    └────────┘└────────┘└──────────────────────────────────────┘
    |}]
;;

let%expect_test "4-state waves display X where no event covers the time" =
  (* A wave without a time-0 event (e.g. from a foreign VCD without an initial
     [$dumpvars]) reads as unknown, not as a confident 0, until its first event. A wave
     with no events at all reads as all X, matching
     [Wave_data_in_mutable_events_four_state.get]. 2-state waves keep their
     zero-before-the-first-event convention. *)
  print
    ~wave_scale:0
    ~display_width:60
    ~display_values:true
    [ make_wave ~name:"bit" ~width:1 [ 4, "0"; 8, "1" ]
    ; make_wave ~name:"bus" ~width:4 [ 4, "0101" ]
    ; make_wave ~name:"empty" ~width:4 []
    ];
  [%expect
    {|
    ┌Signals─┐┌Values──┐┌Waves─────────────────────────────────┐
    │bit     ││       X││XXXX┐   ┌                             │
    │        ││        ││    └───┘                             │
    │        ││        ││────┬                                 │
    │bus     ││    4'hX││ X  │                                 │
    │        ││        ││────┴                                 │
    │        ││        ││─                                     │
    │empty   ││    4'hX││                                      │
    │        ││        ││─                                     │
    └────────┘└────────┘└──────────────────────────────────────┘
    |}]
;;

let%expect_test "wave conditions explicitly downgrade four-state data" =
  let waves =
    Hardcaml.Wave_data.By_mutable_event_four_state
      [| make_bit_wave ~name:"state" "X1Z0" |]
  in
  let find ~n value =
    Waveform.look_for_nth_instance_of_condition_in_waveform
      ~time_steps_per_cycle:1
      ~n
      ~conditions:
        [ { Wave_condition.how_to_find = Suffix "state"
          ; condition = (fun bits -> Bits.equal bits (Bits.of_bool value))
          }
        ]
      waves
  in
  (* X and Z both become 0; the only high value is the pure 1 at time 1. *)
  print_s
    [%message
      (find ~n:1 true : int option)
        (find ~n:1 false : int option)
        (find ~n:2 false : int option)];
  [%expect
    {|
    (("find ~n:1 true"  (1))
     ("find ~n:1 false" (0))
     ("find ~n:2 false" (2)))
    |}]
;;

let%expect_test "mask-only changes are transitions" =
  (* 0 -> Z -> 0: same bits plane throughout, but the value changes. The bus shows
     transitions; the binary wave stays on the low line with Z regions annotated. *)
  let waves =
    [ make_wave ~name:"bit" ~width:1 [ 0, "0"; 4, "Z"; 8, "0" ]
    ; make_wave ~name:"bus" ~width:4 [ 0, "0000"; 4, "000Z"; 8, "0000" ]
    ]
  in
  print ~wave_scale:1 ~display_width:50 waves;
  [%expect
    {|
    ┌Signals───┐┌Waves───────────────────────────────┐
    │bit       ││                                    │
    │          ││────────ZZZZZZZZ──                  │
    │          ││────────┬───────┬─                  │
    │bus       ││ 0      │Z      │0                  │
    │          ││────────┴───────┴─                  │
    └──────────┘└────────────────────────────────────┘
    |}]
;;

let%expect_test "pure 4-state waveforms render identically to 2-state" =
  (* A 4-state waveform containing no X/Z values renders exactly as the equivalent 2-state
     waveform. *)
  let events = [ 0, 1; 4, 10; 8, 5 ] in
  let four_state =
    make_wave
      ~name:"d"
      ~width:4
      (List.map events ~f:(fun (time, value) ->
         time, Bits.of_unsigned_int ~width:4 value |> Bits.to_string))
  in
  let two_state =
    let max_time = ref 0 in
    let wave_data = Hardcaml.Wave_data_in_mutable_events.create 4 max_time in
    List.iter events ~f:(fun (time, value) ->
      Hardcaml.Wave_data_in_mutable_events.insert
        wave_data
        time
        (Bits.of_unsigned_int ~width:4 value);
      max_time := max !max_time time);
    { Hardcaml.Wave_data.Wave.name = "d"
    ; width = 4
    ; typ = Hardcaml.Wave_data.Type.Internal
    ; wave_format = Hardcaml.Wave_format.default
    ; wave_data
    }
  in
  let to_string waves = Waveform.to_string ~display_width:50 ~wave_scale:1 waves in
  let four_state_rendered =
    to_string (Hardcaml.Wave_data.By_mutable_event_four_state [| four_state |])
  in
  let two_state_rendered =
    to_string (Hardcaml.Wave_data.By_mutable_event [| two_state |])
  in
  print_string four_state_rendered;
  require
    (String.equal four_state_rendered two_state_rendered)
    ~if_false_then_print_s:(lazy [%message "renderings differ" two_state_rendered]);
  [%expect
    {|
    ┌Signals───┐┌Waves───────────────────────────────┐
    │          ││────────┬───────┬─                  │
    │d         ││ 1      │A      │5                  │
    │          ││────────┴───────┴─                  │
    └──────────┘└────────────────────────────────────┘
    |}]
;;

(* Compact frames shared with swell's Canvas adapter tests. *)
module Letter_render = Hardcaml_waveterm_kernel.Render.Make (Events)

let print_letter_placement ?(wave_scale = 0) ?(width = 48) waves =
  let waves =
    Hardcaml_waveterm_kernel.Waveform.sort_ports_and_formats (Array.of_list waves) None
  in
  let state =
    { Waves.cfg = { Waves.Config.default with wave_scale; start_signal = 0 }; waves }
  in
  let height = Array.sum (module Int) waves ~f:Wave.get_height_in_chars in
  let ctx = Draw.init ~rows:height ~cols:width in
  let bounds c w = { Rect.r = 0; c; w; h = height } in
  Letter_render.draw_signals ~selected_wave_index:None ~ctx ~bounds:(bounds 0 8) state;
  let (_ : int) =
    Letter_render.draw_values ~selected_wave_index:None ~ctx ~bounds:(bounds 8 8) state
  in
  Letter_render.draw_wave
    ~selected_wave_index:None
    ~ctx
    ~bounds:(bounds 16 (width - 16))
    state;
  Write.utf8 print_string ctx
;;

let%expect_test "right-edge X replaces a clipped transition glyph" =
  print_letter_placement ~wave_scale:1 ~width:19 [ make_bit_wave ~name:"bit" "0X" ];
  [%expect
    {|
    bit            0  X
                    ──┘
    |}]
;;

let%expect_test "X is not deferred into fuzz" =
  print_letter_placement ~wave_scale:(-2) ~width:18 [ make_bit_wave ~name:"bit" "00XX01" ];
  [%expect
    {|
    bit            0X╥
                    ┘╨
    |}]
;;

let%expect_test "deferred Z is not swallowed by X" =
  print_letter_placement ~wave_scale:(-1) ~width:18 [ make_bit_wave ~name:"bit" "1ZZX" ];
  [%expect
    {|
    bit            1┐X
                    Z┘
    |}]
;;

let%expect_test "X defers to the next char when it can letter the same region" =
  print_letter_placement ~wave_scale:(-2) [ make_bit_wave ~name:"bit" "00XXXX" ];
  [%expect
    {|
    bit            0┌X
                    ┘
    |}]
;;

let%test_unit "trace drawing checks the mode once and never reads two-state masks" =
  let max_time = ref 0 in
  let four_state_waves =
    [ make_bit_wave ~max_time ~name:"bit" "0011XXZZ00"
    ; make_wave
        ~max_time
        ~name:"bus"
        ~width:4
        [ 0, "0000"; 2, "0101"; 4, "XX01"; 6, "ZZ00"; 9, "1111" ]
    ; make_wave ~max_time ~name:"late_bit" ~width:1 [ 4, "X"; 8, "1" ]
    ; make_wave ~max_time ~name:"late_bus" ~width:4 [ 4, "XXXZ"; 8, "0011" ]
    ; make_wave ~max_time ~name:"empty_bit" ~width:1 []
    ; make_wave ~max_time ~name:"empty_bus" ~width:4 []
    ]
    |> Array.of_list
  in
  let test
    (type data)
    ~four_state
    (module Backend : Hardcaml.Wave_data.S with type t = data)
    waves
    =
    let mode_checks = ref 0 in
    let module Instrumented = struct
      include Backend

      let is_four_state data =
        Int.incr mode_checks;
        Backend.is_four_state data
      ;;

      let get_mask_at_index data index =
        if not four_state then failwith "two-state rendering accessed the mask plane";
        Backend.get_mask_at_index data index
      ;;
    end
    in
    let module Trace =
      Wave_trace.Make
        (Instrumented)
        (struct
          type ctx = Draw.ctx
          type style = Style.t

          let draw_glyph = Draw.draw_glyph
          let draw_int = Draw.draw_int
        end)
    in
    Array.iter waves ~f:(fun (wave : _ Hardcaml.Wave_data.Wave.t) ->
      List.iter [ -2; -1; 0; 1; 3 ] ~f:(fun wave_scale ->
        List.iter [ 0; 1; 4; 9 ] ~f:(fun off ->
          let rows = if wave.width = 1 then 2 else 3 in
          let ctx = Draw.init ~rows ~cols:12 in
          let bounds = Draw.get_bounds ctx in
          let wave_scale = Render.wave_scale_of_code wave_scale in
          let style = Style.default in
          mode_checks := 0;
          if wave.width = 1
          then Trace.draw_binary_data wave.wave_data ~ctx ~style ~bounds ~wave_scale ~off
          else
            Trace.draw_data
              wave.wave_data
              ~ctx
              ~style
              ~bounds
              ~wave_scale
              ~off
              ~alignment:Left
              ~to_str:(fun ~bits ~mask ->
                Four_state_logic.create_exn ~bits ~mask |> Four_state_logic.to_string);
          [%test_result: int] !mode_checks ~expect:1)))
  in
  let two_state_waves =
    match
      Hardcaml.Wave_data.to_two_state (By_mutable_event_four_state four_state_waves)
    with
    | By_mutable_event waves -> waves
    | By_mutable_event_four_state _ -> assert false
  in
  test ~four_state:false (module Hardcaml.Wave_data_in_mutable_events) two_state_waves;
  test
    ~four_state:true
    (module Hardcaml.Wave_data_in_mutable_events_four_state)
    four_state_waves
;;

let%expect_test "bus text is one cell per codepoint, including four-byte UTF-8" =
  (* A custom formatter may return any UTF-8: [draw_text_data] places one codepoint per
     cell and [Write.utf8] must encode each of them, including those above U+FFFF (here
     U+1D11E, the treble clef, and U+1F600). Truncation lands between codepoints.

     Note the renderer assumes one codepoint is one cell. Terminals draw wide characters
     such as emoji across two cells, so the row containing U+1F600 misaligns by one column
     when viewed in a terminal. We don't try to handle display widths. *)
  let labels = [| "\u{1D11E}\u{3BB}!"; "caf\u{E9}\u{1F600}\u{1D11E}xyz"; "plain" |] in
  let wave_format =
    Hardcaml.Wave_format.Custom (fun bits -> labels.(Bits.to_unsigned_int bits))
  in
  let waves =
    [ make_wave ~name:"s" ~wave_format ~width:2 [ 0, "00"; 4, "01"; 8, "10" ] ]
  in
  print ~wave_scale:1 ~display_width:44 waves;
  [%expect
    {|
    ┌Signals──┐┌Waves──────────────────────────┐
    │         ││────────┬───────┬─             │
    │s        ││ 𝄞λ!    │café😀𝄞.│.             │
    │         ││────────┴───────┴─             │
    └─────────┘└───────────────────────────────┘
    |}];
  print ~wave_scale:(-1) ~display_width:44 waves;
  [%expect
    {|
    ┌Signals──┐┌Waves──────────────────────────┐
    │         ││──┬─┬                          │
    │s        ││ .│.│                          │
    │         ││──┴─┴                          │
    └─────────┘└───────────────────────────────┘
    |}]
;;
