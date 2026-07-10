open! Import

let%expect_test "show a waveform through notty" =
  let ctx = Draw_notty.init ~rows:10 ~cols:60 in
  let waves =
    Test_data.create ~prefix:(fun _ -> "") ~length:10 ~num_signals:3 ~max_bits:64
  in
  Render.draw_ui
    ~ctx
    { Waves.cfg = Waves.Config.default
    ; waves = Waveform.sort_ports_and_formats waves (Some [ Default ])
    };
  Draw_notty.to_image ctx |> Notty_unix.output_image;
  [%expect
    {|
    ┌Signals─┐┌Values──┐┌Waves─────────────────────────────────┐
    │clock   ││        ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐ │
    │        ││        ││    └───┘   └───┘   └───┘   └───┘   └─│
    │dder    ││       1││────────┐                       ┌─────│
    │        ││        ││        └───────────────────────┘     │
    │ikgcvexs││       1││────────┐       ┌───────┐       ┌─────│
    │        ││        ││        └───────┘       └───────┘     │
    │        ││        ││────────┬───────┬───────┬───────┬─────│
    │s       ││D-667198││ -667198│543625 │14517  │121697 │-5568│
    └────────┘└────────┘└──────────────────────────────────────┘
    |}]
;;

let render_binary_data ~data ~w ~off =
  let ctx = Draw_notty.init ~rows:2 ~cols:60 in
  Render.draw_binary_data
    ~ctx
    ~style:Hardcaml_waveterm_kernel.Style.default
    ~bounds:{ r = 0; c = 0; w = 60; h = 2 }
    ~wave_width:w
    ~data
    ~off;
  Draw_notty.to_image ctx |> Notty_unix.output_image;
  print_endline ""
;;

let%expect_test "normal binary rendering" =
  let waves =
    Test_data.create ~prefix:(fun _ -> "") ~length:10 ~num_signals:3 ~max_bits:2
  in
  let data = waves.(1).wave_data in
  render_binary_data ~data ~w:(`Cycles_per_char 3) ~off:0;
  render_binary_data ~data ~w:(`Cycles_per_char 2) ~off:0;
  render_binary_data ~data ~w:(`Cycles_per_char 1) ~off:0;
  render_binary_data ~data ~w:(`Chars_per_cycle 1) ~off:0;
  render_binary_data ~data ~w:(`Chars_per_cycle 2) ~off:0;
  render_binary_data ~data ~w:(`Chars_per_cycle 3) ~off:0;
  render_binary_data ~data ~w:(`Chars_per_cycle 4) ~off:0;
  render_binary_data ~data ~w:(`Chars_per_cycle 5) ~off:0;
  [%expect
    {|
    ┌┐┌┐
    ┘└┘└
    ┌─┐┌┐
    ┘ └┘└
     ┌───┐┌──┐
    ─┘   └┘  └
     ┌───┐┌──┐
    ─┘   └┘  └
      ┌───────┐ ┌─────┐
    ──┘       └─┘     └─
       ┌───────────┐  ┌────────┐
    ───┘           └──┘        └──
        ┌───────────────┐   ┌───────────┐
    ────┘               └───┘           └───
         ┌───────────────────┐    ┌──────────────┐
    ─────┘                   └────┘              └────
    |}]
;;

let b0 = Bits.gnd
let b1 = Bits.vdd

let data_of_list bits =
  let len = List.length bits in
  let arr = Array.of_list bits in
  Data.init len ~width:1 ~f:(fun i -> arr.(i))
;;

let bits_to_str bits = List.map bits ~f:Bits.to_string |> String.concat ~sep:" "

let test_single data ~render =
  List.iter data ~f:(fun data ->
    print_endline (bits_to_str data);
    render ~data:(data_of_list data) ~w:(`Cycles_per_char 3) ~off:0)
;;

let test_back_to_back data ~render =
  List.iter data ~f:(fun prev ->
    List.iter data ~f:(fun data ->
      print_endline (bits_to_str prev ^ " - " ^ bits_to_str data);
      render ~data:(data_of_list (prev @ data)) ~w:(`Cycles_per_char 3) ~off:0))
;;

let fuzz_data =
  [ [ b1; b1; b1 ]
  ; [ b1; b1; b0 ]
  ; [ b1; b0; b1 ]
  ; [ b0; b0; b0 ]
  ; [ b0; b0; b1 ]
  ; [ b0; b1; b0 ]
  ]
;;

let%expect_test "fuzzing - binary" =
  test_single fuzz_data ~render:render_binary_data;
  [%expect
    {|
    1 1 1
    ─

    1 1 0
    ┐
    └
    1 0 1
    ╥
    ╨
    0 0 0

    ─
    0 0 1
    ┌
    ┘
    0 1 0
    ╥
    ╨
    |}];
  test_back_to_back fuzz_data ~render:render_binary_data;
  [%expect
    {|
    1 1 1 - 1 1 1
    ──

    1 1 1 - 1 1 0
    ─┐
     └
    1 1 1 - 1 0 1
    ─╥
     ╨
    1 1 1 - 0 0 0
    ─┐
     └
    1 1 1 - 0 0 1
    ─╥
     ╨
    1 1 1 - 0 1 0
    ─╥
     ╨
    1 1 0 - 1 1 1
    ┐┌
    └┘
    1 1 0 - 1 1 0
    ┐╥
    └╨
    1 1 0 - 1 0 1
    ┐╥
    └╨
    1 1 0 - 0 0 0
    ┐
    └─
    1 1 0 - 0 0 1
    ┐┌
    └┘
    1 1 0 - 0 1 0
    ┐╥
    └╨
    1 0 1 - 1 1 1
    ╥─
    ╨
    1 0 1 - 1 1 0
    ╥┐
    ╨└
    1 0 1 - 1 0 1
    ╥╥
    ╨╨
    1 0 1 - 0 0 0
    ╥┐
    ╨└
    1 0 1 - 0 0 1
    ╥╥
    ╨╨
    1 0 1 - 0 1 0
    ╥╥
    ╨╨
    0 0 0 - 1 1 1
     ┌
    ─┘
    0 0 0 - 1 1 0
     ╥
    ─╨
    0 0 0 - 1 0 1
     ╥
    ─╨
    0 0 0 - 0 0 0

    ──
    0 0 0 - 0 0 1
     ┌
    ─┘
    0 0 0 - 0 1 0
     ╥
    ─╨
    0 0 1 - 1 1 1
    ┌─
    ┘
    0 0 1 - 1 1 0
    ┌┐
    ┘└
    0 0 1 - 1 0 1
    ┌╥
    ┘╨
    0 0 1 - 0 0 0
    ┌┐
    ┘└
    0 0 1 - 0 0 1
    ┌╥
    ┘╨
    0 0 1 - 0 1 0
    ┌╥
    ┘╨
    0 1 0 - 1 1 1
    ╥┌
    ╨┘
    0 1 0 - 1 1 0
    ╥╥
    ╨╨
    0 1 0 - 1 0 1
    ╥╥
    ╨╨
    0 1 0 - 0 0 0
    ╥
    ╨─
    0 1 0 - 0 0 1
    ╥┌
    ╨┘
    0 1 0 - 0 1 0
    ╥╥
    ╨╨
    |}]
;;

let render_data ~data ~w ~off =
  let ctx = Draw_notty.init ~rows:3 ~cols:60 in
  Render.draw_data
    ~ctx
    ~style:Hardcaml_waveterm_kernel.Style.default
    ~bounds:{ r = 0; c = 0; w = 60; h = 3 }
    ~alignment:Left
    ~to_str:Bits.to_string
    ~wave_width:w
    ~data
    ~off;
  Draw_notty.to_image ctx |> Notty_unix.output_image;
  print_endline ""
;;

let%expect_test "data rendering" =
  let waves =
    Test_data.create ~prefix:(fun _ -> "") ~length:10 ~num_signals:3 ~max_bits:2
  in
  let data = waves.(2).wave_data in
  (* fuzzy *)
  render_data ~data ~w:(`Cycles_per_char 3) ~off:0;
  render_data ~data ~w:(`Cycles_per_char 2) ~off:0;
  render_data ~data ~w:(`Cycles_per_char 1) ~off:0;
  render_data ~data ~w:(`Chars_per_cycle 1) ~off:0;
  render_data ~data ~w:(`Chars_per_cycle 2) ~off:0;
  render_data ~data ~w:(`Chars_per_cycle 3) ~off:0;
  render_data ~data ~w:(`Chars_per_cycle 4) ~off:0;
  render_data ~data ~w:(`Chars_per_cycle 5) ~off:0;
  [%expect
    {|
    ┬╥──
    │║1
    ┴╨──
    ─╥───
     ║1
    ─╨───
    ──┬┬┬─────
     0│││1
    ──┴┴┴─────
    ──┬┬┬─────
     0│││1
    ──┴┴┴─────
    ────┬─┬─┬───────────
     0  │1│0│1
    ────┴─┴─┴───────────
    ──────┬──┬──┬─────────────────
     0    │1 │0 │1
    ──────┴──┴──┴─────────────────
    ────────┬───┬───┬───────────────────────
     0      │1  │0  │1
    ────────┴───┴───┴───────────────────────
    ──────────┬────┬────┬─────────────────────────────
     0        │1   │0   │1
    ──────────┴────┴────┴─────────────────────────────
    |}]
;;

let%expect_test "fuzzing - data" =
  test_single fuzz_data ~render:render_data;
  [%expect
    {|
    1 1 1
    ─

    ─
    1 1 0
    ┬
    │
    ┴
    1 0 1
    ╥
    ║
    ╨
    0 0 0
    ─

    ─
    0 0 1
    ┬
    │
    ┴
    0 1 0
    ╥
    ║
    ╨
    |}];
  test_back_to_back fuzz_data ~render:render_data;
  [%expect
    {|
    1 1 1 - 1 1 1
    ──
     1
    ──
    1 1 1 - 1 1 0
    ─┬
     │
    ─┴
    1 1 1 - 1 0 1
    ─╥
     ║
    ─╨
    1 1 1 - 0 0 0
    ─┬
     │
    ─┴
    1 1 1 - 0 0 1
    ─╥
     ║
    ─╨
    1 1 1 - 0 1 0
    ─╥
     ║
    ─╨
    1 1 0 - 1 1 1
    ┬┬
    ││
    ┴┴
    1 1 0 - 1 1 0
    ┬╥
    │║
    ┴╨
    1 1 0 - 1 0 1
    ┬╥
    │║
    ┴╨
    1 1 0 - 0 0 0
    ┬─
    │0
    ┴─
    1 1 0 - 0 0 1
    ┬┬
    ││
    ┴┴
    1 1 0 - 0 1 0
    ┬╥
    │║
    ┴╨
    1 0 1 - 1 1 1
    ╥─
    ║1
    ╨─
    1 0 1 - 1 1 0
    ╥┬
    ║│
    ╨┴
    1 0 1 - 1 0 1
    ╥╥
    ║║
    ╨╨
    1 0 1 - 0 0 0
    ╥─
    ║0
    ╨─
    1 0 1 - 0 0 1
    ╥┬
    ║│
    ╨┴
    1 0 1 - 0 1 0
    ╥╥
    ║║
    ╨╨
    0 0 0 - 1 1 1
    ─┬
     │
    ─┴
    0 0 0 - 1 1 0
    ─╥
     ║
    ─╨
    0 0 0 - 1 0 1
    ─╥
     ║
    ─╨
    0 0 0 - 0 0 0
    ──
     0
    ──
    0 0 0 - 0 0 1
    ─┬
     │
    ─┴
    0 0 0 - 0 1 0
    ─╥
     ║
    ─╨
    0 0 1 - 1 1 1
    ┬─
    │1
    ┴─
    0 0 1 - 1 1 0
    ┬┬
    ││
    ┴┴
    0 0 1 - 1 0 1
    ┬╥
    │║
    ┴╨
    0 0 1 - 0 0 0
    ┬┬
    ││
    ┴┴
    0 0 1 - 0 0 1
    ┬╥
    │║
    ┴╨
    0 0 1 - 0 1 0
    ┬╥
    │║
    ┴╨
    0 1 0 - 1 1 1
    ╥─
    ║1
    ╨─
    0 1 0 - 1 1 0
    ╥┬
    ║│
    ╨┴
    0 1 0 - 1 0 1
    ╥╥
    ║║
    ╨╨
    0 1 0 - 0 0 0
    ╥─
    ║0
    ╨─
    0 1 0 - 0 0 1
    ╥┬
    ║│
    ╨┴
    0 1 0 - 0 1 0
    ╥╥
    ║║
    ╨╨
    |}]
;;
