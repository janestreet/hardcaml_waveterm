open! Import

let%expect_test "show a waveform through notty" =
  let ctx = Draw_notty.init ~rows:10 ~cols:60 in
  let waves =
    Test_data.create ~prefix:(fun _ -> "") ~length:10 ~num_signals:3 ~max_bits:64
  in
  Render.draw_ui ~ctx waves;
  Draw_notty.to_image ctx |> Notty_unix.output_image;
  [%expect
    {|
    ┌Signals─┐┌Values──┐┌Waves─────────────────────────────────┐
    │clock   ││        ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐ │
    │        ││        ││    └───┘   └───┘   └───┘   └───┘   └─│
    │dder    ││       1││────────┐                       ┌─────│
    │        ││        ││        └───────────────────────┘     │
    │        ││        ││────────┬───────┬───────┬───────┬─────│
    │s       ││D-667198││ -667198│543625 │14517  │121697 │-5568│
    │        ││        ││────────┴───────┴───────┴───────┴─────│
    │ikgcvexs││       1││────────┐       ┌───────┐       ┌─────│
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
  let data =
    match waves.waves.(1) with
    | Binary { data; _ } -> data
    | _ -> failwith ""
  in
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
    ╥╥─┐
    ╨╨ └
    ╥─╥─╥
    ╨ ╨ ╨
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
  let data =
    match waves.waves.(2) with
    | Data { data; _ } -> data
    | _ -> failwith ""
  in
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
    ╥╥──
    ║║1
    ╨╨──
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
