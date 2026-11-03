open! Import

module Mutable_event_render =
  Hardcaml_waveterm_kernel.Render.Make (Wave_data_in_mutable_events)

let ctx_to_string ctx =
  let buffer = Buffer.create 1024 in
  Write.utf8 (Buffer.add_string buffer) ctx;
  Buffer.contents buffer
;;

let print_ctx ctx = print_string (ctx_to_string ctx)

let mutable_event_data_of_ints ~width ~length events =
  let data = Wave_data_in_mutable_events.create width (ref (length - 1)) in
  List.iter events ~f:(fun (time, value) ->
    Wave_data_in_mutable_events.insert data time (Bits.of_unsigned_int ~width value));
  data
;;

let draw_binary_events ?(cols = 30) data ~wave_scale ~off =
  let ctx = Draw.init ~rows:2 ~cols in
  Mutable_event_render.draw_binary_data
    ~ctx
    ~style:Style.default
    ~bounds:(Draw.get_bounds ctx)
    ~wave_scale
    ~data
    ~off;
  ctx
;;

let draw_data_events ?(cols = 30) ?(alignment = Text_alignment.Left) data ~wave_scale ~off
  =
  let ctx = Draw.init ~rows:3 ~cols in
  Mutable_event_render.draw_data
    ~ctx
    ~style:Style.default
    ~bounds:(Draw.get_bounds ctx)
    ~to_str:Bits.to_string
    ~alignment
    ~wave_scale
    ~data
    ~off;
  ctx
;;

let%expect_test "binary events, time steps per char" =
  let data =
    mutable_event_data_of_ints
      ~width:1
      ~length:90
      [ 0, 0; 4, 1; 7, 0; 11, 1; 30, 0; 54, 1 ]
  in
  print_ctx (draw_binary_events data ~wave_scale:(`Time_steps_per_char 4) ~off:3);
  [%expect
    {|
    ┌┐┌───┐     ┌─────────
    ┘└┘   └─────┘
    |}]
;;

let%expect_test "binary events, multiple transitions within a char" =
  let data =
    mutable_event_data_of_ints
      ~width:1
      ~length:50
      [ 0, 0; 2, 1; 3, 0; 4, 1; 20, 0; 21, 1 ]
  in
  print_ctx (draw_binary_events ~cols:24 data ~wave_scale:(`Time_steps_per_char 5) ~off:0);
  [%expect
    {|
    ╥───╥─────
    ╨   ╨
    |}]
;;

let%expect_test "data events, chars per time step" =
  let data =
    mutable_event_data_of_ints
      ~width:4
      ~length:20
      [ 0, 0; 3, 1; 6, 3; 9, 8; 12, 15; 16, 4 ]
  in
  print_ctx (draw_data_events data ~wave_scale:(`Chars_per_time_step 2) ~off:0);
  [%expect
    {|
    ──────┬─────┬─────┬─────┬─────
     0000 │0001 │0011 │1000 │1111
    ──────┴─────┴─────┴─────┴─────
    |}]
;;

let%expect_test "data events, time steps per char" =
  let data =
    mutable_event_data_of_ints
      ~width:4
      ~length:120
      [ 0, 0; 4, 1; 12, 2; 13, 3; 31, 8; 70, 15 ]
  in
  print_ctx (draw_data_events data ~wave_scale:(`Time_steps_per_char 6) ~off:0);
  [%expect
    {|
    ┬─╥──┬─────┬────────
    │.║0.│1000 │1111
    ┴─╨──┴─────┴────────
    |}]
;;

let%expect_test "data events, right aligned" =
  let data =
    mutable_event_data_of_ints
      ~width:8
      ~length:120
      [ 0, 0; 4, 1; 12, 15; 31, 128; 70, 255 ]
  in
  print_ctx
    (draw_data_events ~alignment:Right data ~wave_scale:(`Time_steps_per_char 5) ~off:3);
  [%expect
    {|
    ┬┬───┬───────┬──────────
    ││.11│.000000│11111111
    ┴┴───┴───────┴──────────
    |}]
;;

let%expect_test "sparse event data from a large offset" =
  let data =
    mutable_event_data_of_ints
      ~width:4
      ~length:12_001
      [ 0, 0; 3_000, 1; 6_000, 2; 6_001, 3; 9_000, 4; 12_000, 5 ]
  in
  print_ctx
    (draw_data_events ~cols:20 data ~wave_scale:(`Time_steps_per_char 1_000) ~off:2_999);
  [%expect
    {|
    ┬──╥──┬──┬
    │0.║0.│0.│
    ┴──╨──┴──┴
    |}]
;;

let%expect_test "event data defaults to zero before the first event" =
  let event_data = mutable_event_data_of_ints ~width:4 ~length:16 [ 4, 7; 8, 3 ] in
  print_ctx
    (draw_data_events ~cols:20 event_data ~wave_scale:(`Time_steps_per_char 2) ~off:0);
  [%expect
    {|
    ──┬─┬───
     .│.│00.
    ──┴─┴───
    |}]
;;

let%expect_test "event exactly at left edge" =
  let data =
    mutable_event_data_of_ints ~width:4 ~length:32 [ 0, 0; 8, 1; 16, 7; 24, 3 ]
  in
  print_ctx (draw_data_events ~cols:16 data ~wave_scale:(`Time_steps_per_char 4) ~off:8);
  [%expect
    {|
    ──┬─┬─
     .│.│.
    ──┴─┴─
    |}]
;;

let%expect_test "event exactly at right edge belongs to next char" =
  let data = mutable_event_data_of_ints ~width:1 ~length:32 [ 0, 0; 4, 1; 8, 0; 12, 1 ] in
  print_ctx (draw_binary_events ~cols:16 data ~wave_scale:(`Time_steps_per_char 4) ~off:0);
  [%expect
    {|
     ┌┐┌────
    ─┘└┘
    |}]
;;

let%expect_test "same-valued events do not create transitions" =
  let data =
    mutable_event_data_of_ints
      ~width:4
      ~length:40
      [ 0, 0; 4, 1; 8, 1; 12, 1; 20, 3; 24, 3 ]
  in
  print_ctx (draw_data_events ~cols:20 data ~wave_scale:(`Time_steps_per_char 4) ~off:0);
  [%expect
    {|
    ─┬───┬────
     │00.│0011
    ─┴───┴────
    |}]
;;
