open! Import

include struct
  open Hardcaml_waveterm_kernel
  module Rect = Rect
  module Style = Style
end

(* Draw the image within a border.  This forces notty to output the whole thing. *)
let output ctx =
  let open Notty in
  let image = Draw_notty.to_image ctx in
  let v = I.char A.empty '#' (Draw_notty.cols ctx + 2) (Draw_notty.rows ctx + 2) in
  Notty_unix.output_image I.(pad ~l:1 ~r:1 ~t:1 ~b:1 image </> v)
;;

let create_hscroll ~width ~height ~range =
  let hscroll =
    Scroll.HScrollbar.create
      ~bounds:{ Rect.r = 0; c = 0; w = width; h = height }
      ~range
      ()
  in
  ( hscroll
  , fun ~ctx ~xloc ~yloc ~offset ->
      Scroll.Scrollbar.set_bounds
        hscroll
        { Rect.r = yloc; c = xloc; w = width; h = height };
      Scroll.Scrollbar.set_offset hscroll offset;
      Scroll.HScrollbar.draw ~ctx ~style:Style.default hscroll )
;;

let%expect_test "scroller sexp" =
  let hscroll, _ = create_hscroll ~width:20 ~height:12 ~range:50 in
  print_s [%message "configuration" (hscroll : Scroll.HScrollbar.t)];
  [%expect
    {|
    (configuration (
      hscroll (
        (scrollable (
          (adj (
            (range            50)
            (offset           0)
            (on_offset_change <opaque>)))
          (scroll_window_size 20)
          (scroll_bar_mode (Fixed 1))
          (min_scroll_bar_size ())
          (max_scroll_bar_size ())
          (scroll_bar_size     1)
          (scroll_bar_offset   0)
          (mouse_mode          Middle)
          (page_size           -1)
          (document_size       -1)
          (on_scrollbar_change <opaque>)))
        (bar_style Filled)
        (bounds (
          (r 0)
          (c 0)
          (w 20)
          (h 12)))
        (orientation Horz))))
    |}]
;;

let%expect_test "small scrollbar area" =
  let ctx = Draw_notty.init ~rows:7 ~cols:60 in
  let _, draw = create_hscroll ~width:20 ~height:1 ~range:50 in
  draw ~ctx ~xloc:20 ~yloc:0 ~offset:0;
  draw ~ctx ~xloc:20 ~yloc:1 ~offset:10;
  draw ~ctx ~xloc:20 ~yloc:2 ~offset:20;
  draw ~ctx ~xloc:20 ~yloc:3 ~offset:30;
  draw ~ctx ~xloc:20 ~yloc:4 ~offset:40;
  (* maximum value *)
  draw ~ctx ~xloc:20 ~yloc:5 ~offset:49;
  (* clipped to maximum value *)
  draw ~ctx ~xloc:20 ~yloc:6 ~offset:60;
  output ctx;
  [%expect
    {|
    ##############################################################
    #                    █                                       #
    #                        █                                   #
    #                            █                               #
    #                                █                           #
    #                                    █                       #
    #                                       █                    #
    #                                       █                    #
    ##############################################################
    |}]
;;

let%expect_test "big scroll bar area" =
  let ctx = Draw_notty.init ~rows:3 ~cols:60 in
  let _, draw = create_hscroll ~width:60 ~height:1 ~range:200 in
  draw ~ctx ~xloc:0 ~yloc:0 ~offset:0;
  draw ~ctx ~xloc:0 ~yloc:1 ~offset:100;
  draw ~ctx ~xloc:0 ~yloc:2 ~offset:200;
  output ctx;
  [%expect
    {|
    ##############################################################
    #█                                                           #
    #                              █                             #
    #                                                           █#
    ##############################################################
    |}]
;;

let%expect_test "dynamic scroll bar widths" =
  let ctx = Draw_notty.init ~rows:3 ~cols:60 in
  let hscroll, draw = create_hscroll ~width:60 ~height:1 ~range:200 in
  Scroll.Scrollbar.set_mode hscroll (Dynamic 30);
  draw ~ctx ~xloc:0 ~yloc:0 ~offset:0;
  Scroll.Scrollbar.set_mode hscroll (Dynamic 60);
  draw ~ctx ~xloc:0 ~yloc:1 ~offset:100;
  Scroll.Scrollbar.set_mode hscroll (Dynamic 160);
  draw ~ctx ~xloc:0 ~yloc:2 ~offset:140;
  output ctx;
  [%expect
    {|
    ##############################################################
    #███████                                                     #
    #                           █████████████                    #
    #                                 ██████████████████████████ #
    ##############################################################
    |}]
;;

(* This is a bug.  Should be tracked down and fixed. *)
let%expect_test "should not raise" =
  let ctx = Draw_notty.init ~rows:3 ~cols:60 in
  let hscroll, draw = create_hscroll ~width:60 ~height:1 ~range:200 in
  Scroll.Scrollbar.set_mode hscroll (Dynamic 160);
  require_does_not_raise ~cr:CR_someday ~hide_positions:true (fun () ->
    draw ~ctx ~xloc:0 ~yloc:2 ~offset:160);
  [%expect
    {|
    ("unexpectedly raised" (Invalid_argument "index out of bounds"))
    |}]
;;
