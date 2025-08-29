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

let test f ~rows ~cols ~draw_scoll_bar =
  let scroll_bar, remaining_bounds = f () in
  let ctx = Draw_notty.init ~rows ~cols in
  draw_scoll_bar ~ctx ~style:Style.default scroll_bar;
  Draw_notty.Border.draw ~ctx ~bounds:(Scroll.Scrollbar.bounds scroll_bar) "";
  Draw_notty.Border.draw ~ctx ~bounds:remaining_bounds "";
  output ctx
;;

let test_vert ~rows ~cols f = test ~rows ~cols ~draw_scoll_bar:Scroll.VScrollbar.draw f
let test_horz ~rows ~cols f = test ~rows ~cols ~draw_scoll_bar:Scroll.HScrollbar.draw f

let%expect_test "vertical" =
  let rows = 7 in
  let cols = 60 in
  let range = 10 in
  let container = { Rect.r = 0; c = 0; w = cols; h = rows } in
  test_vert ~rows ~cols (fun () ->
    Scroll.VScrollbar.create_right_aligned ~container ~range ());
  [%expect
    {|
    ##############################################################
    #┌────────────────────────────────────────────────────────┐┌┐#
    #│                                                        │││#
    #│                                                        │││#
    #│                                                        │││#
    #│                                                        │││#
    #│                                                        │││#
    #└────────────────────────────────────────────────────────┘└┘#
    ##############################################################
    |}];
  test_vert ~rows ~cols (fun () ->
    Scroll.VScrollbar.create_right_aligned ~container ~range ~w:10 ());
  [%expect
    {|
    ##############################################################
    #┌────────────────────────────────────────────────┐┌────────┐#
    #│                                                ││        │#
    #│                                                ││        │#
    #│                                                ││        │#
    #│                                                ││        │#
    #│                                                ││        │#
    #└────────────────────────────────────────────────┘└────────┘#
    ##############################################################
    |}];
  test_vert ~rows ~cols (fun () ->
    Scroll.VScrollbar.create_right_aligned ~container ~range ~r:3 ~h:4 ~w:3 ());
  [%expect
    {|
    ##############################################################
    #┌───────────────────────────────────────────────────────┐   #
    #│                                                       │   #
    #│                                                       │   #
    #│                                                       │┌─┐#
    #│                                                       ││ │#
    #│                                                       ││ │#
    #└───────────────────────────────────────────────────────┘└─┘#
    ##############################################################
    |}]
;;

let%expect_test "horizontal" =
  let rows = 10 in
  let cols = 60 in
  let range = 10 in
  let container = { Rect.r = 0; c = 0; w = cols; h = rows } in
  test_horz ~rows ~cols (fun () ->
    Scroll.HScrollbar.create_bottom_aligned ~container ~range ~h:2 ());
  [%expect
    {|
    ##############################################################
    #┌──────────────────────────────────────────────────────────┐#
    #│                                                          │#
    #│                                                          │#
    #│                                                          │#
    #│                                                          │#
    #│                                                          │#
    #│                                                          │#
    #└──────────────────────────────────────────────────────────┘#
    #┌──────────────────────────────────────────────────────────┐#
    #└──────────────────────────────────────────────────────────┘#
    ##############################################################
    |}];
  test_vert ~rows ~cols (fun () ->
    Scroll.HScrollbar.create_bottom_aligned ~container ~range ~h:4 ());
  [%expect
    {|
    ##############################################################
    #┌──────────────────────────────────────────────────────────┐#
    #│                                                          │#
    #│                                                          │#
    #│                                                          │#
    #│                                                          │#
    #└──────────────────────────────────────────────────────────┘#
    #┌──────────────────────────────────────────────────────────┐#
    #│                                                          │#
    #│                                                          │#
    #└──────────────────────────────────────────────────────────┘#
    ##############################################################
    |}];
  test_vert ~rows ~cols (fun () ->
    Scroll.HScrollbar.create_bottom_aligned ~container ~range ~c:3 ~w:10 ~h:3 ());
  [%expect
    {|
    ##############################################################
    #┌──────────────────────────────────────────────────────────┐#
    #│                                                          │#
    #│                                                          │#
    #│                                                          │#
    #│                                                          │#
    #│                                                          │#
    #└──────────────────────────────────────────────────────────┘#
    #   ┌────────┐                                               #
    #   │        │                                               #
    #   └────────┘                                               #
    ##############################################################
    |}]
;;
