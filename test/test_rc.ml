open Core
open Hardcaml_waveterm_interactive.Expert

let%expect_test "grab your rcfile" =
  print_s [%sexp (Key_actions.default_keys : Key_actions.File_format.t)];
  [%expect
    {|
    ((common$quit (Escape ())) (common$scroll_up ((Arrow Up) ()))
     (common$scroll_down ((Arrow Down) ()))
     (common$scroll_up_fast ((Arrow Up) (Ctrl)))
     (common$scroll_down_fast ((Arrow Down) (Ctrl)))
     (waveform_window$show_help ((ASCII ?) ()))
     (waveform_window$cursor_centered_zoom_in ((ASCII =) ()))
     (waveform_window$cursor_centered_zoom_out ((ASCII -) ()))
     (waveform_window$cursor_centered_zoom_in_fast ((ASCII =) (Meta)))
     (waveform_window$cursor_centered_zoom_out_fast ((ASCII -) (Meta)))
     (waveform_window$expand_signals_window ((ASCII 9) ()))
     (waveform_window$shrink_signals_window ((ASCII "(") ()))
     (waveform_window$expand_values_window ((ASCII 0) ()))
     (waveform_window$shrink_values_window ((ASCII ")") ()))
     (waveform_window$scroll_wave_to_beginning (Home ()))
     (waveform_window$scroll_wave_to_end (End ()))
     (waveform_window$scroll_to_first_signal (Home (Ctrl)))
     (waveform_window$scroll_to_last_signal (End (Ctrl)))
     (waveform_window$scroll_left ((Arrow Left) ()))
     (waveform_window$scroll_right ((Arrow Right) ()))
     (waveform_window$scroll_left_fast ((Arrow Left) (Ctrl)))
     (waveform_window$scroll_right_fast ((Arrow Right) (Ctrl)))
     (waveform_window$move_cursor_up ((ASCII k) ()))
     (waveform_window$move_cursor_down ((ASCII j) ()))
     (waveform_window$move_cursor_left ((ASCII h) ()))
     (waveform_window$move_cursor_right ((ASCII l) ()))
     (waveform_window$move_cursor_up_fast ((ASCII K) ()))
     (waveform_window$move_cursor_down_fast ((ASCII J) ()))
     (waveform_window$move_cursor_left_fast ((ASCII H) ()))
     (waveform_window$move_cursor_right_fast ((ASCII L) ()))
     (waveform_window$center_on_cursor ((ASCII g) ()))
     (waveform_window$set_cursor_to_first_wave_on_screen ((ASCII G) ()))
     (waveform_window$select_cursor_1 ((ASCII 1) ()))
     (waveform_window$select_cursor_2 ((ASCII 2) ()))
     (waveform_window$select_cursor_3 ((ASCII 3) ()))
     (waveform_window$select_cursor_4 ((ASCII 4) ()))
     (waveform_window$select_cursor_5 ((ASCII 5) ()))
     (waveform_window$select_cursor_1_and_center ((ASCII 1) (Meta)))
     (waveform_window$select_cursor_2_and_center ((ASCII 2) (Meta)))
     (waveform_window$select_cursor_3_and_center ((ASCII 3) (Meta)))
     (waveform_window$select_cursor_4_and_center ((ASCII 4) (Meta)))
     (waveform_window$select_cursor_5_and_center ((ASCII 5) (Meta)))
     (waveform_window$remove_selected_cursor ((ASCII r) ()))
     (waveform_window$maybe_toggle_module (Enter ()))
     (waveform_window$save_ui_state ((ASCII s) ()))
     (waveform_window$load_ui_state ((ASCII S) ()))
     (waveform_window$search_for_transition_forward ((ASCII e) ()))
     (waveform_window$search_for_transition_backward ((ASCII b) ()))
     (waveform_window$cycle_wave_format ((ASCII f) ()))
     (waveform_window$reset_wave_format ((ASCII F) ()))
     (waveform_window$cycle_colour ((ASCII c) ()))
     (waveform_window$toggle_bold ((ASCII C) ())))
    |}]
;;

let%expect_test "round trip" =
  let default = Key_actions.default_keys in
  let from_sexp =
    Key_actions.File_format.sexp_of_t default |> Key_actions.File_format.t_of_sexp
  in
  assert (Key_actions.equal Key_actions.Key.equal default from_sexp)
;;

let parse s = Sexp.of_string_conv_exn s Key_actions.File_format.t_of_sexp
let parse_and_ingore s = (parse s : Key_actions.Key.t Key_actions.t) |> ignore

let%expect_test "parsing" =
  List.iter
    [ "()" (* empty *)
    ; "((waveform_window$maybe_toggle_module (Enter ())))" (* partial *)
    ; [%string
        {|((waveform_window$maybe_toggle_module (Enter ()))
           (common$scroll_up_fast ((Arrow Up) (Ctrl))))|}]
      (* partial out of order *)
    ]
    ~f:parse_and_ingore
;;

let%expect_test "override works" =
  let override = `ASCII '1', None in
  let keys_with_override =
    parse [%string {|((waveform_window$maybe_toggle_module ((ASCII 1) ())))|}]
  in
  assert (
    Key_actions.Key.equal keys_with_override.waveform_window.maybe_toggle_module override);
  (* Test some sample other key is the default *)
  assert (
    Key_actions.Key.equal
      keys_with_override.common.scroll_up
      Key_actions.default_keys.common.scroll_up)
;;
