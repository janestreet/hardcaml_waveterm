open Base

let serialize_expect_test_output =
  lazy
    (match Sys.getenv "EXPECT_TEST_WAVEFORM" with
     | None -> false
     | Some "1" -> true
     | Some "true" -> true
     | _ -> false)
;;

let expect
  ?display_rules
  ?display_width
  ?display_height
  ?display_values
  ?wave_width
  ?wave_height
  ?signals_width
  ?start_cycle
  ?signals_alignment
  ?(show_digest = true)
  ?serialize_to
  t
  =
  let extension = "hardcamlwaveform" in
  Hardcaml_waveterm_kernel.Waveform.print
    ?display_rules
    ?display_width
    ?display_height
    ?display_values
    ?wave_width
    ?wave_height
    ?signals_width
    ?start_cycle
    ?signals_alignment
    t;
  if show_digest
  then (
    let digest = Waveform_digest.create t in
    Stdio.print_endline (Waveform_digest.to_hex_string digest));
  match serialize_to with
  | None -> ()
  | Some serialize_to ->
    let serialize_to =
      if String.equal (Stdlib.Filename.extension serialize_to) extension
      then serialize_to
      else Printf.sprintf "%s.%s" serialize_to extension
    in
    if Lazy.force serialize_expect_test_output
    then Serialize_waveform.marshall t serialize_to
;;
