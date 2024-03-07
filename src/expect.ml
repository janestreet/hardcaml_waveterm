open Base
open Hardcaml_waveterm_kernel.Expert
module M = Expect_intf.M

module Make
  (Data : Data.S)
  (Wave : Wave.M(Data).S)
  (Waves : Waves.M(Data)(Wave).S)
  (Render : Render.M(Data)(Wave)(Waves).S)
  (Waveform : Waveform.M(Data)(Wave)(Waves)(Render).S) =
struct
  module Serialize_waveform =
    Serialize_waveform.Make (Data) (Wave) (Waves) (Render) (Waveform)

  module Digest = Waveform_digest.Make (Data) (Wave) (Waves) (Render) (Waveform)

  let serialize_expect_test_output =
    lazy
      (match Sys.getenv "EXPECT_TEST_WAVEFORM" with
       | None -> `none
       | Some "1" | Some "true" -> `by_name
       | Some "digest" -> `by_digest
       | _ -> `none)
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
    Waveform.print
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
    let digest = lazy (Digest.to_hex_string (Digest.create t)) in
    if show_digest then Stdio.print_endline (Lazy.force digest);
    let serialize_to =
      match serialize_to, Lazy.force serialize_expect_test_output with
      | (Some _ | None), `by_digest -> Some (Lazy.force digest)
      | None, (`none | `by_name) -> None
      | Some _, `none -> None
      | Some name, `by_name -> Some name
    in
    Option.iter serialize_to ~f:(fun serialize_to ->
      let serialize_to =
        if String.equal (Stdlib.Filename.extension serialize_to) extension
        then serialize_to
        else Printf.sprintf "%s.%s" serialize_to extension
      in
      Serialize_waveform.marshall t serialize_to)
  ;;
end
