open! Core
open! Hardcaml_waveterm
open! Filesystem_core

let%expect_test "Marshall should be equal when it doesnt contain Custom wave formats." =
  let waves = Example.testbench () in
  let tmp = Stdlib.Filename.temp_file "waveform" "" in
  Waveform.Serialize.marshall waves tmp;
  let waves' = Waveform.Serialize.unmarshall tmp in
  assert (Waveform.equal waves waves')
;;

let%expect_test "Marshall_here should generate an appropriately-named waveform file" =
  let test () =
    let waves = Example.testbench () in
    Waveform.Serialize.marshall_here waves;
    (Filesystem_core.getcwd () :> File_path.Types.Path.t)
    |> Filesystem_core.ls_dir
    |> List.map ~f:File_path.Types.Part.to_string
    |> List.iter ~f:print_endline
  in
  Filesystem_core.within_temp_dir test;
  [%expect {| test_marshall.hardcamlwaveform |}]
;;
