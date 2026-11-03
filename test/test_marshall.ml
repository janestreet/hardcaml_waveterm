open! Core
open! Hardcaml_waveterm
open! Filesystem_core

let read_prefix filename length =
  let data = In_channel.read_all filename in
  String.prefix data length
;;

let mutable_event_waves () =
  let width = 4 in
  let wave_data = Hardcaml.Wave_data_in_mutable_events.create width (ref 0) in
  List.iter
    [ 0, 1; 3, 2; 5, 15 ]
    ~f:(fun (time, value) ->
      Hardcaml.Wave_data_in_mutable_events.insert
        wave_data
        time
        (Hardcaml.Bits.of_unsigned_int ~width value));
  Hardcaml.Wave_data.By_mutable_event
    [| { Hardcaml.Wave_data.Wave.name = "x"
       ; width
       ; typ = Hardcaml.Wave_data.Type.Internal
       ; wave_format = Hardcaml.Wave_format.Hex
       ; wave_data
       }
    |]
;;

let%expect_test "Marshall should be equal when it doesnt contain Custom wave formats." =
  let waves = Example.testbench () in
  let tmp = Stdlib.Filename.temp_file "waveform" "" in
  Waveform.Serialize.marshall waves tmp;
  let waves' = Waveform.Serialize.unmarshall tmp in
  assert (Waveform.equal waves waves')
;;

let%expect_test "Generated waveforms use the mutable-event bin_io format" =
  let waves = Example.testbench () in
  let tmp = Stdlib.Filename.temp_file "waveform" "" in
  Waveform.Serialize.marshall waves tmp;
  print_s [%message "" ~header:(read_prefix tmp 6 : string)];
  [%expect {| (header "HCWME\001") |}]
;;

let%expect_test "Mutable-event waveforms use the bin_io format" =
  let waves = mutable_event_waves () in
  let tmp = Stdlib.Filename.temp_file "waveform" "" in
  Waveform.Serialize.marshall waves tmp;
  let header = read_prefix tmp 6 in
  let waves' = Waveform.Serialize.unmarshall tmp in
  let roundtripped =
    match waves' with
    | Hardcaml.Wave_data.By_mutable_event [| wave |] ->
      List.init
        (Hardcaml.Wave_data_in_mutable_events.num_events wave.wave_data)
        ~f:(fun index ->
          ( Hardcaml.Wave_data_in_mutable_events.get_time_at_index wave.wave_data index
          , Hardcaml.Wave_data_in_mutable_events.get_data_at_index wave.wave_data index
            |> Hardcaml.Bits.to_unsigned_int ))
    | By_mutable_event _ | By_mutable_event_four_state _ ->
      raise_s [%message "Unexpected waveform data" (waves' : Hardcaml.Wave_data.t)]
  in
  print_s [%message (header : string) (roundtripped : (int * int) list)];
  [%expect {| ((header "HCWME\001") (roundtripped ((0 1) (3 2) (5 15)))) |}]
;;

let mutable_event_four_state_waves () =
  let width = 4 in
  let wave_data = Hardcaml.Wave_data_in_mutable_events_four_state.create width (ref 0) in
  (* (bits, mask) pairs: "XXXX" at time 0, "01ZX" at time 3. *)
  List.iter
    [ 0, "1111", "1111"; 3, "0101", "0011" ]
    ~f:(fun (time, bits, mask) ->
      Hardcaml.Wave_data_in_mutable_events_four_state.insert
        wave_data
        time
        ~bits:(Hardcaml.Bits.of_string bits)
        ~mask:(Hardcaml.Bits.of_string mask));
  Hardcaml.Wave_data.By_mutable_event_four_state
    [| { Hardcaml.Wave_data.Wave.name = "x"
       ; width
       ; typ = Hardcaml.Wave_data.Type.Internal
       ; wave_format = Hardcaml.Wave_format.Hex
       ; wave_data
       }
    |]
;;

let%expect_test "4-state waveforms use their own header and round trip" =
  let waves = mutable_event_four_state_waves () in
  let tmp = Stdlib.Filename.temp_file "waveform" "" in
  Waveform.Serialize.marshall waves tmp;
  let header = read_prefix tmp 6 in
  let waves' = Waveform.Serialize.unmarshall tmp in
  print_s
    [%message
      (header : string)
        ~equal_after_roundtrip:(Hardcaml.Wave_data.equal waves waves' : bool)
        (waves' : Hardcaml.Wave_data.t)];
  [%expect
    {|
    ((header "HCWME\002") (equal_after_roundtrip true)
     (waves'
      (By_mutable_event_four_state
       (((name x) (width 4) (typ Internal) (wave_format Hex)
         (wave_data
          ((time (0 3)) (data (XXXX 01ZX)) (num_events 2) (width 4) (max_time 3))))))))
    |}]
;;

(* Several waves, two of which alias the same wave data, plus a wide (multi-word) wave.
   Pins the [of_waves] de-duplication order and the per-wave data-index encoding, which
   the single-wave cases above cannot exercise. *)
let aliased_and_wide_waves () =
  let make_data ~width events =
    let wave_data = Hardcaml.Wave_data_in_mutable_events.create width (ref 0) in
    List.iter events ~f:(fun (time, value) ->
      Hardcaml.Wave_data_in_mutable_events.insert
        wave_data
        time
        (Hardcaml.Bits.of_unsigned_int ~width value));
    wave_data
  in
  let narrow = make_data ~width:4 [ 0, 1; 3, 2; 5, 15 ] in
  let wide = make_data ~width:65 [ 0, 0; 2, 7 ] in
  Hardcaml.Wave_data.By_mutable_event
    (Array.of_list
       (List.map
          (* [a] and [c] alias [narrow], so the payload holds two wave datas and the wave
             indices must come out as 0, 1, 0. *)
          [ "a", narrow; "b", wide; "c", narrow ]
          ~f:(fun (name, wave_data) ->
            { Hardcaml.Wave_data.Wave.name
            ; width = Hardcaml.Wave_data_in_mutable_events.width wave_data
            ; typ = Hardcaml.Wave_data.Type.Internal
            ; wave_format = Hardcaml.Wave_format.Hex
            ; wave_data
            })))
;;

let%expect_test "aliased waves are serialized once and referenced by index" =
  let waves = aliased_and_wide_waves () in
  let tmp = Stdlib.Filename.temp_file "waveform" "" in
  Waveform.Serialize.marshall waves tmp;
  let waves' = Waveform.Serialize.unmarshall tmp in
  let datas =
    match waves' with
    | Hardcaml.Wave_data.By_mutable_event waves ->
      Array.map waves ~f:(fun wave -> wave.wave_data)
    | By_mutable_event_four_state _ -> assert false
  in
  print_s
    [%message
      ""
        ~equal_after_roundtrip:(Hardcaml.Wave_data.equal waves waves' : bool)
        ~a_and_c_share_data:(phys_equal datas.(0) datas.(2) : bool)
        ~a_and_b_share_data:(phys_equal datas.(0) datas.(1) : bool)];
  [%expect
    {|
    ((equal_after_roundtrip true) (a_and_c_share_data true)
     (a_and_b_share_data false))
    |}]
;;

(* Guards against accidental wire-format changes: old waveform files must remain readable,
   so the bytes written for a fixed waveform must never change. If this test fails, do not
   update the expectation - mint a new magic header for the new format instead. *)
let%expect_test "on-disk bytes are pinned for backwards compatibility" =
  List.iter
    [ mutable_event_waves (); mutable_event_four_state_waves () ]
    ~f:(fun waves ->
      let tmp = Stdlib.Filename.temp_file "waveform" "" in
      Waveform.Serialize.marshall waves tmp;
      let data = In_channel.read_all tmp in
      String.iter data ~f:(fun c -> printf "%02x" (Char.to_int c));
      printf "\n");
  [%expect
    {|
    4843574d45010103000302040308210f00000000000003040501017804020300
    4843574d4502010200030402085f000000000000000402083f0000000000000002040301017804020300
    |}]
;;

(* As above: never rebaseline. A change here means the wire format moved and a new magic
   header is needed instead. *)
let%expect_test "on-disk bytes are pinned for backwards compatibility: aliasing and wide \
                 waves"
  =
  let tmp = Stdlib.Filename.temp_file "waveform" "" in
  Waveform.Serialize.marshall (aliased_and_wide_waves ()) tmp;
  In_channel.read_all tmp |> String.iter ~f:(fun c -> printf "%02x" (Char.to_int c));
  printf "\n";
  [%expect
    {| 4843574d45010203000302040308210f000000000000030405020002410220000000000000000000000000000000000700000000000000000000000000000002410203016104020300016241020301016304020300 |}]
;;

let%expect_test "[to_string] produces exactly the bytes [marshall] writes" =
  (* [marshall] and [to_string] are one codec, not two, so a string encoding can be
     written to a file and read back and vice versa. If these ever diverge, the pinned
     on-disk bytes above stop saying anything about what goes over a wire. *)
  List.iter
    [ "2-state", mutable_event_waves (); "4-state", mutable_event_four_state_waves () ]
    ~f:(fun (description, waves) ->
      let tmp = Stdlib.Filename.temp_file "waveform" "" in
      Waveform.Serialize.marshall waves tmp;
      let encoded = Waveform.Serialize.to_string waves in
      print_s
        [%message
          description
            ~same_bytes_as_file:(String.equal encoded (In_channel.read_all tmp) : bool)
            ~of_string_round_trips:
              (Hardcaml.Wave_data.equal waves (Waveform.Serialize.of_string encoded)
               : bool)
            ~of_string_reads_the_file:
              (Hardcaml.Wave_data.equal
                 waves
                 (Waveform.Serialize.of_string (In_channel.read_all tmp))
               : bool)]);
  [%expect
    {|
    (2-state (same_bytes_as_file true) (of_string_round_trips true)
     (of_string_reads_the_file true))
    (4-state (same_bytes_as_file true) (of_string_round_trips true)
     (of_string_reads_the_file true))
    |}]
;;

let%expect_test "[of_string] rejects bytes that are not a waveform" =
  (* The file-based error names the file; the string-based one has no file to name, so it
     reports the unrecognized header on its own. *)
  let show f = print_s [%sexp (Or_error.try_with f : Hardcaml.Wave_data.t Or_error.t)] in
  Filesystem_core.within_temp_dir (fun () ->
    Out_channel.write_all "not-a-waveform" ~data:"NOTAWAVEFORM";
    show (fun () -> Waveform.Serialize.unmarshall "not-a-waveform"));
  show (fun () -> Waveform.Serialize.of_string "NOTAWAVEFORM");
  [%expect
    {|
    (Error
     ("Unrecognized waveform serialization format" (filename not-a-waveform)
      (prefix NOTAWA)))
    (Error ("Unrecognized waveform serialization format" (prefix NOTAWA)))
    |}]
;;

let%expect_test "4-state waveforms round trip through VCD files" =
  let vcd = Stdlib.Filename.temp_file "waveform" ".vcd" in
  let waves = mutable_event_four_state_waves () in
  print_s [%sexp (waves : Hardcaml.Wave_data.t)];
  [%expect
    {|
    (By_mutable_event_four_state
     (((name x) (width 4) (typ Internal) (wave_format Hex)
       (wave_data
        ((time (0 3)) (data (XXXX 01ZX)) (num_events 2) (width 4) (max_time 3))))))
    |}];
  Waveform.Serialize.marshall waves vcd;
  print_string
    (In_channel.read_all vcd
     |> String.split_lines
     |> List.drop_while ~f:(fun line -> not (String.is_prefix line ~prefix:"$timescale"))
     |> String.concat_lines);
  [%expect
    {|
    $timescale 1ns $end
    $scope module various $end
    $var wire 4 ! x $end
    $upscope $end
    $enddefinitions $end
    $dumpvars
    bxxxx !
    $end
    #0
    bxxxx !
    #3
    b01zx !
    |}];
  let waves = Waveform.Serialize.unmarshall vcd in
  print_s [%sexp (waves : Hardcaml.Wave_data.t)];
  [%expect
    {|
    (By_mutable_event_four_state
     (((name x) (width 4) (typ Internal) (wave_format (Bit_or Hex))
       (wave_data
        ((time (0 3)) (data (XXXX 01ZX)) (num_events 2) (width 4) (max_time 3))))))
    |}]
;;

let%expect_test "VCD waveforms are read and cached in the 4-state mutable-event format" =
  (* VCD is a 4-state format, so a [.vcd] always reads back 4-state, even one written from
     a 2-state waveform - hence the [\002] header when the result is cached natively. *)
  let vcd = Stdlib.Filename.temp_file "waveform" ".vcd" in
  let cache = Stdlib.Filename.temp_file "waveform" "" in
  Waveform.Serialize.marshall (mutable_event_waves ()) vcd;
  let waves = Waveform.Serialize.unmarshall vcd in
  Waveform.Serialize.marshall waves cache;
  print_s [%message "" ~header:(read_prefix cache 6 : string)];
  [%expect {| (header "HCWME\002") |}]
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
