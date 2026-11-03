open Base

module type S = Serialize_waveform_intf.S

let sanitize (waves : _ Hardcaml.Wave_data.Wave.t array) =
  Array.map waves ~f:(fun wave ->
    { wave with
      wave_format =
        (match wave.wave_format with
         | Custom _ -> Bit_or Hex
         | _ as x -> x)
    })
;;

(* Each wave-data format gets its own magic header; the header identifies which payload
   type follows. Adding a format must never change the bytes written for the existing
   formats, so that old files remain readable. *)
let mutable_event_magic_header = "HCWME\001"
let mutable_event_four_state_magic_header = "HCWME\002"

module Two_state_events = Hardcaml.Wave_data_in_mutable_events
module Four_state_events = Hardcaml.Wave_data_in_mutable_events_four_state
module For_file_serialization = Hardcaml.Wave_data.For_file_serialization

let bin_writer_to_string writer t =
  let buf = Bin_prot.Utils.bin_dump writer t in
  let length = Bin_prot.Common.buf_len buf in
  let bytes = Bytes.create length in
  Bin_prot.Common.blit_buf_string buf bytes ~len:length;
  Bytes.to_string bytes
;;

let bin_reader_of_string reader string =
  let length = String.length string in
  let buf = Bin_prot.Common.create_buf length in
  Bin_prot.Common.blit_string_buf string buf ~len:length;
  let pos_ref = ref 0 in
  let t = reader.Bin_prot.Type_class.read buf ~pos_ref in
  if not (Int.equal !pos_ref length)
  then
    raise_s
      [%message
        "Bin_prot payload was not fully consumed" ~read:(!pos_ref : int) (length : int)];
  t
;;

let payload_to_string ~header ~bin_writer ~unique_id ~width waves =
  let payload =
    bin_writer_to_string
      (For_file_serialization.bin_writer_t bin_writer)
      (For_file_serialization.of_waves (sanitize waves) ~unique_id ~width)
  in
  header ^ payload
;;

let to_string (t : Hardcaml.Wave_data.t) =
  match t with
  | By_mutable_event waves ->
    payload_to_string
      ~header:mutable_event_magic_header
      ~bin_writer:Two_state_events.bin_writer_t
      ~unique_id:Two_state_events.unique_id
      ~width:Two_state_events.width
      waves
  | By_mutable_event_four_state waves ->
    payload_to_string
      ~header:mutable_event_four_state_magic_header
      ~bin_writer:Four_state_events.bin_writer_t
      ~unique_id:Four_state_events.unique_id
      ~width:Four_state_events.width
      waves
;;

let marshall_waveterm t filename =
  Stdio.Out_channel.write_all filename ~data:(to_string t)
;;

(* Convert a test filename to a reasonable waveform file name *)
let default_waveform_filename filename =
  let name = Stdlib.Filename.basename filename |> Stdlib.Filename.chop_extension in
  let valid_char c = Char.is_alphanum c || Char.equal c '_' || Char.equal c '$' in
  String.map name ~f:(fun c -> if valid_char c then c else '_') ^ ".hardcamlwaveform"
;;

let marshall_here ~(here : [%call_pos]) t =
  if phys_equal here Lexing.dummy_pos
  then raise_s [%message "Must provide ~here:[%here] when using [marshall_here]"];
  let filename = default_waveform_filename here.pos_fname in
  marshall_waveterm t filename
;;

let payload_of_string ~header ~bin_reader ~width data =
  let payload = String.drop_prefix data (String.length header) in
  bin_reader_of_string (For_file_serialization.bin_reader_t bin_reader) payload
  |> For_file_serialization.to_waves ~width
;;

(* Shared by [of_string] and [unmarshall_waveterm], which differ only in what they can say
   about where the unrecognized bytes came from. *)
let decode data ~unrecognized_header : Hardcaml.Wave_data.t =
  let prefix = String.prefix data (String.length mutable_event_magic_header) in
  if String.is_prefix prefix ~prefix:mutable_event_magic_header
  then
    By_mutable_event
      (payload_of_string
         ~header:mutable_event_magic_header
         ~bin_reader:Two_state_events.bin_reader_t
         ~width:Two_state_events.width
         data)
  else if String.is_prefix prefix ~prefix:mutable_event_four_state_magic_header
  then
    By_mutable_event_four_state
      (payload_of_string
         ~header:mutable_event_four_state_magic_header
         ~bin_reader:Four_state_events.bin_reader_t
         ~width:Four_state_events.width
         data)
  else unrecognized_header ~prefix
;;

let of_string data =
  decode data ~unrecognized_header:(fun ~prefix ->
    raise_s [%message "Unrecognized waveform serialization format" (prefix : string)])
;;

let unmarshall_waveterm filename =
  decode (Stdio.In_channel.read_all filename) ~unrecognized_header:(fun ~prefix ->
    raise_s
      [%message
        "Unrecognized waveform serialization format" (filename : string) (prefix : string)])
;;

(* VCD is a 4-state format, so a [.vcd] always reads back as a 4-state waveform, whatever
   it happens to contain. *)
let unmarshall_vcd filename : Hardcaml.Wave_data.t =
  By_mutable_event_four_state
    (Hardcaml.Vcd.read_mutable_event_based_four_state (Hardcaml_vcd.from_file filename))
;;

let unmarshall filename =
  if String.is_suffix filename ~suffix:".vcd"
  then unmarshall_vcd filename
  else unmarshall_waveterm filename
;;

let marshall_vcd (t : Hardcaml.Wave_data.t) filename =
  match t with
  | By_mutable_event_four_state events ->
    Stdio.Out_channel.with_file filename ~f:(fun chan ->
      Hardcaml.Vcd.write_mutable_event_based_four_state chan events)
  | By_mutable_event events ->
    Stdio.Out_channel.with_file filename ~f:(fun chan ->
      Hardcaml.Vcd.write_mutable_event_based chan events)
;;

let marshall (t : Hardcaml.Wave_data.t) filename =
  if String.is_suffix filename ~suffix:".vcd"
  then marshall_vcd t filename
  else marshall_waveterm t filename
;;
