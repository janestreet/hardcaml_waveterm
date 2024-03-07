open Core
open Hardcaml
open Hardcaml_waveterm

let regex_engine_flag =
  let open Command.Param in
  let emacs re = Re.Emacs.re re in
  let posix re = Re.Posix.re re in
  let pcre re = Re.Pcre.re re in
  let perl re = Re.Perl.re re in
  let regex_engines = [ "emacs", emacs; "posix", posix; "pcre", pcre; "perl", perl ] in
  let regex_flag_type =
    Command.Arg_type.create (fun regex_engine ->
      let regex_engine = String.lowercase regex_engine in
      match List.Assoc.find regex_engines regex_engine ~equal:String.equal with
      | None ->
        raise_s
          [%message
            "Unknown regex engine"
              (regex_engine : string)
              ~available:(List.map regex_engines ~f:fst : string list)]
      | Some re -> re)
  in
  flag
    "-regex"
    (optional_with_default posix regex_flag_type)
    ~doc:
      "[posix|perl|pcre|emacs] Select regular expression syntax for defining display \
       rules"
;;

let wave_format_flag =
  let open Command.Param in
  let wave_format_type =
    Command.Arg_type.create (fun wave_format ->
      match String.lowercase wave_format with
      | "s" | "int" | "sint" | "signed-int" -> Wave_format.Int
      | "u" | "uint" | "unsigned-int" -> Wave_format.Unsigned_int
      | "b" | "bin" | "binary" -> Wave_format.Binary
      | "h" | "hex" -> Wave_format.Hex
      | wave_format -> raise_s [%message "Invalid wave_format" (wave_format : string)])
  in
  flag
    "-wave-format"
    (optional_with_default Wave_format.Hex wave_format_type)
    ~doc:"[h:b:u:s] How to render multi-bit values (hex, binary, int)"
;;

let get_display_rules ~regex_engine ~display_rules ~wave_format =
  match display_rules with
  | [] -> None
  | _ ->
    Some
      (List.map display_rules ~f:(fun regex ->
         Display_rule.port_name_matches
           (Re.compile (regex_engine regex))
           ~wave_format:(Bit_or wave_format)))
;;

module Make (Data : Hardcaml_waveterm_kernel.Expert.Data.S) = struct
  module Waveterm = Hardcaml_waveterm.Expert.Make (Data)
  module Widget = Hardcaml_waveterm_interactive.Widget.Make (Data) (Waveterm)

  let command_show =
    Command.basic
      ~summary:"Display a hardcaml waveform from a waveform binary dump."
      [%map_open.Command
        let filename = anon ("filename" %: string)
        and wave_width = flag "-scale" (optional int) ~doc:" Waveform scale"
        and start_cycle =
          flag "-start-cycle" (optional int) ~doc:" Initial clock cycle to render"
        and signals_width =
          flag "-signals-width" (optional int) ~doc:" Width of signals display window"
        and values_width =
          flag "-values-width" (optional int) ~doc:" Width of values display window"
        and display_rules =
          flag "-rule" (listed string) ~doc:" Display rule regular expressions"
        and ui_state_file =
          flag
            "-ui-state-file"
            (optional string)
            ~doc:" Location to save/load UI state file"
        and regex_engine = regex_engine_flag
        and wave_format = wave_format_flag in
        fun () ->
          let waveform = Waveterm.Serialize.unmarshall filename in
          let display_rules =
            get_display_rules ~regex_engine ~display_rules ~wave_format
          in
          Widget.run
            ?ui_state_file
            ?signals_width
            ?values_width
            ?start_cycle
            ?wave_width
            ?display_rules
            waveform]
  ;;
end

module Show_cyclesim = Make (Hardcaml.Wave_data)
module Show_evsim = Make (Hardcaml_waveterm_event_store.Bits_store)

let command_convert =
  Command.basic
    ~summary:"Convert serialized hardcaml waveform to VCD"
    [%map_open.Command
      let filename_in = anon ("waveterm_in" %: string)
      and filename_out = anon ("vcd_out" %: string) in
      fun () ->
        let waveform = Waveform.Serialize.unmarshall filename_in in
        let waves = Waveform.waves waveform in
        let in_ports =
          Array.filter_map waves ~f:(function
            | Empty _ -> None
            | Clock _ -> None
            | Binary (name, data) -> Some (name, data, ref (Bits.zero (Data.width data)))
            | Data (name, data, _format, _alignment) ->
              Some (name, data, ref (Bits.zero (Data.width data))))
        in
        (* replay the waveform *)
        let sim =
          Cyclesim.Private.create
            ~in_ports:
              (Array.map in_ports ~f:(fun (name, _, bits) -> name, bits) |> Array.to_list)
            ~out_ports_before_clock_edge:[]
            ~out_ports_after_clock_edge:[]
            ~reset:Fn.id
            ~cycle_check:Fn.id
            ~cycle_before_clock_edge:Fn.id
            ~cycle_at_clock_edge:Fn.id
            ~cycle_after_clock_edge:Fn.id
            ~traced:{ input_ports = []; output_ports = []; internal_signals = [] }
            ~lookup_node:(Fn.const None)
            ~lookup_reg:(Fn.const None)
            ~lookup_mem:(Fn.const None)
            ()
        in
        Out_channel.with_file filename_out ~f:(fun file_out ->
          let sim = Vcd.wrap file_out sim in
          let num_cycles =
            let _, data, _ = in_ports.(0) in
            Data.length data
          in
          for cycle = 0 to num_cycles - 1 do
            Array.iter in_ports ~f:(fun (_, data, port) -> port := Data.get data cycle);
            Cyclesim.cycle sim
          done)]
;;

let () =
  Command_unix.run
    (Command.group
       ~summary:""
       [ "show", Show_cyclesim.command_show
       ; "event-driven", Show_evsim.command_show
       ; "convert", command_convert
       ])
;;
