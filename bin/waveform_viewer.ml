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
  let wave_format_type = Command.Arg_type.create Wave_format.of_string_exn in
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
           (Re (Re.compile (regex_engine regex)))
           ~wave_format:(Bit_or wave_format)))
;;

let command_show =
  Command.basic
    ~summary:"Display a hardcaml waveform from a waveform binary dump."
    [%map_open.Command
      let filename = anon ("filename" %: Filename_unix.arg_type)
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
        let waveform = Hardcaml_waveterm.Waveform.Serialize.unmarshall filename in
        let display_rules = get_display_rules ~regex_engine ~display_rules ~wave_format in
        Hardcaml_waveterm_interactive.Widget.run
          ?ui_state_file
          ?signals_width
          ?values_width
          ?start_cycle
          ?wave_width
          ?display_rules
          waveform]
;;

let command_convert =
  Command.basic
    ~summary:"Convert serialized hardcaml waveform to VCD"
    [%map_open.Command
      let filename_in = anon ("waveterm_in" %: Filename_unix.arg_type)
      and filename_out = anon ("vcd_out" %: Filename_unix.arg_type) in
      fun () ->
        let waves = Waveform.Serialize.unmarshall filename_in in
        Waveform.Serialize.marshall_vcd waves filename_out]
;;

let () =
  Command_unix.run
    (Command.group ~summary:"" [ "show", command_show; "convert", command_convert ])
;;
