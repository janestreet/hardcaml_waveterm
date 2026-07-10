open Base

type t = Display_rule.t list [@@deriving sexp_of]

type format =
  { wave_format : Hardcaml.Wave_format.t option
  ; alignment : Text_alignment.t
  }

type matched_port =
  { port : Port.t
  ; format : format option
  }

type matched_rule =
  | Port of matched_port
  | Divider of string

let compare_matched_rule_by_port s t =
  match s, t with
  | Port _, Divider _ -> -1
  | Divider _, Port _ -> 1
  | Divider _, Divider _ -> 0
  | Port { port; format = _ }, Port { port = port'; format = _ } ->
    Port.compare port port'
;;

let run_rule (t : Display_rule.t) (port : Port.t) : format option =
  match t with
  | Input { wave_format; alignment } ->
    if Hardcaml.Wave_data.Type.equal port.type_ Input
    then Some { wave_format; alignment }
    else None
  | Output { wave_format; alignment } ->
    if Hardcaml.Wave_data.Type.equal port.type_ Output
    then Some { wave_format; alignment }
    else None
  | Default ->
    if port.width = 1
    then Some { wave_format = Some Bit; alignment = Left }
    else Some { wave_format = Some Hex; alignment = Left }
  | Regexp { re; wave_format; alignment } ->
    Option.map
      (Re.exec_opt re (port.port_name |> Port_name.to_string))
      ~f:(fun _ -> { wave_format; alignment })
  | Names { names; wave_format; alignment } ->
    if List.mem names port.port_name ~equal:Port_name.equal
    then Some { wave_format; alignment }
    else None
  | Custom f ->
    Option.map (f port) ~f:(fun wave_format ->
      { wave_format = Some wave_format; alignment = Text_alignment.Left })
  | Custom_with_alignment f ->
    Option.map (f port) ~f:(fun (wave_format, alignment) ->
      { wave_format = Some wave_format; alignment })
  | Divider _ -> (* this is handled before the call *) failwith "Unexpected"
;;

let run_matches rule ~unmatched =
  List.partition_map unmatched ~f:(fun port ->
    match run_rule rule port with
    | Some format -> First (Port { port; format = Some format })
    | None -> Second port)
;;

let sort_matched matched = List.sort matched ~compare:compare_matched_rule_by_port

let rec sort (t : Display_rule.t list) ~unmatched =
  match t with
  | [] -> []
  | Default :: _ ->
    let defaults =
      List.sort unmatched ~compare:Port.compare
      |> List.map ~f:(fun port -> Port { port; format = None })
    in
    [ defaults ]
  | Divider name :: t -> [ Divider name ] :: sort t ~unmatched
  | Names { names; wave_format; alignment } :: t ->
    (* We match each name in turn, so they are in the user specified order *)
    let matched, unmatched =
      List.fold names ~init:([], unmatched) ~f:(fun (matched', unmatched) name ->
        let matched, unmatched =
          run_matches (Names { names = [ name ]; wave_format; alignment }) ~unmatched
        in
        matched :: matched', unmatched)
    in
    List.concat (List.rev matched) :: sort t ~unmatched
  | ((Input _ | Output _ | Custom _ | Custom_with_alignment _) as rule) :: t ->
    let matched, unmatched = run_matches rule ~unmatched in
    matched :: sort t ~unmatched
  | (Regexp _ as rule) :: t ->
    let matched, unmatched = run_matches rule ~unmatched in
    sort_matched matched :: sort t ~unmatched
;;

let is_displayed (t : Display_rule.t list) =
  let has_default_rule =
    List.find t ~f:(function
      | Default -> true
      | _ -> false)
    |> Option.is_some
  in
  if has_default_rule
  then fun _ -> true
  else
    fun port ->
    let rec helper = function
      | [] -> false
      | rule :: rest ->
        (match run_rule rule port with
         | Some _ -> true
         | None -> helper rest)
    in
    helper t
;;

let is_signal_displayed t signal =
  let is_displayed = is_displayed t in
  List.filter
    ~f:(fun name ->
      let port =
        { Port.type_ = Port.Type.Internal
        ; width = Hardcaml.Signal.width signal
        ; port_name = Port_name.of_string name
        }
      in
      is_displayed port)
    (Hardcaml.Signal.names signal)
  |> List.is_empty
  |> not
;;

let sort_ports_and_formats t ports = sort t ~unmatched:ports |> List.concat

module With_interface (I : Hardcaml.Interface.S) = struct
  let get_name ?prefix ?suffix name =
    String.concat
      [ Option.value ~default:"" prefix; name; Option.value ~default:"" suffix ]
  ;;

  let default
    ?prefix
    ?suffix
    ?alignment
    ?(wave_format = Hardcaml.Wave_format.Bit_or Hex)
    ()
    =
    I.map I.port_names ~f:(fun name ->
      Display_rule.port_name_is (get_name ?prefix ?suffix name) ?alignment ~wave_format)
    |> I.to_list
  ;;

  let with_format ?prefix ?suffix ?alignment wave_formats =
    I.map2 I.port_names wave_formats ~f:(fun name wave_format ->
      Display_rule.port_name_is ?alignment (get_name ?prefix ?suffix name) ~wave_format)
    |> I.to_list
  ;;
end
