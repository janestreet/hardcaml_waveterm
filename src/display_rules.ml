open! Import

type t = Display_rule.t list [@@deriving sexp_of]

let run_rule (t : Display_rule.t) (port : Port.t)
  : (Wave_format.t * Wave_format.alignment) option
  =
  match t with
  | Default -> if port.width = 1 then Some (Bit, Left) else Some (Hex, Left)
  | Regexp { re; wave_format; alignment } ->
    Option.map
      (Re.exec_opt re (port.port_name |> Port_name.to_string))
      ~f:(fun _ -> wave_format, alignment)
  | Names { names; wave_format; alignment } ->
    if List.mem names port.port_name ~equal:Port_name.equal
    then Some (wave_format, alignment)
    else None
  | Custom f -> Option.map (f port) ~f:(fun a -> a, Wave_format.Left)
  | Custom_with_alignment f -> f port
;;

let rec sort (t : Display_rule.t list) ~unmatched =
  match t with
  | [] -> []
  | Default :: _ ->
    let defaults =
      List.sort unmatched ~compare:Port.compare
      |> List.filter_map ~f:(fun port ->
        run_rule Display_rule.Default port
        |> Option.map ~f:(fun (fmt, allgn) -> port, fmt, allgn))
    in
    [ defaults ]
  | rule :: t ->
    let matched, unmatched =
      List.partition_map unmatched ~f:(fun port ->
        match run_rule rule port with
        | Some (fmt, alignment) -> First (port, fmt, alignment)
        | None -> Second port)
    in
    List.sort matched ~compare:[%compare: Port.t * _ * _] :: sort t ~unmatched
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
