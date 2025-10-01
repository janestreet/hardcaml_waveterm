open Base

include struct
  open Hardcaml_waveterm_kernel
  module Style = Style
end

module M = Hierarchy_intf.M

module Make
    (Data : Hardcaml_waveterm_kernel.Expert.Data.S)
    (M : Hardcaml_waveterm_kernel.Expert.M(Data).S) =
struct
  open M

  type node =
    { mutable visible : bool
    ; signals : Wave.t list
    ; children : node Base.Map.M(String).t
    }
  [@@deriving sexp_of]

  type currently_rendered =
    { actual_wave : Wave.t array
    ; for_rendering : Wave.t array
    }
  [@@deriving sexp_of]

  type t =
    { mutable cfg : Waves.Config.t
    ; mutable cursors : Cursors.t
    ; root : node
    ; mutable currently_rendered : currently_rendered
    }
  [@@deriving sexp_of]

  let empty_node = { visible = false; signals = []; children = Map.empty (module String) }

  let rec update ~path ~wave t =
    match path with
    | [] -> assert false
    | [ _ ] -> { t with signals = (* The list is built backwards *) wave :: t.signals }
    | hd :: tl ->
      let children =
        Map.update t.children hd ~f:(function
          | None -> update ~path:tl ~wave empty_node
          | Some x -> update ~path:tl ~wave x)
      in
      { t with children }
  ;;

  (* Restore order of signals relative to the input order. *)
  let rec put_back_into_display_order (node : node) =
    { node with
      signals = List.rev node.signals
    ; children = Base.Map.map node.children ~f:put_back_into_display_order
    }
  ;;

  let move_to_delta_on_active_node ~start_cycle ~search_forwards_or_backwards (t : t) =
    let selected_wave_index = t.cfg.selected_signal in
    match t.currently_rendered.actual_wave.(selected_wave_index) with
    | Clock _ | Empty _ -> None
    | Binary { data; _ } | Data { data; _ } ->
      let len = Data.length data in
      let start_cycle =
        match search_forwards_or_backwards with
        | `Forwards -> start_cycle
        | `Backwards -> start_cycle - 1
      in
      let%bind.Option initial =
        if 0 <= start_cycle && start_cycle < len
        then Some (Data.get data start_cycle)
        else None
      in
      let inc_or_dec i =
        match search_forwards_or_backwards with
        | `Forwards -> i + 1
        | `Backwards -> i - 1
      in
      let rec loop i =
        let[@inline always] mismatch_with_original () =
          Hardcaml.Bits.(not (to_bool (Data.get data i ==: initial)))
        in
        if i < 0 || i >= len
        then None
        else if mismatch_with_original ()
        then
          Some
            (match search_forwards_or_backwards with
             | `Forwards -> i
             | `Backwards -> i + 1)
        else loop (inc_or_dec i)
      in
      loop (inc_or_dec start_cycle)
  ;;

  let cycle_wave_format (t : t) =
    let selected_wave_index = t.cfg.selected_signal in
    match t.currently_rendered.actual_wave.(selected_wave_index) with
    | Clock _ | Empty _ | Binary _ -> ()
    | Data { wave_format; _ } ->
      wave_format.current
      <- (match wave_format.current with
          (* Cycle through these basic numeric formats *)
          | Binary -> Hex
          | Hex -> Unsigned_int
          | Unsigned_int -> Int
          | Int -> Binary
          (* Default to Binary *)
          | Bit -> Binary
          | Bit_or _ -> Binary
          | Index _ -> Binary
          | Custom _ -> Binary
          | Map _ -> Binary)
  ;;

  let reset_wave_format (t : t) =
    let selected_wave_index = t.cfg.selected_signal in
    match t.currently_rendered.actual_wave.(selected_wave_index) with
    | Clock _ | Empty _ | Binary _ -> ()
    | Data { wave_format; _ } -> wave_format.current <- wave_format.default
  ;;

  let cycle_colour (t : t) =
    let selected_wave_index = t.cfg.selected_signal in
    let next_colour (colour : Style.colour) : Style.colour =
      match colour with
      | Black -> Red
      | Red -> Green
      | Green -> Yellow
      | Yellow -> Blue
      | Blue -> Magenta
      | Magenta -> Cyan
      | Cyan -> White
      | White -> Black
    in
    let cycle_fg_colour { Style.bold; fg; bg } =
      let fg = next_colour fg in
      let fg = if Style.equal_colour fg bg then next_colour fg else fg in
      { Style.bold; fg; bg }
    in
    match t.currently_rendered.actual_wave.(selected_wave_index) with
    | Empty _ -> ()
    | Clock { style; _ } -> style.style <- cycle_fg_colour style.style
    | Binary { style; _ } -> style.style <- cycle_fg_colour style.style
    | Data { style; _ } -> style.style <- cycle_fg_colour style.style
  ;;

  let toggle_bold (t : t) =
    let selected_wave_index = t.cfg.selected_signal in
    let toggle_bold { Style.bold; fg; bg } = { Style.bold = not bold; fg; bg } in
    match t.currently_rendered.actual_wave.(selected_wave_index) with
    | Empty _ -> ()
    | Clock { style; _ } -> style.style <- toggle_bold style.style
    | Binary { style; _ } -> style.style <- toggle_bold style.style
    | Data { style; _ } -> style.style <- toggle_bold style.style
  ;;

  let iter_nodes ~f t =
    let rec loop ~depth ~rev_path node =
      Map.iteri node.children ~f:(fun ~key ~data:node ->
        let module_name = String.concat ~sep:"$" (List.rev (key :: rev_path)) in
        f ~depth ~module_name node;
        loop ~rev_path:(key :: rev_path) ~depth:(depth + 1) node)
    in
    loop ~depth:0 ~rev_path:[] t.root
  ;;

  let iter_waves ~f t =
    let rec loop ~depth ~rev_path node =
      if node.visible
      then (
        Map.iteri node.children ~f:(fun ~key ~data:node ->
          let module_name = String.concat ~sep:"$" (List.rev (key :: rev_path)) in
          f ~depth (Wave.Empty { name = module_name });
          loop ~rev_path:(key :: rev_path) ~depth:(depth + 1) node);
        List.iter ~f:(fun node -> f ~depth node) node.signals)
    in
    loop ~depth:0 ~rev_path:[] t.root
  ;;

  let set_currently_rendered t =
    let actual_wave = ref [] in
    let for_rendering = ref [] in
    let () =
      iter_waves t ~f:(fun ~depth w ->
        actual_wave := w :: !actual_wave;
        let padding = String.init (depth * 2) ~f:(fun _ -> ' ') in
        let name = String.split ~on:'$' (Wave.get_name w) |> List.last_exn in
        let name =
          match w with
          | Empty _ -> "<" ^ name ^ ">"
          | _ -> name
        in
        for_rendering := Wave.set_name w (padding ^ name) :: !for_rendering)
    in
    let for_rendering = Array.of_list_rev !for_rendering in
    let actual_wave = Array.of_list_rev !actual_wave in
    t.currently_rendered <- { for_rendering; actual_wave }
  ;;

  let of_waves (waves : Waves.t) =
    let init = empty_node in
    let ret =
      Array.fold waves.waves ~init ~f:(fun acc wave ->
        let path = String.split ~on:'$' (Wave.get_name wave) in
        update ~path ~wave acc)
    in
    ret.visible <- true;
    let t =
      { cfg = waves.cfg
      ; cursors = Cursors.create ()
      ; root = put_back_into_display_order ret
      ; currently_rendered = { actual_wave = [||]; for_rendering = [||] }
      }
    in
    set_currently_rendered t;
    t
  ;;

  let toggle_module t name =
    let rec loop path (node : node) =
      match path with
      | [] -> node.visible <- not node.visible
      | hd :: tl -> loop tl (Map.find_exn node.children hd)
    in
    (try loop (String.split name ~on:'$') t.root with
     | Not_found_s _ -> raise_s [%message "Cannot resolve key in module" name]);
    set_currently_rendered t
  ;;

  let find_actual_wave t i = t.currently_rendered.actual_wave.(i)

  let get_currently_rendered_waves t =
    let currently_rendered = t.currently_rendered.for_rendering in
    { Waves.cfg = t.cfg; waves = currently_rendered }
  ;;

  let change_selected_signal_index ~delta t =
    let c = t.currently_rendered in
    let next =
      t.cfg.selected_signal + delta
      |> Int.min (Array.length c.for_rendering - 1)
      |> Int.max 0
    in
    t.cfg.selected_signal <- next;
    next
  ;;

  let toggle_selected_module_if_present t =
    match t.currently_rendered.actual_wave.(t.cfg.selected_signal) with
    | Empty { name } ->
      toggle_module t name;
      true
    | Clock _ | Data _ | Binary _ -> false
  ;;
end
