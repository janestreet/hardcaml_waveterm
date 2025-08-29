open Base
open Hardcaml_waveterm_kernel

module Make
    (Data : Hardcaml_waveterm_kernel.Expert.Data.S)
    (M : Hardcaml_waveterm_kernel.Expert.M(Data).S) =
struct
  open M

  type t =
    { key_table : (Key_actions.Key.t, t -> Event_response.t) Hashtbl.t
    ; key_help : Key_help.t list
    ; bounds : Rect.t
    ; scroll_vert : Scroll.VScrollbar.t
    ; mutable offset : int
    ; max_scroll_offset : int
    }

  let set_offset t offset = t.offset <- offset

  let scroll_vertical t ~by =
    let scrolled_offset = t.offset + by |> max 0 |> min t.max_scroll_offset in
    set_offset t scrolled_offset;
    Scroll.Scrollbar.set_offset t.scroll_vert t.offset
  ;;

  module Keys = Key_handlers.Help_window.Make (struct
      type nonrec t = t

      let scroll_vertical = scroll_vertical
    end)

  let create ?help ~rows ~cols (keys : Key_actions.Key.t Key_actions.t) =
    let key_table =
      match help with
      | None -> Keys.key_table keys.common
      | Some { key_table; _ } -> key_table
    in
    let key_help =
      match help with
      | None ->
        Key_actions.zip Key_actions.key_help Key_actions.default_keys
        |> Key_actions.to_list
        |> List.map ~f:(fun ((_group, descr), key) ->
          { Key_help.key = Key_actions.Key.to_string key; descr })
      | Some { key_help; _ } -> key_help
    in
    let max_scroll_offset = max 0 (List.length key_help - (rows - 2)) in
    let scroll_vert, remaining_bounds =
      Scroll.VScrollbar.create_right_aligned
        ~container:{ r = 0; c = 0; h = rows; w = cols }
        ~range:max_scroll_offset
        ()
    in
    let t =
      { key_table
      ; key_help
      ; bounds = remaining_bounds
      ; scroll_vert
      ; offset = 0
      ; max_scroll_offset
      }
    in
    Scroll.Scrollbar.on_offset_change scroll_vert ~f:(set_offset t);
    t
  ;;

  let draw ~ctx t =
    Render.draw_help
      ~ctx
      ~bounds:(Draw_notty.Border.adjust t.bounds)
      t.key_help
      ~offset:t.offset;
    Scroll.VScrollbar.draw ~ctx ~style:Style.default t.scroll_vert;
    Draw_notty.Border.draw ~ctx ~bounds:t.bounds "Help"
  ;;

  let mouse_handler (t : t) ((button, (col, row), mods) as mouse : Notty.Unescape.mouse) =
    let update_mouse_button_scroll () =
      match button with
      | `Press b ->
        (match b, mods with
         | `Scroll `Up, [] ->
           scroll_vertical t ~by:(-1);
           true
         | `Scroll `Down, [] ->
           scroll_vertical t ~by:1;
           true
         | _ -> false)
      | _ -> false
    in
    let update_scroll_bar () =
      Rect.inside (Scroll.Scrollbar.bounds t.scroll_vert) ~row ~col
      && Scroll.Scrollbar.mouse_event t.scroll_vert mouse
    in
    update_mouse_button_scroll () || update_scroll_bar ()
  ;;

  let key_handler (t : t) key =
    match Hashtbl.find t.key_table (Key_actions.Key.from_notty_encoding key) with
    | Some f -> f t
    | None -> No_action
  ;;

  let handler (t : t) event =
    match event with
    | `Mouse mouse ->
      if mouse_handler t mouse then Event_response.Redraw_window else No_action
    | `Key key -> key_handler t key
  ;;
end
