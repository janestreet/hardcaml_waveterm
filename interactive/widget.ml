open Base
open! Async

module Make
    (Data : Hardcaml_waveterm_kernel.Expert.Data.S)
    (M : Hardcaml_waveterm_kernel.Expert.M(Data).S) =
struct
  open M
  module Waveform_window = Waveform_window.Make (Data) (M)
  module Help_window = Help_window.Make (Data) (M)

  module Context = struct
    type t =
      { term : Notty_async.Term.t
      ; keys : Key_actions.Key.t Key_actions.t
      ; mutable rows : int
      ; mutable cols : int
      ; waves : Wave.t array
      ; mutable waveform : Waveform_window.t
      ; mutable help : Help_window.t
      ; events : [ Notty.Unescape.event | `Resize of int * int ] Pipe.Reader.t
      ; stop : unit Deferred.t
      ; mutable draw_ctx : Draw_notty.ctx
      ; mutable active : [ `Waveform | `Help ]
      }

    (* Look for a hardcaml waveterm rc file which stores key bindings.  In order:

     - Look at HARDCAMLWAVETERMRC envvar
     - Look in local dir
     - Look in home dir (via HOME envvar)
    *)

    let keys () =
      let rcfilename = ".hardcamlwavetermrc" in
      let parse rcfilename =
        match Core.Sexp.load_sexp_conv rcfilename Key_actions.File_format.t_of_sexp with
        | exception _ -> None
        | `Error _ -> None
        | `Result keys -> Some keys
      in
      let load_from_envvar () =
        let%bind.Option rcfilename = Sys.getenv "HARDCAMLWAVETERMRC" in
        parse rcfilename
      in
      let load_from_localdir () = parse rcfilename in
      let load_from_homedir () =
        let%bind.Option homedir = Sys.getenv "HOME" in
        parse (homedir ^ "/" ^ rcfilename)
      in
      List.fold
        ~init:None
        [ load_from_envvar; load_from_localdir; load_from_homedir ]
        ~f:(fun r f -> if Option.is_none r then f () else r)
      |> Option.value ~default:Key_actions.default_keys
    ;;

    let create ~ui_state_file waves =
      let%bind term = Notty_async.Term.create () in
      let keys = keys () in
      let cols, rows = Notty_async.Term.size term in
      let waveform = Waveform_window.create ~cols ~rows ~ui_state_file waves keys in
      let help = Help_window.create ~cols ~rows keys in
      let events = Notty_async.Term.events term in
      let stop = Pipe.closed events in
      let%bind () = Notty_async.Term.cursor term None in
      let draw_ctx = Draw_notty.init ~rows ~cols in
      return
        { term
        ; keys
        ; rows
        ; cols
        ; waves = waves.waves
        ; events
        ; stop
        ; waveform
        ; help
        ; draw_ctx
        ; active = `Waveform
        }
    ;;

    let resize ~rows ~cols t =
      let waveform =
        Waveform_window.create
          ~waveform:t.waveform
          ~cols
          ~rows
          ~ui_state_file:(Waveform_window.ui_state_file t.waveform)
          { waves = t.waves; cfg = (Waveform_window.hierarchy t.waveform).cfg }
          t.keys
      in
      let help = Help_window.create ~cols ~rows t.keys in
      let draw_ctx = Draw_notty.init ~rows ~cols in
      t.rows <- rows;
      t.cols <- cols;
      t.waveform <- waveform;
      t.help <- help;
      t.draw_ctx <- draw_ctx
    ;;

    let draw (ctx : t) =
      (match ctx.active with
       | `Waveform -> Waveform_window.draw ~ctx:ctx.draw_ctx ctx.waveform
       | `Help -> Help_window.draw ~ctx:ctx.draw_ctx ctx.help);
      let image = Draw_notty.to_image ctx.draw_ctx in
      Notty_async.Term.image ctx.term image
    ;;

    let handle_event ctx event =
      let handler event =
        match ctx.active with
        | `Waveform -> Waveform_window.handler ctx.waveform event
        | `Help -> Help_window.handler ctx.help event
      in
      let response =
        match event with
        | `Mouse m -> handler (`Mouse m)
        | `Key key -> handler (`Key key)
        | `Resize (cols, rows) ->
          resize ~rows ~cols ctx;
          Redraw_window
        | `Paste _ -> No_action
      in
      match response with
      | Escape ->
        Pipe.close_read ctx.events;
        false
      | Recreate_window ->
        let hierarchy = Waveform_window.hierarchy ctx.waveform in
        ctx.waveform
        <- Waveform_window.create
             ~waveform:ctx.waveform
             ~cols:ctx.cols
             ~rows:ctx.rows
             ~ui_state_file:(Waveform_window.ui_state_file ctx.waveform)
             { waves = ctx.waves; cfg = hierarchy.cfg }
             ctx.keys;
        true
      | Show_help ->
        ctx.active <- `Help;
        true
      | Show_waveform ->
        ctx.active <- `Waveform;
        true
      | No_action -> false
      | Redraw_window -> true
    ;;

    let iter_events ctx =
      (* process events in batches and draw at the end. Given rendering can be slow, this
         behaves much better - especially over a ssh connection. *)
      Pipe.iter' ctx.events ~f:(fun q ->
        let redraw =
          Core.Queue.fold q ~init:false ~f:(fun redraw event ->
            if handle_event ctx event then true else redraw)
        in
        if redraw then draw ctx else return ())
    ;;
  end

  let run_waves ?(ui_state_file = "/tmp/hardcaml_waveterm_state.sexp") (waves : M.Waves.t)
    =
    Array.iter waves.waves ~f:(function
      | Empty _ -> ()
      | Clock { style; _ } -> style.style <- Waveform_window.default_waves_style
      | Binary { style; _ } -> style.style <- Waveform_window.default_waves_style
      | Data { style; _ } -> style.style <- Waveform_window.default_waves_style);
    let%bind ctx = Context.create ~ui_state_file waves in
    let%bind () = Context.draw ctx in
    don't_wait_for (Context.iter_events ctx);
    ctx.stop
  ;;

  let run_and_return ?ui_state_file waves =
    Thread_safe.block_on_async (fun () -> run_waves ?ui_state_file waves) |> Result.ok_exn
  ;;

  let run_and_close ?ui_state_file waves =
    don't_wait_for
      (Monitor.protect
         (fun () -> run_waves ?ui_state_file waves)
         ~finally:(fun _ ->
           shutdown 0;
           return ()));
    Core.never_returns (Scheduler.go ())
  ;;

  let run
    ?ui_state_file
    ?(signals_width = 20)
    ?(values_width = 20)
    ?(start_cycle = 0)
    ?(wave_width = 3)
    ?display_rules
    t
    =
    run_and_close
      ?ui_state_file
      { cfg =
          { Waves.Config.default with
            start_cycle
          ; wave_width
          ; signals_width
          ; values_width
          }
      ; waves = Waveform.sort_ports_and_formats t display_rules
      }
  ;;
end
