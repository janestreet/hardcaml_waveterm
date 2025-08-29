open! Core

module type Waveform_window_api = sig
  type t
  type waves_config
  type hierarchy

  val cfg : t -> waves_config
  val cursors : t -> Cursors.t
  val hierarchy : t -> hierarchy
  val cycles_displayed : t -> int
  val set_cycle_offset : t -> int -> unit
  val get_cycle_offset : t -> int
  val set_signal_offset : t -> int -> unit
  val get_signal_offset : t -> int
  val max_cycle_offset : t -> int
  val max_signal_offset : t -> int
  val change_selected_signal_index : t -> by:int -> int
  val max_view : t -> (int * int option) option
  val get_cursor_offset : t -> int option
  val set_cursor_offset : t -> int -> unit
  val scroll_step : t -> int
  val ui_state_file : t -> string
end

module type Help_api = sig
  type t

  val scroll_vertical : t -> by:int -> unit
end

module type Key_handlers = sig
  module Waveform_window : sig
    module Make
        (Data : Hardcaml_waveterm_kernel.Expert.Data.S)
        (Modl : Hardcaml_waveterm_kernel.Expert.M(Data).S)
        (Hierarchy : Hierarchy.M(Data)(Modl).S)
        (Ui_state : Ui_state.M(Data)(Modl)(Hierarchy).S)
        (Api : Waveform_window_api
               with type waves_config := Modl.Waves.Config.t
                and type hierarchy := Hierarchy.t) : sig
      val key_table
        :  Key_actions.Key.t Key_actions.t
        -> (Api.t -> Event_response.t) Hashtbl.M(Key_actions.Key).t
    end
  end

  module Help_window : sig
    module Make (Api : Help_api) : sig
      val key_table
        :  Key_actions.Key.t Key_actions.Common.t
        -> (Api.t -> Event_response.t) Hashtbl.M(Key_actions.Key).t
    end
  end
end
