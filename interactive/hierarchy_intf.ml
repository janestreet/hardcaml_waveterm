open Base

module M (Data : Hardcaml_waveterm_kernel.Data.S) = struct
  module type S = sig
    type wave := Data.t Hardcaml_waveterm_kernel.Wave.t

    type node =
      { mutable visible : bool
      ; signals : wave list
      ; children : node Base.Map.M(String).t
      }
    [@@deriving sexp_of]

    type currently_rendered =
      { actual_wave : wave array
      ; for_rendering : wave array
      }
    [@@deriving sexp_of]

    type t =
      { mutable cfg : Hardcaml_waveterm_kernel.Waves.Config.t
      ; mutable cursors : Cursors.t
      ; root : node
      ; mutable currently_rendered : currently_rendered
      }
    [@@deriving sexp_of]

    val iter_nodes : f:(depth:int -> module_name:string -> node -> unit) -> t -> unit
    val set_currently_rendered : t -> unit
    val get_currently_rendered_waves : t -> Data.t Hardcaml_waveterm_kernel.Waves.t
    val of_waves : Data.t Hardcaml_waveterm_kernel.Waves.t -> t
    val change_selected_signal_index : delta:int -> t -> int
    val toggle_selected_module_if_present : t -> bool

    val move_to_delta_on_active_node
      :  start_cycle:int
      -> search_forwards_or_backwards:[< `Backwards | `Forwards ]
      -> t
      -> int option

    val cycle_wave_format : t -> unit
    val reset_wave_format : t -> unit
    val cycle_colour : t -> unit
    val toggle_bold : t -> unit
    val find_actual_wave : t -> int -> wave
    val toggle_module : t -> string -> unit
  end
end

module type Hierarchy = sig
  module M = M
  module Make (Data : Hardcaml_waveterm_kernel.Data.S) : M(Data).S
end
