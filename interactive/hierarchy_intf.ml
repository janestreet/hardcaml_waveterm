open Base

module M
    (Data : Hardcaml_waveterm_kernel.Expert.Data.S)
    (Modl : Hardcaml_waveterm_kernel.Expert.M(Data).S) =
struct
  open Modl

  module type S = sig
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

    val iter_nodes : f:(depth:int -> module_name:string -> node -> unit) -> t -> unit
    val set_currently_rendered : t -> unit
    val get_currently_rendered_waves : t -> Waves.t
    val of_waves : Waves.t -> t
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
    val find_actual_wave : t -> int -> Wave.t
    val toggle_module : t -> string -> unit
  end
end

module type Hierarchy = sig
  module M = M

  module Make
      (Data : Hardcaml_waveterm_kernel.Expert.Data.S)
      (Modl : Hardcaml_waveterm_kernel.Expert.M(Data).S) : M(Data)(Modl).S
end
