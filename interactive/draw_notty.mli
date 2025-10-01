open! Base
open Hardcaml_waveterm_kernel

(** Notty based gfx API. We need to use an intermediate in memory representation for
    notty. *)
include module type of Draw

val to_image : ctx -> Notty.I.t

module Border : sig
  val adjust : Rect.t -> Rect.t
  val draw : ctx:ctx -> bounds:Rect.t -> string -> unit
end
