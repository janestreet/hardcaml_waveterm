open Base
open Hardcaml_waveterm_kernel

module Scroll_bar_mode : sig
  type t =
    | Fixed of int
    | Dynamic of int
  [@@deriving sexp_of]
end

module Mouse_mode : sig
  type t =
    | Middle
    | Ratio
    | Auto
  [@@deriving sexp_of]
end

module Scroll_bar_style : sig
  type t =
    | Filled
    | Outline
  [@@deriving sexp_of]
end

module Orientation : sig
  type t =
    | Horz
    | Vert
  [@@deriving sexp_of]
end

module Scrollbar : sig
  type t [@@deriving sexp_of]

  val mouse_event : t -> Notty.Unescape.mouse -> bool
  val bounds : t -> Rect.t
  val set_bounds : t -> Rect.t -> unit
  val offset : t -> int
  val set_offset : t -> int -> unit
  val set_offset_min : t -> unit
  val set_offset_max : t -> unit
  val on_offset_change : t -> f:(int -> unit) -> unit
  val set_mode : t -> Scroll_bar_mode.t -> unit
end

module HScrollbar : sig
  type t = Scrollbar.t [@@deriving sexp_of]

  val create : bounds:Rect.t -> range:int -> unit -> t

  (** Create a scroll bar at the bottom of the given container. Returns the adjust
      container bounds that excludes the scrollbar. *)
  val create_bottom_aligned
    :  ?c:int (** defaults to the right hand side of the container *)
    -> ?w:int (** defaults to the full width of the container *)
    -> ?h:int (** defaults to a standard small height *)
    -> container:Rect.t
    -> range:int
    -> unit
    -> t * Rect.t

  val draw : ctx:Draw_notty.ctx -> style:Style.t -> t -> unit
end

module VScrollbar : sig
  type t = Scrollbar.t [@@deriving sexp_of]

  val create : bounds:Rect.t -> range:int -> unit -> t

  (** Create a scroll bar at the right of the given container. Returns the adjust
      container bounds that excludes the scrollbar. *)
  val create_right_aligned
    :  ?r:int (** defaults to the top of the container *)
    -> ?h:int (** defaults to the full height of the container *)
    -> ?w:int (** defaults to a standard small width *)
    -> container:Rect.t
    -> range:int
    -> unit
    -> t * Rect.t

  val draw : ctx:Draw_notty.ctx -> style:Style.t -> t -> unit
end
