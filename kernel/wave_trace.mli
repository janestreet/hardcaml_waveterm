(** Scalar and bus trace drawing, at either multiple characters per time step or multiple
    time steps per character. Scalar traces use two rows; buses use three rows with
    formatted, aligned values between the lines. Text occupies one cell per UTF-8
    codepoint; malformed sequences draw as U+FFFD.

    Before the first event, four-state data is X and two-state data is 0. Scalar X is
    lettered on the high line and Z on the low line. A letter replaces the glyph in its
    cell, including an edge glyph. The loops may instead retain an edge and place letters
    later in the region. Fuzz (multiple transitions in a character) is never lettered.
    When lettering a character containing both X and Z, X wins.

    Each draw operation selects the mode once with [Data.is_four_state]. Two-state data
    never reads the mask plane; the drawing loops reuse one zero mask per wave. *)

open Base
open Hardcaml

module type Target = sig
  type ctx
  type style

  (** Both primitives must clip to [bounds] themselves; the engine relies on it.
      Coordinates are relative to the origin of [bounds]. *)
  val draw_glyph
    :  ctx:ctx
    -> style:style
    -> bounds:Rect.t
    -> r:int
    -> c:int
    -> Glyph.t
    -> unit

  val draw_int : ctx:ctx -> style:style -> bounds:Rect.t -> r:int -> c:int -> int -> unit
end

module Make (Data : Hardcaml.Wave_data.S) (Target : Target) : sig
  (** Read the value in effect at a time clipped to the data's range. [default] must be a
      shared zero of [Data.width data] bits and [four_state] must match the data's mode.
      Zero-length data returns all ones for four-state data and [default] for two-state
      data. *)
  val get_data_bounds_clipped
    :  Data.t
    -> int
    -> four_state:bool
    -> default:Bits.t
    -> Bits.t

  (** As above for the mask plane. Two-state reads always return [default]; a zero-length
      four-state store returns all ones of [Data.width data] bits. *)
  val get_mask_bounds_clipped
    :  Data.t
    -> int
    -> four_state:bool
    -> default:Bits.t
    -> Bits.t

  val draw_binary_data
    :  Data.t
    -> ctx:Target.ctx
    -> style:Target.style
    -> bounds:Rect.t
    -> wave_scale:[ `Time_steps_per_char of int | `Chars_per_time_step of int ]
    -> off:int
    -> unit

  val draw_data
    :  Data.t
    -> ctx:Target.ctx
    -> style:Target.style
    -> bounds:Rect.t
    -> to_str:(bits:Bits.t -> mask:Bits.t -> string)
    -> alignment:Text_alignment.t
    -> wave_scale:[ `Time_steps_per_char of int | `Chars_per_time_step of int ]
    -> off:int
    -> unit

  module To_render : sig
    (** Values are held as raw [Four_state_value] planes; on two-state data the masks are
        all zeros. *)
    type 'a t =
      { at_most_one_transition : bool
      ; first : 'a
      ; first_mask : 'a
      ; last : 'a
      ; last_mask : 'a
      }
  end

  module For_testing : sig
    val get_transition_data
      :  prev_data:(Bits.t * Bits.t) option
      -> data:Data.t
      -> time_steps_per_char:int
      -> off:int
      -> Bits.t To_render.t
  end
end
