open Base
open Hardcaml_waveterm_kernel

module type S = sig
  type t

  val create : ?help:t -> rows:int -> cols:int -> Key_actions.Key.t Key_actions.t -> t
  val draw : ctx:Draw.ctx -> t -> unit

  val handler
    :  t
    -> [ `Key of Notty.Unescape.key | `Mouse of Notty.Unescape.mouse ]
    -> Event_response.t
end

module type Help_window = sig
  module Make (Data : Expert.Data.S) (Modl : Expert.M(Data).S) : S
end
