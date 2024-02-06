open! Base

module type Data = sig
  type t [@@deriving sexp_of]

  val none : t
  val merge : t -> t -> t
end

module type Time = sig
  type t [@@deriving compare, sexp_of]

  val zero : t
end

module M (Time : Time) (Data : Data) = struct
  module type S = sig
    type t [@@deriving sexp_of]

    val create : unit -> t
    val set : t -> int -> Time.t -> Data.t -> unit
    val get_time_at_index : t -> int -> Time.t
    val get_data_at_index : t -> int -> Data.t
    val length : t -> int
    val capacity : t -> int
    val resize : t -> unit
    val find_insertion_index : t -> Time.t -> int option
    val insert : t -> Time.t -> Data.t -> unit
    val get : t -> Time.t -> Data.t
  end
end

module type Event_store = sig
  module type Data = Data
  module type Time = Time

  module M = M
  module Make (Time : Time) (Data : Data) : M(Time)(Data).S
end
