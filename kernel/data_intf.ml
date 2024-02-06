(** A dynamically-sized array, similar to std::vector in C++. *)

open Base
open Hardcaml

module type Readable = sig
  type t [@@deriving sexp_of, equal]

  val width : t -> int
  val length : t -> int
  val get : t -> int -> Bits.t
end

module type Data = sig
  module type Readable = Readable

  type t [@@deriving sexp_of, compare, equal]

  include Readable with type t := t

  val create : int -> t
  val init : int -> width:int -> f:(int -> Bits.t) -> t
  val set : t -> int -> Bits.t -> unit
  val set_mutable_unsafe : t -> int -> Bits.Mutable.t -> unit
  val set_from_bytes : int -> t -> int -> Bytes.t -> int -> unit
  val non_cache_hits : t -> int

  (** Raw, packed data. The underlying array is resizable, so [Bytes.length] does not
      indicate the actually used data. *)
  val raw_data : t -> Bytes.t

  (** Number of used bytes in [raw_data]. *)
  val used_raw_data_bytes : t -> int
end
