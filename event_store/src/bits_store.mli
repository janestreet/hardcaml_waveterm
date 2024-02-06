open Base
module Time : Event_store.Time with type t = int
module Data : Event_store.Data with type t = Hardcaml.Bits.t
include Event_store.M(Time)(Data).S
