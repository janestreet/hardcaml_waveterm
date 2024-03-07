open Base

module Data = struct
  type t = Hardcaml.Bits.t [@@deriving sexp_of]

  let none = Hardcaml.Bits.empty
  let merge _ x = x
end

module Time = struct
  type t = int [@@deriving compare, sexp_of]

  let zero = 0
end

module Event_store = Event_store.Make (Time) (Data)

type t =
  { t : Event_store.t
  ; width : int
  ; max_time : int ref
  }
[@@deriving sexp_of]

let create width max_time = { t = Event_store.create (); width; max_time }
let length t = !(t.max_time) + 1
let get t = Event_store.get t.t
let equal _ _ = false
let width t = t.width

let get_digestible_string t =
  let num_events = Event_store.length t.t in
  let data_words = (t.width + 63) / 64 in
  let total_length = ref (8 + 8 + (8 * num_events) + (8 * data_words * num_events)) in
  let bytes = Bytes.create !total_length in
  let pos = ref 0 in
  let set_int64 x =
    assert (!pos + 8 <= !total_length);
    Bytes.unsafe_set_int64 bytes !pos x;
    pos := !pos + 8
  in
  let set_int x = set_int64 (Int64.of_int x) in
  let set_bits b =
    for i = 0 to data_words - 1 do
      set_int64 (Hardcaml.Bits.unsafe_get_int64 b i)
    done
  in
  set_int t.width;
  set_int !(t.max_time);
  for i = 0 to num_events - 1 do
    set_int (Event_store.get_time_at_index t.t i);
    set_bits (Event_store.get_data_at_index t.t i)
  done;
  assert (!pos = !total_length);
  bytes, !total_length
;;

let event_store t = t.t
