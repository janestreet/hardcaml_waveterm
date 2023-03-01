open Base
open Hardcaml

let check_cache = true

type t =
  { mutable data : Bytes.t (* packed data *)
  ; mutable length : int (* number of elements in array *)
  ; width : int (* bit width *)
  ; rounded_width : int
  (* bit width rounded up to the nearest power of 2 if [width<64], or a multiple of 64. *)
  ; log2_rounded_width : int
  ; mutable cached_bits : Bits.t
  ; mutable cached_sub_word : int
  ; cached_multi_word : Bytes.t
  ; cached_temp_multi_word : Bytes.t
  (* this is needed to compare the underlying values. This is because we cannot do a
     [Bytes.compare] at arbitrary offsets within a buffer. *)
  ; mutable non_cache_hits : int
  }
[@@deriving sexp_of, compare, fields, equal]

let get64 = Stdlib.Bytes.get_int64_ne
let set64 = Stdlib.Bytes.set_int64_ne

let total_length length_in_bytes rounded_width =
  if rounded_width < 8
  then length_in_bytes * (8 / rounded_width)
  else length_in_bytes / (rounded_width / 8)
;;

let[@cold] raise_invalid_width actual_width expected_width =
  raise_s
    [%message
      "Unexpected [Bits.t] width in waveform" (actual_width : int) (expected_width : int)]
;;

let create width =
  let rounded_width =
    if width < 64 then Int.ceil_pow2 width else Int.round_up ~to_multiple_of:64 width
  in
  (* at least 1 64 bit word, or as many words as needed for a single element. *)
  let length_in_bytes = max 8 (rounded_width / 8) in
  { data = Bytes.make length_in_bytes '\000'
  ; length = 0
  ; width
  ; rounded_width
  ; log2_rounded_width = Int.ceil_log2 rounded_width
  ; cached_bits = Bits.zero width
  ; cached_sub_word = 0
  ; cached_multi_word = Bytes.make length_in_bytes '\000'
  ; cached_temp_multi_word = Bytes.make length_in_bytes '\000'
  ; non_cache_hits = 0
  }
;;

let resize t =
  let length_in_bytes = Bytes.length t.data in
  let data = Bytes.make (length_in_bytes * 2) '\000' in
  Bytes.blit ~src:t.data ~src_pos:0 ~dst:data ~dst_pos:0 ~len:length_in_bytes;
  t.data <- data
;;

let total_length t = total_length (Bytes.length t.data) t.rounded_width

(* >= 64 bits *)
let set_multi_word t index bits =
  let underlying_repr = Bits.Expert.unsafe_underlying_repr bits in
  let bytes_per_word = Bits.number_of_data_bytes bits in
  Bytes.blit
    ~src:underlying_repr
    ~src_pos:Bits.Expert.offset_for_data
    ~dst:t.data
    ~dst_pos:(bytes_per_word * index)
    ~len:bytes_per_word
;;

(* < 64 bits *)
let masks = Array.init 6 ~f:(fun i -> Int64.((1L lsl Int.(1 lsl i)) - 1L))

let set_sub_word t index bits =
  let mask = masks.(t.log2_rounded_width) in
  let shift = 6 - t.log2_rounded_width in
  let byte_offset = (index lsr shift) lsl 3 in
  let part_offset = (index land ((1 lsl shift) - 1)) lsl t.log2_rounded_width in
  let new_bits = Bits.unsafe_get_int64 bits 0 in
  let cur_bits = get64 t.data byte_offset in
  let bits =
    Int64.(cur_bits land lnot (mask lsl part_offset) lor (new_bits lsl part_offset))
  in
  set64 t.data byte_offset bits
;;

let rec set t index bits =
  if index < total_length t
  then
    if Bits.width bits <> t.width
    then raise_invalid_width (Bits.width bits : int) (t.width : int)
    else (
      if t.rounded_width < 64
      then set_sub_word t index bits
      else set_multi_word t index bits;
      t.length <- max t.length (index + 1))
  else (
    resize t;
    set t index bits)
;;

(* >= 64 bits *)
let get_multi_word t index =
  let bytes_per_word = t.rounded_width lsr 3 in
  Bytes.blit
    ~src:t.data
    ~src_pos:(bytes_per_word * index)
    ~dst:t.cached_temp_multi_word
    ~dst_pos:0
    ~len:bytes_per_word;
  if check_cache && Bytes.equal t.cached_temp_multi_word t.cached_multi_word
  then t.cached_bits
  else (
    let bits = Bits.zero t.width in
    let bits_underlying_repr = Bits.Expert.unsafe_underlying_repr bits in
    Bytes.blit
      ~src:t.cached_temp_multi_word
      ~src_pos:0
      ~dst:bits_underlying_repr
      ~dst_pos:Bits.Expert.offset_for_data
      ~len:bytes_per_word;
    Bytes.blit
      ~src:t.cached_temp_multi_word
      ~src_pos:0
      ~dst:t.cached_multi_word
      ~dst_pos:0
      ~len:bytes_per_word;
    t.non_cache_hits <- t.non_cache_hits + 1;
    t.cached_bits <- bits;
    bits)
;;

(* < 64 bits *)
let get_sub_word t index =
  (* extract the value *)
  let mask = masks.(t.log2_rounded_width) in
  let shift = 6 - t.log2_rounded_width in
  let byte_offset = (index lsr shift) lsl 3 in
  let part_offset = (index land ((1 lsl shift) - 1)) lsl t.log2_rounded_width in
  let bits = get64 t.data byte_offset in
  let bits = Int64.((bits lsr part_offset) land mask) |> Int64.to_int_trunc in
  (* check if it is cached. *)
  if check_cache && bits = t.cached_sub_word
  then t.cached_bits
  else (
    t.non_cache_hits <- t.non_cache_hits + 1;
    t.cached_sub_word <- bits;
    t.cached_bits <- Bits.of_int ~width:t.width bits;
    t.cached_bits)
;;

let[@cold] raise_index_out_of_bounds index length =
  raise_s [%message "Index into waveform is out of bounds" (index : int) (length : int)]
;;

let get t index =
  if index >= t.length
  then raise_index_out_of_bounds index t.length
  else if t.rounded_width < 64
  then get_sub_word t index
  else get_multi_word t index
;;

let init length ~width ~f =
  let t = create width in
  for index = 0 to length - 1 do
    set t index (f index)
  done;
  t
;;
