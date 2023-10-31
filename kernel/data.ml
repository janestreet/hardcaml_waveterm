[@@@ocaml.flambda_o3]

open Base
open Hardcaml

let check_cache = true

type t =
  { mutable data : Bytes.t (* packed data *)
  ; mutable length : int (* number of elements in array *)
  ; mutable total_length : int
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
      (* ; set : Bytes.t -> int -> Bits.t -> unit [@compare.ignore] *)
  ; setter_index : int
  }
[@@deriving sexp_of, compare, fields ~getters, equal]

let get64u = Bytes.unsafe_get_int64
let set64u = Bytes.unsafe_set_int64

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

(* Usable for >= 64 bits *)
let masks = Array.init 6 ~f:(fun i -> Int64.((1L lsl Int.(1 lsl i)) - 1L))
let offset_for_data = 8
let () = assert (offset_for_data = Constant.Expert.offset_for_data)

let[@cold] resize t =
  let length_in_bytes = Bytes.length t.data in
  let data = Bytes.make (length_in_bytes * 2) '\000' in
  Bytes.blit ~src:t.data ~src_pos:0 ~dst:data ~dst_pos:0 ~len:length_in_bytes;
  t.data <- data;
  t.total_length <- t.total_length * 2
;;

let number_of_data_bytes b = Bytes.length b - offset_for_data
let bytes_to_int t = Bytes.unsafe_get_int64 t offset_for_data |> Int64.to_int_trunc

let set_multi_word data index bits =
  let bytes_per = number_of_data_bytes bits in
  Bytes.unsafe_blit
    ~src:bits
    ~src_pos:Bits.Expert.offset_for_data
    ~dst:data
    ~dst_pos:(bytes_per * index)
    ~len:bytes_per
;;

let set log2_rounded_width data index bits =
  let mask = Int64.((1L lsl Int.(1 lsl log2_rounded_width)) - 1L) in
  let shift = 6 - log2_rounded_width in
  let byte_offset = (index lsr shift) lsl 3 in
  let part_offset = (index land ((1 lsl shift) - 1)) lsl log2_rounded_width in
  let new_bits = Bytes.unsafe_get_int64 bits offset_for_data in
  let cur_bits = get64u data byte_offset in
  let bits =
    Int64.(cur_bits land lnot (mask lsl part_offset) lor (new_bits lsl part_offset))
  in
  set64u data byte_offset bits
;;

let set1 d i b = set 0 d i b
let set2 d i b = set 1 d i b
let set4 d i b = set 2 d i b

let set_unsafe log2_rounded_width data index bits =
  let shift = 6 - log2_rounded_width in
  let byte_offset = (index lsr shift) lsl 3 in
  let part_offset = (index land ((1 lsl shift) - 1)) lsl log2_rounded_width in
  let new_bits = Bytes.unsafe_get_int64 bits offset_for_data in
  let cur_bits = get64u data byte_offset in
  let bits = Int64.(cur_bits lor (new_bits lsl part_offset)) in
  set64u data byte_offset bits
;;

let set1_unsafe d i b = set_unsafe 0 d i b
let set2_unsafe d i b = set_unsafe 1 d i b
let set4_unsafe d i b = set_unsafe 2 d i b

let set8 data index bits =
  let bits = bytes_to_int bits in
  Bytes.unsafe_set data index (Char.unsafe_of_int bits)
;;

let set16 data index bits =
  let bits = bytes_to_int bits in
  Bytes.unsafe_set_int16 data (index lsl 1) bits
;;

let set32 data index bits =
  let bits = bytes_to_int bits in
  Bytes.unsafe_set_int32 data (index lsl 2) (Int32.of_int_trunc bits)
;;

let set64 data index bits =
  let byte_offset = index lsl 3 in
  set64u data byte_offset (Bytes.unsafe_get_int64 bits offset_for_data)
;;

let set128 data index bits =
  let byte_offset = index lsl 4 in
  set64u data byte_offset (Bytes.unsafe_get_int64 bits offset_for_data);
  set64u data (byte_offset + 8) (Bytes.unsafe_get_int64 bits (offset_for_data + 8))
;;

let set192 data index bits =
  let byte_offset = (index lsl 4) + (index lsl 3) in
  set64u data byte_offset (Bytes.unsafe_get_int64 bits offset_for_data);
  set64u data (byte_offset + 8) (Bytes.unsafe_get_int64 bits (offset_for_data + 8));
  set64u data (byte_offset + 16) (Bytes.unsafe_get_int64 bits (offset_for_data + 16))
;;

let set256 data index bits =
  let byte_offset = index lsl 5 in
  set64u data byte_offset (Bytes.unsafe_get_int64 bits offset_for_data);
  set64u data (byte_offset + 8) (Bytes.unsafe_get_int64 bits (offset_for_data + 8));
  set64u data (byte_offset + 16) (Bytes.unsafe_get_int64 bits (offset_for_data + 16));
  set64u data (byte_offset + 24) (Bytes.unsafe_get_int64 bits (offset_for_data + 24))
;;

let setters =
  [ 1, set1
  ; 2, set2
  ; 4, set4
  ; 8, set8
  ; 16, set16
  ; 32, set32
  ; 64, set64
  ; 128, set128
  ; 192, set192
  ; 256, set256
  ]
;;

let setters_unsafe =
  [ 1, set1_unsafe
  ; 2, set2_unsafe
  ; 4, set4_unsafe
  ; 8, set8
  ; 16, set16
  ; 32, set32
  ; 64, set64
  ; 128, set128
  ; 192, set192
  ; 256, set256
  ]
;;

let setter_index width =
  let rec f index = function
    | [] -> index
    | (max_width, _) :: tl -> if width <= max_width then index else f (index + 1) tl
  in
  f 0 setters
;;

let setter_fn = List.map setters ~f:snd @ [ set_multi_word ] |> Array.of_list

let setter_fn_unsafe =
  List.map setters_unsafe ~f:snd @ [ set_multi_word ] |> Array.of_list
;;

let rec set_mutable_unsafe t index (bits : Bits.Mutable.t) =
  if index >= t.total_length
  then (
    resize t;
    set_mutable_unsafe t index bits)
  else (
    setter_fn_unsafe.(t.setter_index) t.data index (bits :> Bytes.t);
    t.length <- max t.length (index + 1))
;;

let rec set t index (bits : Bits.t) =
  if Bits.width bits <> t.width
  then raise_invalid_width (Bits.width bits : int) (t.width : int);
  if index >= t.total_length
  then (
    resize t;
    set t index bits)
  else (
    setter_fn.(t.setter_index) t.data index (bits :> Bytes.t);
    t.length <- max t.length (index + 1))
;;

let create width =
  let rounded_width =
    if width < 64 then Int.ceil_pow2 width else Int.round_up ~to_multiple_of:64 width
  in
  (* at least 1 64 bit word, or as many words as needed for a single element. *)
  let length_in_bytes = max 8 (rounded_width / 8) in
  { data = Bytes.make length_in_bytes '\000'
  ; length = 0
  ; total_length = total_length length_in_bytes rounded_width
  ; width
  ; rounded_width
  ; log2_rounded_width = min 9 (Int.ceil_log2 rounded_width)
  ; cached_bits = Bits.zero width
  ; cached_sub_word = 0
  ; cached_multi_word = Bytes.make length_in_bytes '\000'
  ; cached_temp_multi_word = Bytes.make length_in_bytes '\000'
  ; non_cache_hits = 0
  ; setter_index = setter_index width
  }
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
  let bits = get64u t.data byte_offset in
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
  if index >= t.length || index < 0
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
