open! Import

let clock = Wave.Clock "clock"

let random_string ~max_length =
  String.init
    (Random.int max_length + 1)
    ~f:(fun _ -> Random.int 26 + Char.to_int 'a' |> Char.of_int_exn)
;;

let random_bits ~length ~width = Data.init length ~width ~f:(fun _ -> Bits.random ~width)

let random_format () =
  match Random.int 4 with
  | 0 -> Wave_format.Unsigned_int
  | 1 -> Wave_format.Int
  | 2 -> Wave_format.Hex
  | _ -> Wave_format.Binary
;;

let random_data ~prefix ~length =
  if Random.int 2 = 0
  then Wave.Binary (prefix ^ random_string ~max_length:20, random_bits ~length ~width:1)
  else
    Wave.Data
      ( prefix ^ random_string ~max_length:20
      , random_bits ~length ~width:(Random.int 64 + 1)
      , random_format ()
      , Left )
;;

let create ~prefix ~length ~num_signals =
  { Waves.cfg = Waves.Config.default
  ; waves =
      clock :: List.init num_signals ~f:(fun i -> random_data ~prefix:(prefix i) ~length)
      |> Array.of_list
  }
;;

let%expect_test "1 bit cases" =
  let t = Data.init 48 ~width:1 ~f:(fun i -> if i < 32 then Bits.vdd else Bits.gnd) in
  print_s [%message (t : Data.t)];
  let t = Data.init 48 ~width:1 ~f:(fun i -> if i < 32 then Bits.gnd else Bits.vdd) in
  print_s [%message (t : Data.t)];
  let t = Data.init 32 ~width:1 ~f:(fun i -> if i % 2 = 0 then Bits.gnd else Bits.vdd) in
  print_s [%message (t : Data.t)];
  let t = Data.init 66 ~width:1 ~f:(fun i -> if i % 2 = 0 then Bits.gnd else Bits.vdd) in
  print_s [%message (t : Data.t)];
  [%expect
    {|
    (t (
      (data          "\255\255\255\255\000\000\000\000")
      (length        48)
      (total_length  64)
      (width         1)
      (rounded_width 1)
      (log2_rounded_width 0)
      (cached_bits        0)
      (cached_sub_word    0)
      (cached_multi_word "\000\000\000\000\000\000\000\000")
      (cached_temp_multi_word "\000\000\000\000\000\000\000\000")
      (non_cache_hits 0)
      (setter_index   0)))
    (t (
      (data          "\000\000\000\000\255\255\000\000")
      (length        48)
      (total_length  64)
      (width         1)
      (rounded_width 1)
      (log2_rounded_width 0)
      (cached_bits        0)
      (cached_sub_word    0)
      (cached_multi_word "\000\000\000\000\000\000\000\000")
      (cached_temp_multi_word "\000\000\000\000\000\000\000\000")
      (non_cache_hits 0)
      (setter_index   0)))
    (t (
      (data          "\170\170\170\170\000\000\000\000")
      (length        32)
      (total_length  64)
      (width         1)
      (rounded_width 1)
      (log2_rounded_width 0)
      (cached_bits        0)
      (cached_sub_word    0)
      (cached_multi_word "\000\000\000\000\000\000\000\000")
      (cached_temp_multi_word "\000\000\000\000\000\000\000\000")
      (non_cache_hits 0)
      (setter_index   0)))
    (t (
      (data "\170\170\170\170\170\170\170\170\002\000\000\000\000\000\000\000")
      (length             66)
      (total_length       128)
      (width              1)
      (rounded_width      1)
      (log2_rounded_width 0)
      (cached_bits        0)
      (cached_sub_word    0)
      (cached_multi_word "\000\000\000\000\000\000\000\000")
      (cached_temp_multi_word "\000\000\000\000\000\000\000\000")
      (non_cache_hits 0)
      (setter_index   0)))
    |}]
;;

let%expect_test "2 bits" =
  let t = Data.create 2 in
  Data.set t 1 Bits.(of_int ~width:2 3);
  print_s [%message (t : Data.t)];
  [%expect
    {|
    (t (
      (data          "\012\000\000\000\000\000\000\000")
      (length        2)
      (total_length  32)
      (width         2)
      (rounded_width 2)
      (log2_rounded_width 1)
      (cached_bits        00)
      (cached_sub_word    0)
      (cached_multi_word "\000\000\000\000\000\000\000\000")
      (cached_temp_multi_word "\000\000\000\000\000\000\000\000")
      (non_cache_hits 0)
      (setter_index   1)))
    |}];
  Data.set t 1 Bits.(of_int ~width:2 0);
  print_s [%message (t : Data.t)];
  [%expect
    {|
    (t (
      (data          "\000\000\000\000\000\000\000\000")
      (length        2)
      (total_length  32)
      (width         2)
      (rounded_width 2)
      (log2_rounded_width 1)
      (cached_bits        00)
      (cached_sub_word    0)
      (cached_multi_word "\000\000\000\000\000\000\000\000")
      (cached_temp_multi_word "\000\000\000\000\000\000\000\000")
      (non_cache_hits 0)
      (setter_index   1)))
    |}];
  for i = 0 to 3 do
    Data.set t i Bits.(of_int ~width:2 3)
  done;
  print_s [%message (t : Data.t)];
  [%expect
    {|
    (t (
      (data          "\255\000\000\000\000\000\000\000")
      (length        4)
      (total_length  32)
      (width         2)
      (rounded_width 2)
      (log2_rounded_width 1)
      (cached_bits        00)
      (cached_sub_word    0)
      (cached_multi_word "\000\000\000\000\000\000\000\000")
      (cached_temp_multi_word "\000\000\000\000\000\000\000\000")
      (non_cache_hits 0)
      (setter_index   1)))
    |}]
;;

let%expect_test "16 bits" =
  let t = Data.create 16 in
  for i = 0 to 5 do
    Data.set t i Bits.(of_int ~width:16 0xFFFF);
    print_s [%message (t : Data.t)]
  done;
  [%expect
    {|
    (t (
      (data          "\255\255\000\000\000\000\000\000")
      (length        1)
      (total_length  4)
      (width         16)
      (rounded_width 16)
      (log2_rounded_width 4)
      (cached_bits        0000000000000000)
      (cached_sub_word    0)
      (cached_multi_word "\000\000\000\000\000\000\000\000")
      (cached_temp_multi_word "\000\000\000\000\000\000\000\000")
      (non_cache_hits 0)
      (setter_index   4)))
    (t (
      (data          "\255\255\255\255\000\000\000\000")
      (length        2)
      (total_length  4)
      (width         16)
      (rounded_width 16)
      (log2_rounded_width 4)
      (cached_bits        0000000000000000)
      (cached_sub_word    0)
      (cached_multi_word "\000\000\000\000\000\000\000\000")
      (cached_temp_multi_word "\000\000\000\000\000\000\000\000")
      (non_cache_hits 0)
      (setter_index   4)))
    (t (
      (data          "\255\255\255\255\255\255\000\000")
      (length        3)
      (total_length  4)
      (width         16)
      (rounded_width 16)
      (log2_rounded_width 4)
      (cached_bits        0000000000000000)
      (cached_sub_word    0)
      (cached_multi_word "\000\000\000\000\000\000\000\000")
      (cached_temp_multi_word "\000\000\000\000\000\000\000\000")
      (non_cache_hits 0)
      (setter_index   4)))
    (t (
      (data          "\255\255\255\255\255\255\255\255")
      (length        4)
      (total_length  4)
      (width         16)
      (rounded_width 16)
      (log2_rounded_width 4)
      (cached_bits        0000000000000000)
      (cached_sub_word    0)
      (cached_multi_word "\000\000\000\000\000\000\000\000")
      (cached_temp_multi_word "\000\000\000\000\000\000\000\000")
      (non_cache_hits 0)
      (setter_index   4)))
    (t (
      (data "\255\255\255\255\255\255\255\255\255\255\000\000\000\000\000\000")
      (length             5)
      (total_length       8)
      (width              16)
      (rounded_width      16)
      (log2_rounded_width 4)
      (cached_bits        0000000000000000)
      (cached_sub_word    0)
      (cached_multi_word "\000\000\000\000\000\000\000\000")
      (cached_temp_multi_word "\000\000\000\000\000\000\000\000")
      (non_cache_hits 0)
      (setter_index   4)))
    (t (
      (data "\255\255\255\255\255\255\255\255\255\255\255\255\000\000\000\000")
      (length             6)
      (total_length       8)
      (width              16)
      (rounded_width      16)
      (log2_rounded_width 4)
      (cached_bits        0000000000000000)
      (cached_sub_word    0)
      (cached_multi_word "\000\000\000\000\000\000\000\000")
      (cached_temp_multi_word "\000\000\000\000\000\000\000\000")
      (non_cache_hits 0)
      (setter_index   4)))
    |}]
;;

let%expect_test "64 bits" =
  let t = Data.create 64 in
  for i = 0 to 2 do
    Data.set t i Bits.(of_int64 ~width:64 0xFFFF_FFFF_FFFF_FFFFL);
    print_s [%message (t : Data.t)]
  done;
  [%expect
    {|
    (t (
      (data          "\255\255\255\255\255\255\255\255")
      (length        1)
      (total_length  1)
      (width         64)
      (rounded_width 64)
      (log2_rounded_width 6)
      (cached_bits
       0000000000000000000000000000000000000000000000000000000000000000)
      (cached_sub_word 0)
      (cached_multi_word "\000\000\000\000\000\000\000\000")
      (cached_temp_multi_word "\000\000\000\000\000\000\000\000")
      (non_cache_hits 0)
      (setter_index   6)))
    (t (
      (data "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255")
      (length             2)
      (total_length       2)
      (width              64)
      (rounded_width      64)
      (log2_rounded_width 6)
      (cached_bits
       0000000000000000000000000000000000000000000000000000000000000000)
      (cached_sub_word 0)
      (cached_multi_word "\000\000\000\000\000\000\000\000")
      (cached_temp_multi_word "\000\000\000\000\000\000\000\000")
      (non_cache_hits 0)
      (setter_index   6)))
    (t (
      (data
       "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\000\000\000\000\000\000")
      (length             3)
      (total_length       4)
      (width              64)
      (rounded_width      64)
      (log2_rounded_width 6)
      (cached_bits
       0000000000000000000000000000000000000000000000000000000000000000)
      (cached_sub_word 0)
      (cached_multi_word "\000\000\000\000\000\000\000\000")
      (cached_temp_multi_word "\000\000\000\000\000\000\000\000")
      (non_cache_hits 0)
      (setter_index   6)))
    |}]
;;

let%expect_test "65 bits" =
  let t = Data.create 65 in
  for i = 0 to 2 do
    Data.set t i Bits.(ones 65);
    print_s [%message (t : Data.t)]
  done;
  [%expect
    {|
    (t (
      (data "\255\255\255\255\255\255\255\255\001\000\000\000\000\000\000\000")
      (length             1)
      (total_length       1)
      (width              65)
      (rounded_width      128)
      (log2_rounded_width 7)
      (cached_bits
       00000000000000000000000000000000000000000000000000000000000000000)
      (cached_sub_word 0)
      (cached_multi_word
       "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000")
      (cached_temp_multi_word
       "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000")
      (non_cache_hits 0)
      (setter_index   7)))
    (t (
      (data
       "\255\255\255\255\255\255\255\255\001\000\000\000\000\000\000\000\255\255\255\255\255\255\255\255\001\000\000\000\000\000\000\000")
      (length             2)
      (total_length       2)
      (width              65)
      (rounded_width      128)
      (log2_rounded_width 7)
      (cached_bits
       00000000000000000000000000000000000000000000000000000000000000000)
      (cached_sub_word 0)
      (cached_multi_word
       "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000")
      (cached_temp_multi_word
       "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000")
      (non_cache_hits 0)
      (setter_index   7)))
    (t (
      (data
       "\255\255\255\255\255\255\255\255\001\000\000\000\000\000\000\000\255\255\255\255\255\255\255\255\001\000\000\000\000\000\000\000\255\255\255\255\255\255\255\255\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000")
      (length             3)
      (total_length       4)
      (width              65)
      (rounded_width      128)
      (log2_rounded_width 7)
      (cached_bits
       00000000000000000000000000000000000000000000000000000000000000000)
      (cached_sub_word 0)
      (cached_multi_word
       "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000")
      (cached_temp_multi_word
       "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000")
      (non_cache_hits 0)
      (setter_index   7)))
    |}]
;;

(* This is the old (simple) representation of waveform data used in waveterm.

   We replicate in this test so we can compare it to the new packed representation.
*)
module Unpacked_data = struct
  type t =
    { mutable data : Bits.t array
    ; mutable length : int
    ; width : int
    }
  [@@deriving sexp_of, compare, equal]

  let[@cold] raise_index_out_of_bounds index length =
    raise_s [%message "Index into waveform is out of bounds" (index : int) (length : int)]
  ;;

  let get d index =
    if index < d.length then d.data.(index) else raise_index_out_of_bounds index d.length
  ;;

  let create width = { data = [||]; length = 0; width }
  let _init n ~width ~f = { data = Array.init n ~f; length = n; width }

  let resize d =
    let old_data = d.data in
    let new_len = max 1 (Array.length d.data * 2) in
    d.data
      <- Array.init new_len ~f:(fun i ->
           try old_data.(i) with
           | _ -> Bits.gnd)
  ;;

  let[@cold] raise_invalid_width actual_width expected_width =
    raise_s
      [%message
        "Unexpected [Bits.t] width in waveform"
          (actual_width : int)
          (expected_width : int)]
  ;;

  let rec set d n v =
    if n < Array.length d.data
    then
      if Bits.width v <> d.width
      then raise_invalid_width (Bits.width v : int) (d.width : int)
      else (
        d.data.(n) <- v;
        d.length <- max d.length (n + 1))
    else (
      resize d;
      set d n v)
  ;;
end

let%expect_test "random tests for [get] and [set]" =
  let max_bits = 340 in
  let length = 4 * 1024 in
  require_does_not_raise [%here] (fun () ->
    for bits = 1 to max_bits do
      let packed = Data.create bits in
      let unpacked = Unpacked_data.create bits in
      (* Set it once so it contains random data - this is so we can test the edit in place
         behaviour of the [set] function *)
      for index = 0 to length - 1 do
        let bits = Bits.random ~width:bits in
        Data.set packed index bits;
        Unpacked_data.set unpacked index bits
      done;
      (* Set it to the values we want to read *)
      for index = 0 to length - 1 do
        let bits = Bits.random ~width:bits in
        Data.set packed index bits;
        Unpacked_data.set unpacked index bits
      done;
      (* Read back all the values and assert they are correct (or rather match the old
         implementation). *)
      for index = 0 to length - 1 do
        let bits_packed = Data.get packed index in
        let bits_unpacked = Unpacked_data.get unpacked index in
        if not (Bits.equal bits_packed bits_unpacked)
        then raise_s [%message "data misamtch" (bits : int) (index : int)]
      done
    done);
  [%expect {| |}]
;;
