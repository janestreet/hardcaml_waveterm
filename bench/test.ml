open Core
module Bits = Hardcaml.Bits
module Data = Hardcaml.Wave_data_in_mutable_events

let size = 1_000

(* Test [Data.set_mutable_unsafe] *)
let setup width =
  let data = Data.create width (ref (size - 1)) in
  (* [set_mutable_unsafe] writes unchecked and never resizes: grow the store to hold
     [size] events up front, so the loop measures the cost of the set itself. *)
  while Data.capacity data < size do
    Data.resize data
  done;
  let bits = Array.init size ~f:(fun _ -> Bits.Mutable.create width) in
  (* Touch the last slot so first-write effects are off the measured path. *)
  Data.set_mutable_unsafe data (size - 1) ~time:(size - 1) bits.(0);
  data, bits
;;

let loop (data, bits) =
  for idx = 0 to size - 1 do
    Data.set_mutable_unsafe data idx ~time:idx bits.(idx)
  done
;;

let%bench_fun "1" =
  let t = setup 1 in
  fun () -> loop t
;;

let%bench_fun "2" =
  let t = setup 2 in
  fun () -> loop t
;;

let%bench_fun "4" =
  let t = setup 4 in
  fun () -> loop t
;;

let%bench_fun "8" =
  let t = setup 8 in
  fun () -> loop t
;;

let%bench_fun "16" =
  let t = setup 16 in
  fun () -> loop t
;;

let%bench_fun "32" =
  let t = setup 32 in
  fun () -> loop t
;;

let%bench_fun "64" =
  let t = setup 64 in
  fun () -> loop t
;;

let%bench_fun "128" =
  let t = setup 128 in
  fun () -> loop t
;;

let%bench_fun "192" =
  let t = setup 192 in
  fun () -> loop t
;;

let%bench_fun "256" =
  let t = setup 256 in
  fun () -> loop t
;;

let%bench_fun "320" =
  let t = setup 320 in
  fun () -> loop t
;;
