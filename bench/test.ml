open Core
module Bits = Hardcaml.Bits
module Data = Hardcaml_waveterm_cyclesim.Data

let size = 1_000

(* Test [Data.set] *)
(* let setup width =
 *   let data = Data.create width in
 *   let bits = Array.init size ~f:(fun _ -> Bits.random ~width) in
 *   (* Force allocation of the array - focus on the cost of the set itself *)
 *   Data.set data (size - 1) bits.(0);
 *   data, bits
 * ;;
 *
 * let loop (data, bits) =
 *   for idx = 0 to size - 1 do
 *     Data.set data idx bits.(idx)
 *   done
 * ;; *)

(* Test [Data.set_mutable_unsafe] *)
let setup width =
  let data = Data.create width in
  let bits = Array.init size ~f:(fun _ -> Bits.Mutable.create width) in
  (* Force allocation of the array - focus on the cost of the set itself *)
  Data.set_mutable_unsafe data (size - 1) bits.(0);
  data, bits
;;

let loop (data, bits) =
  for idx = 0 to size - 1 do
    Data.set_mutable_unsafe data idx bits.(idx)
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
