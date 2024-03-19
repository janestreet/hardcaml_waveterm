open Core
open Hardcaml
module Time = Int

module Data = struct
  include Bits

  let none = empty
  let merge _ b = b
end

module Events = Hardcaml_waveterm_event_store.Event_store.Make (Time) (Data)

module Waveterm = Hardcaml_waveterm_kernel.Expert.Make (struct
  include Events

  let equal _ _ = false
  let width t = get t 0 |> Bits.width
  let get_digestible_string _ = Bytes.of_string "", 0
end)

let create_wave d =
  let t = Events.create () in
  List.iter d ~f:(fun (time, data) -> Events.insert t time data);
  t
;;

let waves =
  List.init 5 ~f:(fun widx ->
    List.init 10 ~f:(fun tidx ->
      let time = widx * 2 * tidx in
      [ time, Bits.gnd; time + widx, Bits.vdd ])
    |> List.concat)
  |> List.map ~f:create_wave
;;

let%expect_test "" =
  let waves =
    List.mapi waves ~f:(fun idx w -> Waveterm.Wave.Binary ("w" ^ Int.to_string idx, w))
  in
  Waveterm.Waveform.print
    ~wave_width:(-1)
    (Waveterm.Waveform.create_from_data
       ~waves
       ~ports:
         (List.init 5 ~f:(fun idx ->
            { Hardcaml_waveterm_kernel.Port.type_ = Input
            ; port_name =
                Hardcaml_waveterm_kernel.Port_name.of_string ("w" ^ Int.to_string idx)
            ; width = 1
            })));
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │w0             ││─                                                  │
    │               ││                                                   │
    │w1             ││ ┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌                               │
    │               ││─┘└┘└┘└┘└┘└┘└┘└┘└┘└┘                               │
    │w2             ││  ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─                               │
    │               ││──┘ └─┘ └─┘ └─┘ └─┘                                │
    │w3             ││   ┌──┐  ┌──┐  ┌──┐                                │
    │               ││───┘  └──┘  └──┘  └─                               │
    │w4             ││    ┌───┐   ┌───┐                                  │
    │               ││────┘   └───┘   └───                               │
    │               ││                                                   │
    │               ││                                                   │
    │               ││                                                   │
    │               ││                                                   │
    │               ││                                                   │
    │               ││                                                   │
    │               ││                                                   │
    │               ││                                                   │
    └───────────────┘└───────────────────────────────────────────────────┘
    |}]
;;
