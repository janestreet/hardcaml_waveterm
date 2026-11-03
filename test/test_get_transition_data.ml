open! Core
open! Import
module Four_state_data = Hardcaml.Wave_data_in_mutable_events_four_state
module Four_state_render = Hardcaml_waveterm_kernel.Render.Make (Four_state_data)

let data_of_list bits =
  let len = List.length bits in
  let arr = Array.of_list bits in
  Data.init len ~width:1 ~f:(fun i -> arr.(i))
;;

let run_tests tests =
  let rows =
    List.map tests ~f:(fun (prev_bits, data) ->
      let data = data_of_list data in
      let time_steps_per_char = Data.length data in
      let prev_data =
        Option.map prev_bits ~f:(fun bits -> bits, Bits.zero (Bits.width bits))
      in
      let { Render.To_render.at_most_one_transition
          ; first
          ; last
          ; first_mask = _
          ; last_mask = _
          }
        =
        Render.For_testing.get_transition_data
          ~prev_data
          ~data
          ~time_steps_per_char
          ~off:0
      in
      let prev =
        match prev_bits with
        | None -> ""
        | Some bits -> Bits.to_string bits
      in
      let data =
        List.init time_steps_per_char ~f:(fun i -> Data.get data i |> Bits.to_string)
        |> String.concat ~sep:" "
      in
      [ prev
      ; data
      ; Bits.to_string first
      ; Bits.to_string last
      ; Bool.to_string (not at_most_one_transition)
      ])
  in
  Ascii_table.simple_list_table
    [ "Prev"; "Data"; "First"; "Last"; "Multi transition" ]
    rows
;;

let%expect_test "test" =
  run_tests
    (List.concat
       [ List.init 8 ~f:(fun i -> None, Bits.(bits_lsb (of_unsigned_int ~width:3 i)))
       ; List.init 8 ~f:(fun i ->
           Some Bits.gnd, Bits.(bits_lsb (of_unsigned_int ~width:3 i)))
       ; List.init 8 ~f:(fun i ->
           Some Bits.vdd, Bits.(bits_lsb (of_unsigned_int ~width:3 i)))
       ]);
  [%expect
    {|
    ┌──────┬───────┬───────┬──────┬──────────────────┐
    │ Prev │  Data │ First │ Last │ Multi transition │
    ├──────┼───────┼───────┼──────┼──────────────────┤
    │      │ 0 0 0 │     0 │    0 │            false │
    │      │ 1 0 0 │     1 │    0 │            false │
    │      │ 0 1 0 │     0 │    0 │             true │
    │      │ 1 1 0 │     1 │    0 │            false │
    │      │ 0 0 1 │     0 │    1 │            false │
    │      │ 1 0 1 │     1 │    1 │             true │
    │      │ 0 1 1 │     0 │    1 │            false │
    │      │ 1 1 1 │     1 │    1 │            false │
    │    0 │ 0 0 0 │     0 │    0 │            false │
    │    0 │ 1 0 0 │     1 │    0 │             true │
    │    0 │ 0 1 0 │     0 │    0 │             true │
    │    0 │ 1 1 0 │     1 │    0 │             true │
    │    0 │ 0 0 1 │     0 │    1 │            false │
    │    0 │ 1 0 1 │     1 │    1 │             true │
    │    0 │ 0 1 1 │     0 │    1 │            false │
    │    0 │ 1 1 1 │     1 │    1 │            false │
    │    1 │ 0 0 0 │     0 │    0 │            false │
    │    1 │ 1 0 0 │     1 │    0 │            false │
    │    1 │ 0 1 0 │     0 │    0 │             true │
    │    1 │ 1 1 0 │     1 │    0 │            false │
    │    1 │ 0 0 1 │     0 │    1 │             true │
    │    1 │ 1 0 1 │     1 │    1 │             true │
    │    1 │ 0 1 1 │     0 │    1 │             true │
    │    1 │ 1 1 1 │     1 │    1 │            false │
    └──────┴───────┴───────┴──────┴──────────────────┘
    |}]
;;

let four_state_data_of_list values =
  let max_time = ref (List.length values - 1) in
  let data = Four_state_data.create 1 max_time in
  List.iteri values ~f:(fun time string ->
    let value = Four_state_logic.of_string string in
    Four_state_data.insert
      data
      time
      ~bits:(Four_state_logic.bits value)
      ~mask:(Four_state_logic.mask value));
  data
;;

let four_state_string bits mask =
  Four_state_logic.create_exn ~bits ~mask |> Four_state_logic.to_string
;;

let%expect_test "four-state transitions" =
  let rows =
    List.map
      [ None, [ "0"; "Z"; "Z" ]
      ; None, [ "0"; "Z"; "0" ]
      ; None, [ "X"; "Z"; "Z" ]
      ; Some "0", [ "Z"; "Z"; "0" ]
      ]
      ~f:(fun (previous, values) ->
        let data = four_state_data_of_list values in
        let prev_data =
          Option.map previous ~f:(fun string ->
            let value = Four_state_logic.of_string string in
            Four_state_logic.bits value, Four_state_logic.mask value)
        in
        let { Four_state_render.To_render.at_most_one_transition
            ; first
            ; first_mask
            ; last
            ; last_mask
            }
          =
          Four_state_render.For_testing.get_transition_data
            ~prev_data
            ~data
            ~time_steps_per_char:(List.length values)
            ~off:0
        in
        [ Option.value previous ~default:""
        ; String.concat ~sep:" " values
        ; four_state_string first first_mask
        ; four_state_string last last_mask
        ; Bool.to_string (not at_most_one_transition)
        ])
  in
  Ascii_table.simple_list_table
    [ "Prev"; "Data"; "First"; "Last"; "Multi transition" ]
    rows;
  [%expect
    {|
    ┌──────┬───────┬───────┬──────┬──────────────────┐
    │ Prev │  Data │ First │ Last │ Multi transition │
    ├──────┼───────┼───────┼──────┼──────────────────┤
    │      │ 0 Z Z │     0 │    Z │            false │
    │      │ 0 Z 0 │     0 │    0 │             true │
    │      │ X Z Z │     X │    Z │            false │
    │    0 │ Z Z 0 │     Z │    0 │             true │
    └──────┴───────┴───────┴──────┴──────────────────┘
    |}]
;;
