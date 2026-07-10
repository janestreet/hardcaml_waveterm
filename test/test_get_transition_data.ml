open! Core
open! Import

let data_of_list bits =
  let len = List.length bits in
  let arr = Array.of_list bits in
  Data.init len ~width:1 ~f:(fun i -> arr.(i))
;;

let run_tests tests =
  let rows =
    List.map tests ~f:(fun (prev_bits, data) ->
      let data = data_of_list data in
      let chars_per_cycle = Data.length data in
      let { Render.To_render.at_most_one_transition; first; last } =
        Render.For_testing.get_transition_data ~prev_bits ~data ~chars_per_cycle ~off:0
      in
      let prev =
        match prev_bits with
        | None -> ""
        | Some bits -> Bits.to_string bits
      in
      let data =
        List.init chars_per_cycle ~f:(fun i -> Data.get data i |> Bits.to_string)
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
