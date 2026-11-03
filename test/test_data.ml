open! Import

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

let clock =
  { Hardcaml.Wave_data.Wave.name = "clock"
  ; width = 1
  ; typ = Internal
  ; wave_format = Bit
  ; wave_data = random_bits ~length:0 ~width:1
  }
;;

let random_data i ~prefix ~length ~max_bits =
  if i % 2 = 0
  then
    { Hardcaml.Wave_data.Wave.name = prefix ^ random_string ~max_length:20
    ; width = 1
    ; typ = Internal
    ; wave_format = Bit
    ; wave_data = random_bits ~length ~width:1
    }
  else (
    let wave_format = random_format () in
    let width = Random.int max_bits + 1 in
    { Hardcaml.Wave_data.Wave.name = prefix ^ random_string ~max_length:20
    ; width
    ; typ = Internal
    ; wave_format
    ; wave_data = random_bits ~length ~width
    })
;;

let create ~prefix ~length ~num_signals ~max_bits =
  clock
  :: List.init num_signals ~f:(fun i ->
    random_data i ~prefix:(prefix i) ~length ~max_bits)
  |> Array.of_list
;;
