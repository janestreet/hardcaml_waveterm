open Base

module Config = struct
  type t =
    { mutable signals_width : int
    ; mutable values_width : int
    ; mutable wave_width : int
    ; mutable wave_height : int
    ; mutable start_cycle : int
    ; mutable start_signal : int
    ; mutable selected_signal : int
    ; mutable wave_cursor : int
    ; mutable signal_scroll : int
    ; mutable value_scroll : int
    }
  [@@deriving sexp]

  let default =
    { signals_width = 20
    ; values_width = 20
    ; wave_width = 3
    ; wave_height = 1
    ; start_cycle = 0
    ; start_signal = 0
    ; selected_signal = 0
    ; wave_cursor = 0
    ; signal_scroll = 0
    ; value_scroll = 0
    }
  ;;
end

type t =
  { cfg : Config.t
  ; waves : Wave.t array
  }
[@@deriving sexp_of]

let write ch w =
  let w =
    { w with
      waves =
        Array.map
          ~f:(fun (d : Wave.t) : Wave.t ->
            (match d with
             | Empty _ | Clock _ -> d
             | Binary (n, d) ->
               Binary (n, Data.init (Data.length d) ~width:(Data.width d) ~f:(Data.get d))
             | Data (n, d, ts, alignment) ->
               let ts =
                 match ts with
                 | Custom _ -> Wave_format.Binary
                 | _ -> ts
               in
               (* cant marshal functions *)
               Data
                 ( n
                 , Data.init (Data.length d) ~width:(Data.width d) ~f:(Data.get d)
                 , ts
                 , alignment )))
          w.waves
    }
  in
  Stdlib.Marshal.to_channel ch w []
;;

let read ch : t = Stdlib.Marshal.from_channel ch
