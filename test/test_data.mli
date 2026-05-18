open! Import

val random_string : max_length:int -> string

val create
  :  prefix:(int -> string)
  -> length:int
  -> num_signals:int
  -> max_bits:int
  -> Data.t Hardcaml.Wave_data.Wave.t array
