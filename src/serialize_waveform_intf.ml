open Base

module type S = sig
  (** Rewrite wave formats that cannot survive serialization - [Custom _] carries a
      closure - to [Bit_or Hex]. *)
  val sanitize
    :  'wave_data Hardcaml.Wave_data.Wave.t array
    -> 'wave_data Hardcaml.Wave_data.Wave.t array

  (** [marshall t filename] writes [t] to [filename]. A [.vcd] suffix selects
      [marshall_vcd]; any other suffix uses the native waveform format. Both support
      2-state and 4-state waveforms. *)
  val marshall : Hardcaml.Wave_data.t -> string -> unit

  val marshall_here : here:[%call_pos] -> Hardcaml.Wave_data.t -> unit

  (** [unmarshall filename] reads a waveform back. A [.vcd] suffix always yields a
      [By_mutable_event_four_state] waveform, because VCD is a 4-state format (see
      [Vcd.read_mutable_event_based_four_state]); use [Wave_data.to_two_state] if you need
      a 2-state waveform. The native format preserves whichever constructor was written. *)
  val unmarshall : string -> Hardcaml.Wave_data.t

  (** Write a VCD file directly from the event-based waveform data. 2-state and 4-state
      waveforms are both supported. Note that a VCD round trip does not preserve the
      [Wave_data.t] constructor: [unmarshall] of a [.vcd] always returns a 4-state
      waveform. *)
  val marshall_vcd : Hardcaml.Wave_data.t -> string -> unit

  (** [to_string t] returns exactly the bytes that [marshall] writes for a native (non
      [.vcd]) waveform file: the magic header identifying [t]'s wave-data format, followed
      by the [Wave_data.For_file_serialization] bin_io payload. Both constructors are
      supported, and each gets its own header.

      This is the same codec as [marshall], not a parallel one, so anything encoded with
      [to_string] can be written to a file and read back with [unmarshall], and vice
      versa. Use it to move a waveform over a wire, or anywhere else a file is not wanted. *)
  val to_string : Hardcaml.Wave_data.t -> string

  (** [of_string data] decodes bytes produced by [to_string] (equivalently, the contents
      of a native waveform file). Both magic headers are recognized, and the constructor
      is whichever one the bytes were written with, so this is an exact inverse of
      [to_string].

      Raises if the bytes do not start with a recognized magic header, or if the payload
      that follows is malformed. VCD is not accepted: it is a textual format with no magic
      header, and reading it needs a file (see [unmarshall]). *)
  val of_string : string -> Hardcaml.Wave_data.t
end

module type Serialize_waveform = sig
  module type S = S

  include S
end
