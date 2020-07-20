(** A [Rule.t] is a predicate on [Port.t]s that specifies the display format of matching
    ports. *)

open! Import

type t =
  | Default
  | Regexp of
      { re : Re.re
      ; wave_format : Wave_format.t
      ; alignment : Wave_format.alignment
      }
  | Names of
      { names : Port_name.t list
      ; wave_format : Wave_format.t
      ; alignment : Wave_format.alignment
      }
  | Custom of (Port.t -> Wave_format.t option)
  | Custom_with_alignment of (Port.t -> (Wave_format.t * Wave_format.alignment) option)
[@@deriving sexp_of]

(** Default formatting - binary for 1 bit signals, hex otherwise. *)
val default : t

(** Use given [format] for ports whose name match the regular expression [re]. *)
val port_name_matches : Re.re -> wave_format:Wave_format.t -> t

(** Use [format] for ports with given name. *)
val port_name_is
  :  ?alignment:Wave_format.alignment
  -> string
  -> wave_format:Wave_format.t
  -> t

(** Match any one of a list of names. *)
val port_name_is_one_of
  :  ?alignment:Wave_format.alignment
  -> wave_format:Wave_format.t
  -> string list
  -> t

(** In [custom f], [f] returns [None] to signify no match, or [Some format] to specify a
    display format. *)
val custom : f:(Port.t -> Wave_format.t option) -> t

(** Similar tp [f], but allows the user to specify the alignment of the wave. *)
val custom_with_alignment
  :  f:(Port.t -> (Wave_format.t * Wave_format.alignment) option)
  -> t
