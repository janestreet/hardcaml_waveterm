(** A [Rule.t] is a predicate on [Port.t]s that specifies the display format of matching
    ports. *)

open Base

type t =
  | Default
  | Regexp of
      { re : Re.re
      ; wave_format : Hardcaml.Wave_format.t option
      ; alignment : Text_alignment.t
      }
  | Names of
      { names : Port_name.t list
      ; wave_format : Hardcaml.Wave_format.t option
      ; alignment : Text_alignment.t
      }
  | Custom of (Port.t -> Hardcaml.Wave_format.t option)
  | Custom_with_alignment of
      (Port.t -> (Hardcaml.Wave_format.t * Text_alignment.t) option)
[@@deriving sexp_of]

(** Default formatting - binary for 1 bit signals, hex otherwise. *)
val default : t

(** Use given [format] for ports whose name match the regular expression [re]. *)
val port_name_matches
  :  ?alignment:Text_alignment.t
  -> ?wave_format:Hardcaml.Wave_format.t
  -> Re.re
  -> t

(** Use [format] for ports with given name. *)
val port_name_is
  :  ?alignment:Text_alignment.t
  -> ?wave_format:Hardcaml.Wave_format.t
  -> string
  -> t

(** Match any one of a list of names. *)
val port_name_is_one_of
  :  ?alignment:Text_alignment.t
  -> ?wave_format:Hardcaml.Wave_format.t
  -> string list
  -> t

(** In [custom f], [f] returns [None] to signify no match, or [Some format] to specify a
    display format. *)
val custom : f:(Port.t -> Hardcaml.Wave_format.t option) -> t

(** Similar tp [f], but allows the user to specify the alignment of the wave. *)
val custom_with_alignment
  :  f:(Port.t -> (Hardcaml.Wave_format.t * Text_alignment.t) option)
  -> t

module type States = sig
  type t [@@deriving sexp_of, enumerate]
end

(** For use with statemachines. Derives state names from the variant describing the states
    of the state machine. *)
val states
  :  ?onehot:bool
       (** Default is [false]. State is onehot encoded. Otherwise binary encoded.
      (Note; the uncommon grey coded case is not supported). *)
  -> ?alignment:Text_alignment.t
  -> (module States)
  -> string
  -> t
