open Base

type t =
  | Default
  | Regexp of
      { re : (Re.re[@sexp.opaque])
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

module Regexp = struct
  type t =
    | Glob of string
    | Posix of string
    | Perl of string
    | Re of (Re.re[@sexp.opaque])
  [@@deriving sexp_of]

  let compile re =
    match re with
    | Glob re -> Re.Glob.glob re |> Re.compile
    | Posix re -> Re.Posix.re re |> Re.compile
    | Perl re -> Re.Perl.re re |> Re.compile
    | Re re -> re
  ;;
end

let default = Default

let port_name_is_one_of ?(alignment = Text_alignment.Left) ?wave_format names =
  Names { names = List.map names ~f:Port_name.of_string; wave_format; alignment }
;;

let port_name_is ?alignment ?wave_format name =
  port_name_is_one_of [ name ] ?wave_format ?alignment
;;

let port_name_matches ?(alignment = Text_alignment.Left) ?wave_format re =
  Regexp { re = Regexp.compile re; wave_format; alignment }
;;

let custom ~f = Custom f
let custom_with_alignment ~f = Custom_with_alignment f

module type States = sig
  type t [@@deriving sexp_of, enumerate]
end
