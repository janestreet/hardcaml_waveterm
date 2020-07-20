open! Import

type t =
  | Default
  | Regexp of
      { re : (Re.re[@sexp.opaque])
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

let default = Default

let port_name_is_one_of ?(alignment = Wave_format.Left) ~wave_format names =
  Names { names = List.map names ~f:Port_name.of_string; wave_format; alignment }
;;

let port_name_is ?alignment name ~wave_format =
  port_name_is_one_of [ name ] ~wave_format ?alignment
;;

let port_name_matches re ~wave_format =
  Regexp { re; wave_format; alignment = Wave_format.Left }
;;

let custom ~f = Custom f
let custom_with_alignment ~f = Custom_with_alignment f
