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

let default = Default

let port_name_is_one_of ?(alignment = Text_alignment.Left) ?wave_format names =
  Names { names = List.map names ~f:Port_name.of_string; wave_format; alignment }
;;

let port_name_is ?alignment ?wave_format name =
  port_name_is_one_of [ name ] ?wave_format ?alignment
;;

let port_name_matches ?(alignment = Text_alignment.Left) ?wave_format re =
  Regexp { re; wave_format; alignment }
;;

let custom ~f = Custom f
let custom_with_alignment ~f = Custom_with_alignment f

module type States = sig
  type t [@@deriving sexp_of, enumerate]
end

let states_binary ?alignment (module States : States) name =
  let states =
    List.map States.all ~f:(fun state -> Sexp.to_string_hum (States.sexp_of_t state))
  in
  port_name_is ?alignment name ~wave_format:(Index states)
;;

let states_onehot ?alignment (module States : States) name =
  let states =
    Array.of_list_map States.all ~f:(fun state ->
      Sexp.to_string_hum (States.sexp_of_t state))
  in
  let open Hardcaml in
  port_name_is
    ?alignment
    name
    ~wave_format:
      (Custom (fun b -> Bits.onehot_to_binary b |> Bits.to_int |> Array.get states))
;;

let states ?(onehot = false) ?alignment (module States : States) name =
  if onehot
  then states_onehot ?alignment (module States) name
  else states_binary ?alignment (module States) name
;;
