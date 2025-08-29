open Core
module Draw_notty = Draw_notty
module Scroll = Scroll
module Widget = Widget
include Widget.Make (Hardcaml_waveterm_cyclesim.Data) (Hardcaml_waveterm_cyclesim)

module Expert = struct
  module Key_actions = Key_actions
end

let print_key_help () =
  Key_actions.zip Key_actions.key_help Key_actions.default_keys
  |> Key_actions.to_list
  |> List.iter ~f:(fun ((_group, descr), key) ->
    print_endline [%string "%{key#Key_actions.Key:20}    %{descr}"])
;;
