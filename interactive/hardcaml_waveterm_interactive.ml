open Core
module Draw_notty = Draw_notty
module Scroll = Scroll
module Widget = Widget

module Expert = struct
  module Key_actions = Key_actions
end

let run = Widget.run
let run_async = Widget.run_async

let print_key_help () =
  Key_actions.zip Key_actions.key_help Key_actions.default_keys
  |> Key_actions.to_list
  |> List.iter ~f:(fun ((_group, descr), key) ->
    print_endline [%string "%{key#Key_actions.Key:20}    %{descr}"])
;;
