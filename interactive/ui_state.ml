open Base
open Hardcaml_waveterm_kernel
module M = Ui_state_intf.M

module Make
    (Data : Expert.Data.S)
    (M : Expert.M(Data).S)
    (Hierarchy : Hierarchy.M(Data)(M).S) =
struct
  open M

  module Module = struct
    type t = { expanded : bool } [@@deriving sexp]
  end

  type t =
    { cfg : Waves.Config.t
    ; modules : Module.t Map.M(String).t
    ; cursors : Cursors.t
    }
  [@@deriving sexp]

  let of_hierarchy (h : Hierarchy.t) =
    let table = Hashtbl.create (module String) in
    Hierarchy.iter_nodes h ~f:(fun ~depth:_ ~module_name node ->
      Hashtbl.add_exn table ~key:module_name ~data:{ Module.expanded = node.visible });
    { cfg = h.cfg
    ; cursors = h.cursors
    ; modules = Hashtbl.to_alist table |> Map.of_alist_exn (module String)
    }
  ;;

  let apply_to { cfg; cursors; modules } (hierarchy : Hierarchy.t) =
    hierarchy.cfg <- cfg;
    hierarchy.cursors <- cursors;
    Hierarchy.iter_nodes hierarchy ~f:(fun ~depth:_ ~module_name node ->
      Map.find modules module_name
      |> Option.iter ~f:(fun { expanded } -> node.visible <- expanded));
    Hierarchy.set_currently_rendered hierarchy;
    ()
  ;;
end
