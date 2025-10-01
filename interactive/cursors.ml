open Base

let num_cursors = 5

type t =
  { offsets : int option array
  ; mutable selected : int
  }
[@@deriving sexp]

let create () = { offsets = Array.init num_cursors ~f:(Fn.const None); selected = 0 }
let get_offset t = t.offsets.(t.selected)
let get_offset_as_string t = Option.value_map (get_offset t) ~default:"-" ~f:Int.to_string
let set_offset t offset = t.offsets.(t.selected) <- offset
let get_selected t = t.selected
let get_selected_as_string t = Int.to_string (get_selected t + 1)

let get_active_cursors t =
  (* Drawing cursors on top of each other can cancel them out. So we go through here and
     find cursors at unique locations only. We also have to deal with primary/secondary
     cursors as well. *)
  let rec f i =
    if i = num_cursors
    then []
    else (
      match t.offsets.(i) with
      | Some offset -> offset :: f (i + 1)
      | None -> f (i + 1))
  in
  let unique_cursors = f 0 |> List.dedup_and_sort ~compare:Int.compare in
  match get_offset t with
  | None -> List.map unique_cursors ~f:(fun c -> `Secondary c)
  | Some offset ->
    List.map unique_cursors ~f:(fun c -> if c = offset then `Primary c else `Secondary c)
;;

let set_selected t selected =
  if selected < 0
  then raise_s [%message "Cannot set negative cursor index" (selected : int)];
  if selected >= num_cursors
  then raise_s [%message "Cursor index out of range" (selected : int) (num_cursors : int)];
  t.selected <- selected
;;

let with_selected t ~f =
  let selected = get_selected t in
  let result = f selected in
  set_selected t selected;
  result
;;
