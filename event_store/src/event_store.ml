open Base

module type Data = Event_store_intf.Data
module type Time = Event_store_intf.Time

module M = Event_store_intf.M

module Make (Time : Time) (Data : Data) = struct
  (* Data sorted in time order. *)
  type t =
    { mutable data : Data.t array
    ; mutable time : Time.t array
    ; mutable length : int
    }
  [@@deriving sexp_of]

  let initial_size = 1

  let create () =
    { data = Array.create ~len:initial_size Data.none
    ; time = Array.create ~len:initial_size Time.zero
    ; length = 0
    }
  ;;

  (* Basic getter's and setter's *)

  let set t index time data =
    (* assume the array is large enough *)
    t.data.(index) <- data;
    t.time.(index) <- time;
    (* increase length if required *)
    t.length <- max t.length (index + 1)
  ;;

  let get_time_at_index t index = t.time.(index)
  let get_data_at_index t index = t.data.(index)
  let length t = t.length
  let capacity t = Array.length t.data

  let resize t =
    (* Double size and copy old data across *)
    let capacity = capacity t * 2 in
    let data = Array.create ~len:capacity Data.none in
    let time = Array.create ~len:capacity Time.zero in
    Array.blito ~src:t.data ~dst:data ~src_len:(length t) ();
    Array.blito ~src:t.time ~dst:time ~src_len:(length t) ();
    t.data <- data;
    t.time <- time
  ;;

  let find_insertion_index t time =
    Array.binary_search
      ~len:(length t)
      t.time
      `Last_less_than_or_equal_to
      ~compare:Time.compare
      time
  ;;

  let shuffle_up t index =
    for i = length t downto index + 1 do
      set t i (get_time_at_index t (i - 1)) (get_data_at_index t (i - 1))
    done
  ;;

  let insert t time data =
    (* Ensure we have space for a shuffle operation. *)
    if length t = capacity t then resize t;
    if length t = 0
    then (* This becomes our first element at index 0 *)
      set t 0 time data
    else (
      match find_insertion_index t time with
      | None ->
        (* Insert at 0, and shuffle everything up *)
        shuffle_up t 0;
        set t 0 time data
      | Some index ->
        (* 2 cases: time in array equals time of insert -> merge
           time in array is less than time of insert -> shuffle *)
        (match Time.compare (get_time_at_index t index) time with
         | 0 -> set t index time (Data.merge (get_data_at_index t index) data)
         | -1 ->
           shuffle_up t (index + 1);
           set t (index + 1) time data
         | _ -> raise_s [%message "[insert] unhandled case"]))
  ;;

  let get t time =
    match find_insertion_index t time with
    | None -> raise_s [%message "[Event_store.get] Invalid time" (time : Time.t)]
    | Some index -> get_data_at_index t index
  ;;
end
