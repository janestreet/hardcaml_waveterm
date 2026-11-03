open! Import

module Counting_data = struct
  type t =
    { values : int array
    ; data_reads : int ref
    ; mask_reads : int ref
    ; time_reads : int ref
    ; searches : int ref
    }
  [@@deriving sexp_of]

  let equal a b = phys_equal a b
  let width _ = 1
  let length t = Array.length t.values
  let num_events t = Array.length t.values

  let find_insertion_index t time =
    Int.incr t.searches;
    if time < 0 then None else Some (Int.min time (num_events t - 1))
  ;;

  let get_time_at_index t index =
    Int.incr t.time_reads;
    index
  ;;

  let get_data_at_index t index =
    Int.incr t.data_reads;
    Bits.of_bool (Int.equal t.values.(index) 1)
  ;;

  let is_four_state _ = false

  let get_mask_at_index t _ =
    Int.incr t.mask_reads;
    Bits.gnd
  ;;

  let get t time =
    match find_insertion_index t time with
    | None -> Bits.gnd
    | Some index -> get_data_at_index t index
  ;;

  let get_digestible_string _ = Bytes.create 0, 0

  let create values =
    { values = Array.of_list values
    ; data_reads = ref 0
    ; mask_reads = ref 0
    ; time_reads = ref 0
    ; searches = ref 0
    }
  ;;
end

module Render = Hardcaml_waveterm_kernel.Render.Make (Counting_data)

let draw_binary data ~cols ~cycles_per_char =
  let ctx = Draw.init ~rows:2 ~cols in
  Render.draw_binary_data
    ~ctx
    ~style:Style.default
    ~bounds:(Draw.get_bounds ctx)
    ~wave_scale:(`Time_steps_per_char cycles_per_char)
    ~data
    ~off:0
;;

let%expect_test "dense events stop scanning once fuzz is known" =
  let data =
    Counting_data.create
      (List.init 10_000 ~f:(fun index ->
         match index with
         | 0 -> 0
         | 1 -> 1
         | 2 -> 0
         | _ -> index % 2))
  in
  draw_binary data ~cols:1 ~cycles_per_char:10_000;
  (* [mask_reads] must stay 0: the 2-state render path must never consult the mask value. *)
  print_s
    [%message
      (data.data_reads : int ref)
        (data.mask_reads : int ref)
        (data.time_reads : int ref)
        (data.searches : int ref)];
  [%expect
    {|
    ((data.data_reads 4)
     (data.mask_reads 0)
     (data.time_reads 1)
     (data.searches   2))
    |}]
;;
