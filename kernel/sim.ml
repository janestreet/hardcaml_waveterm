open Base
open Hardcaml

let get_type = function
  | None -> fun _ -> Wave_format.Binary
  | Some l ->
    fun n ->
      (try List.Assoc.find_exn l n ~equal:String.equal with
       | _ -> Wave_format.Binary)
;;

let traced_nodes sim cycle =
  let traced = Cyclesim.traced sim in
  List.filter_map traced ~f:(fun t ->
    let width = Signal.width t.signal in
    let d = Data.create width in
    let v = Cyclesim.lookup sim t.signal in
    Option.map v ~f:(fun v -> t, d, fun _ -> Data.set_mutable_unsafe d !cycle v))
;;

let wrap ?cfg sim =
  let cycle = ref 0 in
  let compare =
    match cfg with
    | None -> String.compare
    | Some s ->
      let f =
        let s = List.mapi ~f:(fun i (n, _) -> n, i) s in
        fun x -> List.Assoc.find s x ~equal:String.equal
      in
      fun a b ->
        (match f a, f b with
         | None, None -> String.compare a b
         | Some _, None -> -1
         | None, Some _ -> 1
         | Some a, Some b -> Int.compare a b)
  in
  let port (n, v) =
    match n with
    | "clock" | "clk" -> Wave.Clock n, fun _ -> ()
    | "reset" | "rst" ->
      let d = Data.create 1 in
      Wave.Binary (n, d), fun v -> Data.set d !cycle (if v then Bits.vdd else Bits.gnd)
    | _ ->
      let t = get_type cfg n in
      let width = Bits.width !v in
      let d = Data.create width in
      let wave =
        if width = 1 && Poly.equal t Binary
        then Wave.Binary (n, d)
        else Wave.Data (n, d, t, Left)
      in
      wave, fun _ -> Data.set d !cycle !v
  in
  let traced_nodes = traced_nodes sim cycle in
  let traced_waves =
    List.concat_map traced_nodes ~f:(fun ({ signal; names }, d, _) ->
      List.map names ~f:(fun n ->
        match n with
        | "clock" | "clk" -> Wave.Clock n
        | "reset" | "rst" ->
          let d = Data.create 1 in
          Wave.Binary (n, d)
        | _ ->
          let t = get_type cfg n in
          let width = Signal.width signal in
          if width = 1 && Poly.equal t Binary
          then Wave.Binary (n, d)
          else Wave.Data (n, d, t, Left)))
  in
  let in_ports = List.map (Cyclesim.in_ports sim) ~f:port in
  let out_ports = List.map (Cyclesim.out_ports ~clock_edge:Before sim) ~f:port in
  let waves =
    List.concat [ List.map in_ports ~f:fst; List.map out_ports ~f:fst; traced_waves ]
    |> List.sort ~compare:(fun w0 w1 -> compare (Wave.get_name w0) (Wave.get_name w1))
    |> Array.of_list
  in
  let updates =
    List.concat
      [ List.map in_ports ~f:snd
      ; List.map out_ports ~f:snd
      ; List.map traced_nodes ~f:(fun (_, _, f) -> f)
      ]
    |> Array.of_list
  in
  let tasks rst () =
    Array.iter ~f:(fun f -> f rst) updates;
    Int.incr cycle
  in
  let sim =
    Cyclesim.Private.modify
      sim
      [ After, Reset, tasks true; Before, At_clock_edge, tasks false ]
  in
  sim, waves
;;
