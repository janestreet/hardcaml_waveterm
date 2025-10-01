open Core

type t =
  { r : int
  ; c : int
  ; w : int
  ; h : int
  }
[@@deriving sexp_of]

let empty = { r = 0; c = 0; w = 0; h = 0 }
let inside t ~row ~col = row >= t.r && col >= t.c && row < t.r + t.h && col < t.c + t.w

let%expect_test "inside" =
  let test t ~are_inside ~are_outside =
    List.map are_inside ~f:(fun p -> p, true)
    @ List.map are_outside ~f:(fun p -> p, false)
    |> List.iter ~f:(fun ((row, col), should_be_inside) ->
      let inside = inside t ~row ~col in
      if not (Bool.equal inside should_be_inside)
      then
        raise_s
          [%message
            "Unexpected" (row : int) (col : int) (should_be_inside : bool) (inside : bool)];
      print_endline [%string {|(%{row#Int}, %{col#Int})  inside-%{inside#Bool}|}])
  in
  (*
     (1, 2) --- (1, 3)
       |           |
     (4, 2) --- (4, 3)
  *)
  test
    { r = 1; c = 2; h = 4; w = 2 }
    ~are_inside:[ 1, 2; 4, 2; 4, 3; 1, 3 ]
    ~are_outside:[ 0, 2; 1, 1; 0, 3; 1, 4; 5, 3; 4, 4; 5, 2; 4, 1 ];
  [%expect
    {|
    (1, 2)  inside-true
    (4, 2)  inside-true
    (4, 3)  inside-true
    (1, 3)  inside-true
    (0, 2)  inside-false
    (1, 1)  inside-false
    (0, 3)  inside-false
    (1, 4)  inside-false
    (5, 3)  inside-false
    (4, 4)  inside-false
    (5, 2)  inside-false
    (4, 1)  inside-false
    |}]
;;
