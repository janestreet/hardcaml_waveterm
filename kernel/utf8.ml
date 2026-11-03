open! Base

(* [Stdlib.String.get_utf_8_uchar] never raises: a malformed sequence decodes as U+FFFD
   with a positive advance, so arbitrary byte strings are safe to draw and measure. *)
let decode s ~pos = Stdlib.String.get_utf_8_uchar s pos
let advance d = Stdlib.Uchar.utf_decode_length d
let scalar d = Uchar.to_scalar (Stdlib.Uchar.utf_decode_uchar d)

let iteri s ~f =
  let bytes = String.length s in
  let rec loop pos i =
    if pos < bytes
    then (
      let d = decode s ~pos in
      f ~i ~scalar:(scalar d);
      loop (pos + advance d) (i + 1))
  in
  loop 0 0
;;

let length s =
  let length = ref 0 in
  iteri s ~f:(fun ~i:_ ~scalar:_ -> Int.incr length);
  !length
;;

let to_scalar_array s =
  let scalars = Array.create ~len:(length s) 0 in
  iteri s ~f:(fun ~i ~scalar -> scalars.(i) <- scalar);
  scalars
;;

(* The byte offset of codepoint [n] (the string's length in bytes when [n] runs past its
   end). *)
let byte_offset s n =
  let bytes = String.length s in
  let rec loop pos i =
    if i >= n || pos >= bytes then pos else loop (pos + advance (decode s ~pos)) (i + 1)
  in
  loop 0 0
;;

let sub s ~pos ~len =
  let start = byte_offset s pos in
  let stop = byte_offset s (pos + len) in
  String.sub s ~pos:start ~len:(stop - start)
;;
