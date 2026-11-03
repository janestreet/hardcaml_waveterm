(** UTF-8 helpers for the render stack. Text lands on the canvas one codepoint per cell,
    so every width / window / truncation computation over drawn text must count
    codepoints, not bytes - these are the shared primitives for that. Malformed sequences
    decode as U+FFFD rather than raising, so arbitrary strings are safe.

    Codepoints are equated with display cells (no double-width or combining-character
    handling), which matches the terminal for everything the viewer draws today. *)

open! Base

(** The number of codepoints in [s] (its display width in cells). *)
val length : string -> int

(** Iterate the codepoints of [s]: [f] gets the codepoint index and the unicode scalar. *)
val iteri : string -> f:(i:int -> scalar:int -> unit) -> unit

(** The codepoints of [s] as unicode scalars. *)
val to_scalar_array : string -> int array

(** [String.sub] in codepoint coordinates: the [len]-codepoint window of [s] starting at
    codepoint [pos]. The window is clamped to the end of the string. *)
val sub : string -> pos:int -> len:int -> string
