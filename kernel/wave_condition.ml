open Hardcaml

module How_to_find = struct
  type t =
    | Suffix of string
    | Regex of Display_rule.Regexp.t
end

type t =
  { how_to_find : How_to_find.t
  ; condition : Bits.t -> bool
  }
