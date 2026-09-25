(** The standard [String] module, plus substring search.  See [string.mli]. *)

include Stdlib.String

(** [occurs_at s sub i] is [true] iff [sub] occurs in [s] at [i].

    [i] must leave room for [sub]: [i + length sub <= length s]. *)
let occurs_at s sub i =
  let rec from j =
    j = length sub || (get sub j = get s (i + j) && from (j + 1))
  in
  from 0

let find_sub s ~sub =
  let last = length s - length sub in
  let rec find i =
    if i > last then
      None
    else if occurs_at s sub i then
      Some i
    else
      find (i + 1)
  in
  find 0

let rfind_sub s ~sub =
  let rec find i =
    if i < 0 then
      None
    else if occurs_at s sub i then
      Some i
    else
      find (i - 1)
  in
  find (length s - length sub)

let contains_sub s ~sub =
  Option.is_some (find_sub s ~sub)
