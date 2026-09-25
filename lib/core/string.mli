(** The standard [String] module, plus substring search, which the standard
    module lacks. *)

include module type of Stdlib.String


(** {1 Substrings} *)

val find_sub : t -> sub:string -> int option
(** [find_sub s ~sub] is the position of the first occurrence of [sub] in
    [s], if any.  The empty string occurs at position 0. *)

val rfind_sub : t -> sub:string -> int option
(** [rfind_sub s ~sub] is the position of the last occurrence of [sub] in
    [s], if any.  The empty string occurs at position [length s]. *)

val contains_sub : t -> sub:string -> bool
(** [contains_sub s ~sub] is [true] iff [sub] occurs in [s]. *)
