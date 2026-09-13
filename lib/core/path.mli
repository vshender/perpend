(** Repo-relative file paths, as tools like [git ls-files] print them: relative
    to the repository root, with ['/'] as the separator.

    A path is one or more non-empty segments separated by ['/'], none of them
    ["."] or [".."].  It is validated once, when it is parsed, so that every
    function taking a [t] can rely on that. *)

type t
(** The type of a validated path. *)

val of_string : string -> (t, string) result
(** [of_string s] is [s] as a path.  A string that is not a path is an [Error]
    with a message naming the problem. *)

val to_string : t -> string
(** [to_string p] is the string [p] was parsed from. *)

val segments : t -> string list
(** [segments p] is [p] split on ['/'].  The list is computed once, at parse
    time. *)
