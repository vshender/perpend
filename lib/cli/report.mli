(** The pieces of text that the reports of the commands share. *)

val count : int -> string -> string
(** [count n word] is [n] and [word], with the plural [s] when [n] is not one:
    ["1 file"], ["2 files"]. *)

val section : string -> string list -> string
(** [section name files] is a section of files: a header with [name] and the
    number of [files], as in ["unattributed (2 files)"], then a line per element
    of [files], indented by two spaces.  Every line ends with a newline. *)
