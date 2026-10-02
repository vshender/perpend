(** The pieces of text that the reports of the commands share. *)

open Perpend_core
open Perpend_manifest

val count : int -> string -> string
(** [count n word] is [n] and [word], with the plural [s] when [n] is not one:
    ["1 file"], ["2 files"]. *)

val section : string -> string list -> string
(** [section name files] is a section of files: a header with [name] and the
    number of [files], as in ["unattributed (2 files)"], then a line per element
    of [files], indented by two spaces.  Every line ends with a newline. *)

val ambiguous_file : Path.t -> (Glob.t * Manifest.Id.t) list -> string
(** [ambiguous_file path candidates] is the line of the ambiguous file [path]
    in a report: the path, a colon and the candidates, as
    {!Candidates.to_string} prints them, as in
    ["src/a.css: **/__tests__/** (tests), **/*.css (styles)"]. *)
