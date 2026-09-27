(** The candidates of an ambiguous file, as the commands print them. *)

open Perpend_core
open Perpend_manifest

val to_string : (Glob.t * Manifest.Id.t) list -> string
(** [to_string candidates] is [candidates] on one line: each pattern with its
    module in parentheses, separated by commas, as in
    ["**/__tests__/** (tests), **/*.css (styles)"]. *)
