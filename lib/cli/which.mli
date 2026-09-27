(** The [which] command: the module of one file.

    [which] answers one question: which module does this file belong to?  It
    prints the verdict of the attribution for one path. *)

open Perpend_core
open Perpend_manifest

val report : Manifest.t -> Path.t -> string
(** [report manifest path] is the verdict for [path] as one line, ended by a
    newline:

    - the id of the module;
    - ["unattributed"] when no pattern matches;
    - ["ambiguous: "] and the candidates, as {!Candidates.to_string} prints
      them. *)
