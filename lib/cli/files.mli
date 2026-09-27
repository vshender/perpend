(** The [files] command: every module with its files.

    [files] shows how well the manifest describes the repository, from the
    manifest and the list of files alone: which files each module gets, which
    files no pattern covers, and where patterns of several modules compete.
    The report follows the manifest, module by module, so that the two can be
    read side by side while the patterns are adjusted. *)

open Perpend_core
open Perpend_manifest

val report : Manifest.t -> Path.t list -> string
(** [report manifest paths] is the report for [paths]: a section per module of
    [manifest] that has patterns, in the order of the manifest, then the
    sections [unattributed] and [ambiguous], then a summary line.  A blank
    line separates the sections from each other and from the summary line.

    A section starts with its name and the number of its files in parentheses,
    as in ["lib/core (8 files)"], and lists its files below it, one per line,
    indented by two spaces, in the order of [paths].

    - The section of a module lists the files that belong to it.  A module with
      no files is shown too: its patterns match nothing, other modules win every
      file that they match, or those files are ambiguous.
    - [unattributed] lists the files that no pattern matches.
    - [ambiguous] lists the files for which no single module wins: each line has
      the path, a colon and the candidates, as {!Candidates.to_string} prints
      them.

    The summary line has the number of files, of unattributed files and of
    ambiguous files.  The text ends with a newline. *)
