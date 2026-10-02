(** The [graph] command: the dependencies between the modules.

    [graph] shows the graph that the facts of a provider give over the modules
    of the manifest: which module depends on which.  The report follows the
    manifest, module by module, so that the two can be read side by side.  It
    also lists the files that have no single module: their facts are missing
    from the graph, and the manifest is the place to fix that. *)

open Perpend_manifest
open Perpend_engine

val report : hide:Manifest.Id.t list -> Graph.t -> (string, string) result
(** [report ~hide graph] is the report for [graph].  It has four parts, with a
    blank line between them:

    - The modules: a line per module with its id, in the order of the manifest.
      Under a module there is a line for every module that it depends on,
      indented by two spaces, with the number of facts, as in
      ["  -> lib/core (6 facts)"].  These lines are in the order of the manifest
      too.
    - The sections [unattributed] and [ambiguous], as {!Files.report} prints
      them, with a blank line between the two.
    - Two lines with numbers: the packages that no module lists, as in
      ["unlisted packages: 7"], and the imports that the provider could not
      follow, as in ["unresolved imports: 2"].
    - A summary line with the number of modules, of edges, and of the facts
      behind these edges, as in ["11 modules, 17 edges, 147 facts"].  A fact
      that is not an occurrence of an edge is not counted.

    The text ends with a newline.

    [hide] takes modules out of the report.  A module is hidden when its id is
    under an id of [hide], in the sense of
    {!Perpend_manifest.Manifest.Id.is_under}: [infra] hides [infra] and
    [infra/ci].  A hidden module has no line, so its own dependencies are not
    shown, and no other module shows its dependency on it.  The summary counts
    what is shown and gives the number of hidden modules, as in
    ["7 modules (4 hidden), 12 edges, 80 facts"].  The parts about files,
    packages and imports stay as they are.

    The result is an [Error] when an id of [hide] hides no module: such an id
    is probably a typing mistake.  The message names the first such id of
    [hide]. *)
