(** Graph: the dependencies between the modules of the manifest, as the facts
    show them.

    A provider reports the dependencies of files on other files and on packages.
    The owners declare modules.  The graph joins the two: every fact whose two
    ends, the subject and the object, belong to different modules becomes an
    occurrence of the edge between them.

    - The nodes are the modules of the manifest.
    - An edge is an ordered pair of modules with its occurrences: the facts
      themselves, as the provider wrote them.  The graph does not sum them up or
      derive anything from them.  Every field of a fact, such as the names or
      the evidence, stays available to the code that uses the graph.
    - A fact whose ends are in one module is not an edge.  The graph is about
      the architecture, and what happens inside a module is not part of it.

    The graph also says what it does not show.  A fact is not an edge when one
    of its ends has no single module:

    - a file that no pattern matches;
    - a file that several modules claim, when no single module wins;
    - a package that no module lists.

    A user who reads the graph must know that the picture is incomplete before
    trusting it.  Coverage collects such files and packages from every record
    that the provider reported.  It also keeps the unresolved records of the
    provider. *)

open Perpend_core
open Perpend_facts
open Perpend_manifest


(** {1 Edges} *)

(** The dependency of one module on another. *)
module Edge : sig
  (** The type of edges. *)
  type t = {
    source : Manifest.Id.t;
    (** The module that depends. *)
    target : Manifest.Id.t;
    (** The module that it depends on.  Never [source]. *)
    occurrences : Facts.Fact.t list;
    (** The facts behind the edge: every fact whose subject belongs to [source]
        and whose object belongs to [target], in the order of the facts.  At
        least one. *)
  }
end


(** {1 Coverage} *)

(** What the provider reported that the edges do not show. *)
module Coverage : sig
  (** The type of coverage.

      Coverage is the part of the graph that the edges do not show, so that a
      user who reads the graph knows where the picture is incomplete before
      trusting it.

      The graph attributes every file that the provider reported.  A provider
      names files in three kinds of records, and each kind can name a file
      that the other kinds do not:

      - A file record, {!Perpend_facts.Facts.File}, says that a file exists,
        with or without imports.  A provider with the [file_inventory]
        capability sends one for every file that it covers.
      - A fact, {!Perpend_facts.Facts.Fact}, names its subject, the file that
        has the import, and its object, the file that the import points to.
        The object can be a package instead; then the fact names one file,
        its subject.
      - An unresolved record, {!Perpend_facts.Facts.Unresolved}, names the file
        with the import that the provider could not follow.

      Coverage holds:

      - in [unattributed], the files that belong to no module;
      - in [ambiguous], the files that several modules claim, when no single
        module wins;
      - in [unlisted_packages], the packages that no module lists;
      - in [unresolved], the imports that the provider could not follow.

      A file that the graph puts in a module is in neither [unattributed] nor
      [ambiguous].  Neither list has a file twice, and both are in the order
      of first mention:

      + the file records;
      + the facts, with the subject of a fact before its object;
      + the unresolved records.

      The order is stated here because {!Perpend_facts.Facts.t} keeps the order
      of the records inside each kind only, not between kinds. *)
  type t = {
    unattributed : Path.t list;
    (** The files that no pattern of the manifest matches. *)
    ambiguous : (Path.t * (Glob.t * Manifest.Id.t) list) list;
    (** The files that patterns of several modules match, when no single module
        wins, with their candidates as {!Attribution.of_path} gives them. *)
    unlisted_packages : string list;
    (** The packages that the facts name and that no module lists, each once,
        in the order of the facts. *)
    unresolved : Facts.Unresolved.t list;
    (** The imports that the provider could not follow, as it reported them. *)
  }
end


(** {1 Graphs} *)

(** The type of graphs. *)
type t = {
  nodes : Manifest.Id.t list;
  (** Every module of the manifest, in the order of the manifest.  A module
      without edges and a module that stands for packages are nodes too. *)
  edges : Edge.t list;
  (** The edges, in the order of their first occurrences in the facts.  No two
      edges have the same source and target. *)
  coverage : Coverage.t;
  (** What the provider reported that the edges do not show. *)
}

val create : Manifest.t -> Facts.t -> t
(** [create manifest facts] is the graph of [facts] over the modules of
    [manifest].

    For each fact, one of these holds:

    - the subject belongs to a module, and the object belongs to another one:
      the fact is an occurrence of the edge between them;
    - both belong to the same module: the fact is not an edge;
    - otherwise the fact is not an edge either, and coverage names the file or
      the package that has no single module. *)
