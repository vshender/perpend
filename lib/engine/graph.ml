(** Graph.  See [graph.mli]. *)

open Perpend_core
open Perpend_facts
open Perpend_manifest


(** {1 Edges} *)

module Edge = struct
  type t = {
    source : Manifest.Id.t;
    target : Manifest.Id.t;
    occurrences : Facts.Fact.t list;
  }
end


(** {1 Coverage} *)

module Coverage = struct
  type t = {
    unattributed : Path.t list;
    ambiguous : (Path.t * (Glob.t * Manifest.Id.t) list) list;
    unlisted_packages : string list;
    unresolved : Facts.Unresolved.t list;
  }
end

(** [coverage files packages unresolved] is the coverage over:

    - [files], each with its verdict;
    - [packages], each with its module, if any;
    - the [unresolved] records.

    [files] and [packages] are in the order of first mention, and the coverage
    keeps it. *)
let coverage files packages unresolved : Coverage.t =
  {
    unattributed =
      files |> List.filter_map
        (fun (path, verdict) ->
           match (verdict : Attribution.verdict) with
           | Unattributed           -> Some path
           | Module _ | Ambiguous _ -> None);
    ambiguous =
      files |> List.filter_map
        (fun (path, verdict) ->
           match (verdict : Attribution.verdict) with
           | Ambiguous candidates    -> Some (path, candidates)
           | Module _ | Unattributed -> None);
    unlisted_packages =
      packages |> List.filter_map
        (fun (name, id) ->
           match id with
           | None   -> Some name
           | Some _ -> None);
    unresolved;
  }


(** {1 Tables} *)

(** A table from keys to values that also remembers the order in which the keys
    were first added.  The graph reports files, packages and edges in the order
    of first mention, and a hash table alone forgets it. *)
module Ordered_table (Key : Hashtbl.HashedType) = struct
  module Table = Hashtbl.Make (Key)

  (** The type of tables with values of type ['v]. *)
  type 'v t = {
    table : 'v Table.t;
    (** The values by key. *)
    mutable entries : (Key.t * 'v) list;
    (** The pairs, newest first. *)
  }

  (** [create ()] is an empty table. *)
  let create () =
    { table = Table.create 64; entries = [] }

  (** [find_or_add t key ~default] is the value of [key] in the table [t].  When
      [key] is new, its value is [default ()], and the pair becomes the newest
      entry. *)
  let find_or_add t key ~default =
    match Table.find_opt t.table key with
    | Some value -> value
    | None       ->
      let value = default () in
      Table.add t.table key value;
      t.entries <- (key, value) :: t.entries;
      value

  (** [to_list t] is the pairs of the table [t] in the order in which they
      were added. *)
  let to_list t =
    List.rev t.entries
end

(** The ends of an edge: the module that depends and the module that it depends
    on. *)
module Ends = struct
  type t = Manifest.Id.t * Manifest.Id.t
  (** The type of ends: the source and the target. *)

  (** [equal a b] is [true] iff the ends [a] and [b] have the same source and
      the same target. *)
  let equal (source, target) (source', target') =
    Manifest.Id.equal source source' && Manifest.Id.equal target target'

  (** [hash ends] is a hash of [ends]: equal ends have equal hashes. *)
  let hash (source, target) =
    Hashtbl.hash (Manifest.Id.hash source, Manifest.Id.hash target)
end

(** Tables with files as keys. *)
module Path_table = Ordered_table (Path)

(** Tables with the names of packages as keys. *)
module String_table = Ordered_table (String)

(** Tables with the ends of edges as keys. *)
module Ends_table = Ordered_table (Ends)


(** {1 Graphs} *)

type t = {
  nodes : Manifest.Id.t list;
  edges : Edge.t list;
  coverage : Coverage.t;
}

(** An open graph: a graph that is being built.  It holds the files, the
    packages and the edges of the records that were read so far. *)
type open_graph = {
  attribution : Attribution.t;
  (** The attribution over the manifest. *)
  files : Attribution.verdict Path_table.t;
  (** Every file that the records mentioned so far, with its verdict. *)
  packages : Manifest.Id.t option String_table.t;
  (** Every package that the facts mentioned so far, by name, with its module,
      if any. *)
  edges : Facts.Fact.t list ref Ends_table.t;
  (** Every edge so far, by its ends, with its occurrences, newest first. *)
}

(** [verdict_of g path] is the verdict for the file [path] in the open graph
    [g].  The first call for a path attributes it and records the mention; later
    calls only look it up. *)
let verdict_of g path =
  Path_table.find_or_add g.files path
    ~default:(fun () -> Attribution.of_path g.attribution path)

(** [mention_file g path] records in the open graph [g] that a record mentions
    the file [path], so that coverage sees the file. *)
let mention_file g path =
  ignore (verdict_of g path)

(** [module_of_file g path] is the module of the file [path] in the open graph
    [g], if the file has exactly one.  It records the mention as [verdict_of]
    does. *)
let module_of_file g path =
  match verdict_of g path with
  | Module id                  -> Some id
  | Unattributed | Ambiguous _ -> None

(** [module_of_package g name] is the module that lists the package [name], if
    any.  The first call for a name records the mention in the open graph
    [g]. *)
let module_of_package g name =
  String_table.find_or_add g.packages name
    ~default:(fun () -> Attribution.of_package g.attribution name)

(** [add_occurrence g ~source ~target fact] adds [fact] to the edge from the
    module [source] to the module [target] in the open graph [g], and creates
    the edge at its first occurrence. *)
let add_occurrence g ~source ~target fact =
  let occurrences =
    Ends_table.find_or_add g.edges (source, target) ~default:(fun () -> ref [])
  in
  occurrences := fact :: !occurrences

(** [add_fact g fact] records [fact] in the open graph [g] as an occurrence when
    its subject and its object belong to two different modules.  Both ends are
    looked up in any case, so that coverage sees every file and package that a
    fact mentions. *)
let add_fact g (fact : Facts.Fact.t) =
  let source = module_of_file g fact.subject in
  let target =
    match fact.object_ with
    | File path    -> module_of_file g path
    | Package name -> module_of_package g name
  in
  match source, target with
  | Some source, Some target when not (Manifest.Id.equal source target) ->
    add_occurrence g ~source ~target fact
  | _ -> ()  (* one module, or an end without a single module *)

(** [edge ((source, target), occurrences)] is the edge from the module [source]
    to the module [target].  [occurrences] is a reference to the list of its
    facts, newest first, and the edge has them in the order of the facts. *)
let edge ((source, target), occurrences) =
  { Edge.source; target; occurrences = List.rev !occurrences }

let create (manifest : Manifest.t) (facts : Facts.t) =
  let g =
    {
      attribution = Attribution.create manifest;
      files = Path_table.create ();
      packages = String_table.create ();
      edges = Ends_table.create ();
    }
  in
  (* The order of first mention: file records, then facts, then unresolved
     records. *)
  facts.files |> List.iter
    (fun (file : Facts.File.t) -> mention_file g file.path);
  facts.facts |> List.iter (add_fact g);
  facts.unresolved |> List.iter
    (fun (u : Facts.Unresolved.t) -> mention_file g u.subject);
  {
    nodes = List.map (fun (m : Manifest.Module.t) -> m.id) manifest.modules;
    edges = List.map edge (Ends_table.to_list g.edges);
    coverage =
      coverage
        (Path_table.to_list g.files)
        (String_table.to_list g.packages)
        facts.unresolved;
  }
