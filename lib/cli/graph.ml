(** The [graph] command.  See [graph.mli]. *)

open Perpend_core
open Perpend_manifest
open Perpend_engine

(** [edge_line e] is the line of the edge [e] under its source: the target and
    the number of facts. *)
let edge_line (e : Graph.Edge.t) =
  Printf.sprintf "  -> %s (%s)\n"
    (Manifest.Id.to_string e.target)
    (Report.count (List.length e.occurrences) "fact")

(** [module_lines nodes edges id] is the line of the module [id], then a line
    for every edge of [edges] that starts at [id].  [nodes] is the ids of the
    modules to show, in the order of the manifest.  The lines of the edges
    follow the same order, by target. *)
let module_lines nodes edges id =
  let outgoing =
    edges |> List.filter
      (fun (e : Graph.Edge.t) -> Manifest.Id.equal e.source id)
  in

  (* [edge_to target] is the edge from [id] to the module [target], if any. *)
  let edge_to target =
    outgoing |> List.find_opt
      (fun (e : Graph.Edge.t) -> Manifest.Id.equal e.target target)
  in

  Manifest.Id.to_string id ^ "\n"
  ^ String.concat "" (nodes |> List.filter_map edge_to |> List.map edge_line)

let report ~hide (graph : Graph.t) =
  (* [hidden id] is [true] iff some id of [hide] hides the module [id]. *)
  let hidden id =
    List.exists (fun ancestor -> Manifest.Id.is_under ~ancestor id) hide
  in

  (* The first id of [hide] that hides no module, if any. *)
  let useless_id =
    hide |> List.find_opt
      (fun ancestor ->
         not (List.exists (Manifest.Id.is_under ~ancestor) graph.nodes))
  in

  match useless_id with
  | Some id ->
    Error
      (Printf.sprintf "no module is '%s' or under it"
         (Manifest.Id.to_string id))

  | None ->
    let nodes = List.filter (Fun.negate hidden) graph.nodes in
    let edges =
      graph.edges |> List.filter
        (fun (e : Graph.Edge.t) ->
           not (hidden e.source) && not (hidden e.target))
    in
    let modules =
      String.concat "" (List.map (module_lines nodes edges) nodes)
    and unattributed =
      Report.section "unattributed"
        (List.map Path.to_string graph.coverage.unattributed)
    and ambiguous =
      Report.section "ambiguous"
        (graph.coverage.ambiguous |> List.map
           (fun (path, candidates) -> Report.ambiguous_file path candidates))
    and numbers =
      Printf.sprintf "unlisted packages: %d\nunresolved imports: %d\n"
        (List.length graph.coverage.unlisted_packages)
        (List.length graph.coverage.unresolved)
    and summary =
      let hidden_count = List.length graph.nodes - List.length nodes
      and facts =
        edges |> List.fold_left
          (fun n (e : Graph.Edge.t) -> n + List.length e.occurrences)
          0
      in
      Printf.sprintf "%s%s, %s, %s\n"
        (Report.count (List.length nodes) "module")
        (if hidden_count = 0 then
           ""
         else
           Printf.sprintf " (%d hidden)" hidden_count)
        (Report.count (List.length edges) "edge")
        (Report.count facts "fact")
    in
    (* The first part is empty when every module is hidden. *)
    [modules; unattributed; ambiguous; numbers; summary]
    |> List.filter (fun part -> part <> "")
    |> String.concat "\n"
    |> Result.ok
