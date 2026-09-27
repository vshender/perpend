(** The [files] command.  See [files.mli]. *)

open Perpend_core
open Perpend_manifest
open Perpend_engine

(** [count n word] is [n] and [word], with the plural [s] when [n] is not one:
    ["1 file"], ["2 files"]. *)
let count n word =
  if n = 1 then
    Printf.sprintf "%d %s" n word
  else
    Printf.sprintf "%d %ss" n word

(** [section name lines] is a header with [name] and the number of [lines],
    then [lines], each indented by two spaces. *)
let section name lines =
  Printf.sprintf "%s (%s)\n" name (count (List.length lines) "file")
  ^ String.concat "" (List.map (Printf.sprintf "  %s\n") lines)

(** [module_section verdicts m] is the section of [m]: the paths that belong
    to it. *)
let module_section verdicts (m : Manifest.Module.t) =
  let files =
    List.filter_map
      (fun (path, verdict) ->
         match (verdict : Attribution.verdict) with
         | Module id when Manifest.Id.equal id m.id ->
           Some (Path.to_string path)
         | Module _ | Unattributed | Ambiguous _ -> None)
      verdicts
  in
  section (Manifest.Id.to_string m.id) files

let report (manifest : Manifest.t) paths =
  let attribution = Attribution.create manifest in
  let verdicts =
    List.map (fun path -> (path, Attribution.of_path attribution path)) paths
  in
  let modules =
    List.filter
      (fun (m : Manifest.Module.t) ->
         match m.contents with
         | Paths _    -> true
         | Packages _ -> false)
      manifest.modules
  and unattributed =
    List.filter_map
      (fun (path, verdict) ->
         match (verdict : Attribution.verdict) with
         | Unattributed           -> Some (Path.to_string path)
         | Module _ | Ambiguous _ -> None)
      verdicts
  and ambiguous =
    List.filter_map
      (fun (path, verdict) ->
         match (verdict : Attribution.verdict) with
         | Ambiguous candidates ->
           Some (Path.to_string path ^ ": " ^ Candidates.to_string candidates)
         | Module _ | Unattributed -> None)
      verdicts
  in
  let summary =
    Printf.sprintf "%s, %d unattributed, %d ambiguous\n"
      (count (List.length verdicts) "file")
      (List.length unattributed)
      (List.length ambiguous)
  in
  String.concat "\n"
    (List.map (module_section verdicts) modules
     @
     [
       section "unattributed" unattributed;
       section "ambiguous" ambiguous;
       summary;
     ])
