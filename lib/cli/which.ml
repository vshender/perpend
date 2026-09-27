(** The [which] command.  See [which.mli]. *)

open Perpend_manifest
open Perpend_engine

let report manifest path =
  match Attribution.of_path (Attribution.create manifest) path with
  | Module id            -> Printf.sprintf "%s\n" (Manifest.Id.to_string id)
  | Unattributed         -> "unattributed\n"
  | Ambiguous candidates ->
    Printf.sprintf "ambiguous: %s\n" (Candidates.to_string candidates)
