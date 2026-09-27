(** The candidates of an ambiguous file.  See [candidates.mli]. *)

open Perpend_core
open Perpend_manifest

let to_string candidates =
  let candidate (pattern, id) =
    Printf.sprintf "%s (%s)" (Glob.to_string pattern) (Manifest.Id.to_string id)
  in
  String.concat ", " (List.map candidate candidates)
