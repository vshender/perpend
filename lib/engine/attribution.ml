(** Attribution.  See [attribution.mli]. *)

open Perpend_core
open Perpend_manifest


(** {1 Attributions} *)

(** The type of attributions. *)
type t = {
  patterns : (Glob.t * Manifest.Id.t) list;
  (** Every pattern of the manifest with its module, in the order of the
      manifest. *)
  packages : (string * Manifest.Id.t) list;
  (** Every package of the manifest with its module. *)
}

let create (manifest : Manifest.t) =
  let patterns_of (m : Manifest.Module.t) =
    match m.contents with
    | Paths patterns -> List.map (fun p -> (p, m.id)) patterns
    | Packages _     -> []
  and packages_of (m : Manifest.Module.t) =
    match m.contents with
    | Paths _           -> []
    | Packages packages -> List.map (fun n -> (n, m.id)) packages
  in
  {
    patterns = List.concat_map patterns_of manifest.modules;
    packages = List.concat_map packages_of manifest.modules;
  }


(** {1 Files} *)

type verdict =
  | Module of Manifest.Id.t
  | Unattributed
  | Ambiguous of (Glob.t * Manifest.Id.t) list

(** [beats p q] is [true] iff [p] is more specific than [q]. *)
let beats p q =
  match Glob.specificity p q with
  | Glob.More_specific _                  -> true
  | Glob.Less_specific _ | Glob.Ambiguous -> false

let of_path t path =
  let matching = List.filter (fun (p, _) -> Glob.matches p path) t.patterns in
  (* [beaten (p, _)] is [true] iff some matching pattern beats [p]. *)
  let beaten (p, _) =
    List.exists (fun (q, _) -> beats q p) matching
  in
  let candidates =
    match List.filter (Fun.negate beaten) matching with
    | []       -> matching  (* no unbeaten pattern: all are candidates *)
    | unbeaten -> unbeaten  (* the usual case: the unbeaten are candidates *)
  in
  match candidates with
  | []                -> Unattributed
  | (_, id) :: others ->
    if List.for_all (fun (_, id') -> Manifest.Id.equal id id') others then
      Module id
    else
      Ambiguous candidates


(** {1 Packages} *)

let of_package t name =
  List.assoc_opt name t.packages
