(** Attribution: the module of the manifest that a file or a package belongs to.

    The manifest declares modules, and a provider reports files and packages.
    Attribution maps one to the other:

    - a file belongs to the module whose pattern describes it most closely;
    - a package belongs to the module that lists it.

    Several patterns can match one file.  The candidates are the matching
    patterns that no other matching pattern beats, in the sense of
    {!val:Perpend_core.Glob.specificity}.  When all candidates belong to one
    module, the file belongs to that module.  That relation is not an order, so
    the patterns are compared in pairs and never sorted.  When patterns of
    several modules match and no single module wins, the file is ambiguous.  The
    manifest is the place to fix that: a pattern for the overlap makes one
    module win. *)

open Perpend_core
open Perpend_manifest


(** {1 Attributions} *)

type t
(** The type of attributions: the patterns and the packages of one manifest,
    each with its module. *)

val create : Manifest.t -> t
(** [create manifest] is the attribution over the modules of [manifest]. *)


(** {1 Files} *)

(** The type of verdicts: what [of_path] says about a file. *)
type verdict =
  | Module of Manifest.Id.t
  (** The file belongs to this module. *)
  | Unattributed
  (** No pattern of the manifest matches the file. *)
  | Ambiguous of (Glob.t * Manifest.Id.t) list
  (** Patterns of several modules match the file, and no single module wins.
      The list holds the candidates with their modules, in the order of the
      manifest; see [of_path] for what a candidate is. *)

val of_path : t -> Path.t -> verdict
(** [of_path t path] is the verdict for [path].

    The candidates are the patterns that match [path] and that no other
    matching pattern beats, in the sense of
    {!val:Perpend_core.Glob.specificity}.  Specificity is not an order, so every
    matching pattern can be beaten; then all of them are candidates.

    - No pattern matches [path]: the verdict is [Unattributed].
    - All candidates belong to one module: that module wins.  Several candidates
      of one module do not make the file ambiguous.
    - Otherwise the verdict is [Ambiguous]. *)


(** {1 Packages} *)

val of_package : t -> string -> Manifest.Id.t option
(** [of_package t name] is the module that lists the package [name], if any.
    Names are compared as written: the manifest gives a package by the name that
    the language uses to import it, and so does a provider. *)
