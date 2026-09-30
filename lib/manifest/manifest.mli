(** The manifest is the architecture that the owners of a repository declare:
    the modules and the rules between them.

    This module gives the OCaml type of the manifest and a strict reader for
    its YAML text.

    A module is a named set of files, or a named set of external packages:

    {v
    version: 1
    modules:
      core:     {paths: ["src/core/**"]}
      plugins:  {paths: ["src/plugins/*/**"]}
      ext/orm:  {external: ["sqlalchemy", "drizzle-orm"]}
    v}

    - A module with [paths] is a part of the repository: its files are the paths
      that its patterns match.  The patterns of different modules can overlap;
      a file belongs to the module whose pattern describes it most closely.
    - A module with [external] is outside the repository: it stands for the
      packages listed, by the names that the language uses to import them.

    The reader accepts only what this format defines:

    - every key is required, and a module has exactly one of [paths] and
      [external];
    - nothing has a default;
    - an unknown key or a repeated key is an error.

    The manifest is the input that people write, and a misspelt key in it means
    a module or a rule that silently does not apply.  A lenient reader would
    turn a typo into a rule that passes.

    The reader takes YAML as a tree of mappings, lists and text, nothing more:

    - one document, whose root is a mapping;
    - a scalar is read as its key expects, text unless documented otherwise:
      [yes] is the string ["yes"] where text is expected, and the reader
      converts nothing on its own;
    - a pattern or a package name that starts with a YAML indicator, such as
      ['*'] or ['@'], must be quoted;
    - anchors, aliases and tags are errors.

    JSON is YAML, so a manifest may also be written as JSON. *)

open Perpend_core


(** {1 Modules} *)

(** Module ids. *)
module Id : sig
  type t
  (** The type of module ids.

      An id is one or more non-empty segments separated by ['/'], without ASCII
      control characters.  The segments give ids a hierarchy: [core/api] is
      under [core], [core-api] is not.  A rule that names a module applies to
      the modules under it too.  So the owners control which rules cover a
      module by its id: [ext/orm] is one of the [ext] modules, and a rule about
      [ext] covers it.  ['*'] is not allowed: it is the wildcard of patterns,
      and an id is a name, not a pattern. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same id. *)

  val hash : t -> int
  (** [hash id] is a hash of [id]: equal ids have equal hashes.  With [equal],
      it lets a hash table use ids as keys, as [Hashtbl.Make] requires. *)

  val of_string : string -> (t, string) result
  (** [of_string s] is [s] as an id.  A string that is not an id is an [Error]
      with a message naming the problem. *)

  val to_string : t -> string
  (** [to_string id] is the string that [id] was parsed from. *)
end

(** One declared module. *)
module Module : sig
  (** The type of the contents of a module: the files or the packages that it
      stands for.  It is one or the other, never both. *)
  type contents =
    | Paths of Glob.t list
    (** The files of the repository that the patterns match.  Written as
        [paths].  At least one pattern, without ASCII control characters, no
        pattern twice. *)
    | Packages of string list
    (** External packages, by the names that the language uses to import them:
        ["sqlalchemy"], ["@scope/name"], ["node:fs"].  Written as [external].
        At least one name, each non-empty and without ASCII control characters,
        no name twice. *)

  (** The type of modules.

      In YAML, a module is one entry of [modules]: the key is the id, the value
      is a mapping with exactly one of [paths] and [external]. *)
  type t = {
    id : Id.t;
    (** The id of the module.  No two modules have the same id. *)
    contents : contents;
    (** How the module is defined.  A pattern or a package appears in one module
        only. *)
  }
end

(** The type of manifests. *)
type t = {
  modules : Module.t list;
  (** The declared modules, in the order of the file.  At least one. *)
}


(** {1 Reading} *)

val version : int
(** The version of the manifest format that this module reads: [1].  The
    [version] key of a manifest must have this value, written as a plain scalar
    in decimal digits, without a sign or leading zeros: [1], not ["1"], [01] or
    [+1].

    The version changes only when a change of the format makes the manifests
    that were written before it invalid. *)

(** A reading error, with the line and the location of the node at fault. *)
type error = {
  line : int;
  (** The line of the input, from 1, where the node at fault starts, or, for an
      error about a key, where the key starts. *)
  location : string;
  (** The node at fault, by the keys and indices from the root, as in
      ["modules.core.paths[1]"]: keys are joined by ['.'], and an item of a list
      is written as [[i]].  Empty for the root.  When the error is about a key,
      such as an unknown or a repeated key, the location is the mapping that
      holds the key, and the message names the key. *)
  message : string;
  (** What is wrong with it, in one line. *)
}

val error_to_string : error -> string
(** [error_to_string e] is [e] as one line: ["line 12: modules.core: ..."], or
    ["line 1: ..."] for the root. *)

val of_string : string -> (t, error) result
(** [of_string s] reads the YAML text [s].

    - The text must be UTF-8 and one YAML document; a second document, or
      content after the first, is an error.
    - The root is a mapping with the keys [version] and [modules], in any order.
    - [modules] maps each id to its module and has at least one entry.
    - A YAML syntax error is reported with the words of the YAML parser.  The
      parser does not tell where it stopped, so the error is located at the last
      line that it read before the problem. *)
