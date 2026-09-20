(** Facts are what a provider found in the repository.  They travel as a JSONL
    file: the provider prints it, the core reads it.

    A provider is an external process that scans the code.  Its output has one
    JSON object per line:

    - first, a header that names the provider;
    - then a file record for every file that it covers, a fact for every
      dependency that it found, and an unresolved record for every import that
      it could not follow.

    This module gives the OCaml types of those records, a strict reader and a
    writer.

    The reader accepts only what this protocol, [perpend.facts/1], defines:

    - every field is required unless it is documented as optional;
    - nothing has a default;
    - an unknown field or record type is an error.

    Providers are written for this protocol, so a misspelt field or record type
    is a bug, not a version difference, and it must fail loudly.  A lenient
    reader would turn it into missing data and let a rule pass on an incomplete
    picture.

    A fact says what depends on what, and where.  For example,
    [from b import foo, bar] on line 3 of [src/a.py] is one fact: the file
    [src/a.py] depends on the file [src/b.py] through the names [foo] and [bar],
    and the place is line 3 with that text.  The two parts play different roles:

    - what depends on what, names included, is the identity of the fact.  Later
      steps derive a key from it: the name under which the baseline and the
      reports recognise the same dependency from one run to the next;
    - the place is evidence for people.  An edit can move it without changing
      the dependency, so it is never part of a key.

    Paths are repo-relative, as {!Perpend_core.Path} defines them. *)

open Perpend_core


(** {1 Vocabulary} *)

(** What a dependency points to. *)
module Artifact : sig
  (** The type of artifacts, by kind and id, written as
      [{"kind": ..., "id": ...}]. *)
  type t =
    | File of Path.t
    (** A file of the repository. *)
    | Package of string
    (** An external package, by the name the language uses to import it:
        ["sqlalchemy"], ["@scope/name"], ["node:fs"].  Non-empty. *)
end

(** How reliably the provider found the target of a dependency. *)
module Precision : sig
  (** The type of the precision of a fact. *)
  type t =
    | Syntactic
    (** The provider read the import and looked the target up by name: for
        example, [utils.text] as [utils/text.py] under the source roots.  Fast
        and needs no build, but it can be wrong when the name fits several
        places or the import path is unusual; the provider's source roots are
        the setting to adjust then. *)
    | Resolved
    (** The language's own module resolver found the target, the one that the
        compiler or the bundler uses, with its path aliases and package lookups:
        for example, TypeScript maps [@app/util] through the [paths] of
        tsconfig.  More trustworthy than a syntactic fact. *)

  val of_string : string -> t option
  (** [of_string s] is the precision named [s] in JSON, if any. *)

  val to_string : t -> string
  (** [to_string p] is the JSON name of [p]. *)
end

(** Properties of the place where an import is written that tell how the
    dependency behaves at run time.

    Several flags can apply at once: an import inside a function inside a [try]
    block is both lazy and guarded.  Rules can ignore facts by flag. *)
module Flag : sig
  (** The type of flags. *)
  type t =
    | Type_only
    (** The dependency exists for the type checker only and is erased before the
        program runs.

        - Python: an import under [if TYPE_CHECKING:].
        - TypeScript: [import type], or an import that only types use. *)
    | Lazy
    (** The import runs when the code around it runs, not when the module loads.

        - Python: an import inside a function body.
        - JavaScript: [require()] or [import()] inside a function body. *)
    | Guarded
    (** The import is inside a [try] block, so the code can handle a missing
        target.

        - Python: [try:] around the import.
        - JavaScript: [try] around [require()]. *)
    | Dynamic
    (** The import is an expression, not a declaration, so the target is chosen
        when the code runs.  An import whose target is not a literal is not a
        fact but an unresolved record.

        - Python: [importlib.import_module] with a literal argument.
        - JavaScript: [import()]. *)

  val of_string : string -> t option
  (** [of_string s] is the flag named [s] in JSON, if any. *)

  val to_string : t -> string
  (** [to_string f] is the JSON name of [f]. *)
end

(** What a provider is able to report.

    When a rule needs a capability that the provider lacks, the rule is not
    evaluated, or only evaluated partially, and its result says so.  Missing
    data never counts as a pass. *)
module Capability : sig
  (** The type of capabilities. *)
  type t =
    | File_imports
    (** Facts between files of the repository. *)
    | Imported_names
    (** The names an import brings in, in [Fact.names]. *)
    | Package_imports
    (** Facts whose object is an external package. *)
    | Unresolved
    (** Unresolved records for the imports it could not follow. *)
    | Context_flags
    (** Flags on facts. *)
    | File_inventory
    (** A file record for every file it covers, so that files without
        dependencies are known too. *)

  val of_string : string -> t option
  (** [of_string s] is the capability named [s] in JSON, if any. *)

  val to_string : t -> string
  (** [to_string c] is the JSON name of [c]. *)
end


(** {1 Records}

    Every record is a JSON object whose ["type"] field names its kind. *)

(** The header: the first line of the file. *)
module Provider : sig
  (** The type of headers, ["type": "provider"], with one extra field:
      ["protocol"], which must be ["perpend.facts/1"]. *)
  type t = {
    name : string;
    (** The provider's name, such as ["py-grimp"].  Non-empty. *)
    version : string;
    (** The provider's version, such as ["0.1"].  Non-empty; it goes into the
        provenance of every decision. *)
    languages : string list;
    (** The languages the provider covers, by name, for example ["python"] or
        ["typescript"].  Each non-empty and listed once. *)
    capabilities : Capability.t list;
    (** What the provider is able to report.  Each listed once. *)
  }
end

(** A file the provider covers. *)
module File : sig
  (** The type of file records, ["type": "file"]. *)
  type t = {
    path : Path.t;
    (** The file. *)
    language : string option;
    (** The file's language, if the provider knows it.  Non-empty when
        present. *)
  }
end

(** Where a dependency is written.  Evidence is for people who read a finding,
    so that they can open the place; it is never part of a fact's identity,
    since an edit can move it. *)
module Evidence : sig
  (** The type of evidence. *)
  type t = {
    line : int option;
    (** The line, from 1, if the provider knows it. *)
    text : string option;
    (** The import as written, if the provider keeps it.  Non-empty when
        present.  For display only. *)
  }
end

(** A single dependency that the provider found: a file needs an artifact, at
    one place in the code.  Two imports of the same module from the same file
    are two facts. *)
module Fact : sig
  (** The type of facts, ["type": "fact"]. *)
  type t = {
    subject : Path.t;
    (** The file that depends; the import is written in it. *)
    object_ : Artifact.t;
    (** What it depends on.  ["object"] in JSON. *)
    precision : Precision.t;
    (** How the provider found it. *)
    names : string list;
    (** The names the import brings in, each non-empty and listed once; empty
        when the import brings in a whole module or the provider does not
        know. *)
    flags : Flag.t list;
    (** The context of the import.  Each listed once. *)
    evidence : Evidence.t;
    (** Where the import is written. *)
  }
end

(** An import the provider could not follow. *)
module Unresolved : sig
  (** The type of unresolved records, ["type": "unresolved"].

      The provider reports such imports instead of dropping them.  Then the core
      knows where its picture of the code is incomplete, and it can say so
      instead of passing a rule by mistake. *)
  type t = {
    subject : Path.t;
    (** The file that has the import. *)
    target : string;
    (** The import as written, or the dynamic call that stands for it.
        Non-empty. *)
    evidence : Evidence.t;
    (** Where it is written. *)
  }
end

(** The whole facts file.  Each list keeps the order in which the provider
    printed those records; the order between kinds is not kept. *)
type t = {
  provider : Provider.t;
  (** The header. *)
  files : File.t list;
  (** The file records; no path appears twice. *)
  facts : Fact.t list;
  (** The facts. *)
  unresolved : Unresolved.t list;
  (** The unresolved records. *)
}


(** {1 Reading and writing} *)

val protocol : string
(** The protocol this module implements: ["perpend.facts/1"]. *)

(** A reading error, located by line. *)
type error = {
  line : int;
  (** The line of the input, from 1. *)
  message : string;
  (** What is wrong with it, in one line. *)
}

val error_to_string : error -> string
(** [error_to_string e] is [e] as one line: ["line 12: ..."]. *)

val of_string : string -> (t, error) result
(** [of_string s] reads the JSONL text [s].  Reading stops at the first line
    that does not parse; then the header and the file records are checked, and
    the error names the line of the record at fault.

    - The text must be UTF-8.
    - Every line must be one JSON object with a ["type"]; a blank line is an
      error.
    - The first line must be the provider header, and no other line may be.
    - A line may end with ["\r"], and a final newline is allowed. *)

val to_string : t -> string
(** [to_string t] prints [t] as JSONL:

    - one compact object per line, each line ended by a newline;
    - the header first, then the files, the facts and the unresolved records;
    - fields in the order of the record declarations above, so that the same
      value always prints the same bytes.

    The writer does not check [t].  When [t] respects the constraints documented
    above, [of_string (to_string t)] is [Ok t]. *)
