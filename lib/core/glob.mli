(** Path patterns that select files.

    A pattern has the form of a {!Path}: one or more non-empty segments
    separated by ['/'], none of them ["."] or [".."].  Two things in it are
    special:

    - ["*"] inside a segment matches any run of characters except ['/'];
    - a segment that is exactly ["**"] matches zero or more whole segments;
      at the end of a pattern it matches at least one, so that ["src/**"]
      means the contents of [src] and never [src] itself, as in gitignore.
      Two stars in a row are allowed only as such a segment.

    Nothing else is special, and there is no way to escape ["*"]; patterns
    are case-sensitive and anchored at the repository root.

    A pattern matches a path when it describes the whole path.  A plain path
    therefore names exactly one file.

    - ["src/core"]         matches [src/core] only;
    - ["src/core/**"]      matches [src/core/a.py] and [src/core/x/y.py],
                           not [src/core];
    - ["src/*"]            matches [src/a.py], not [src/x/b.py];
    - ["src/plugins/*/**"] matches [src/plugins/a/x.py], not [src/plugins/a];
    - ["**/__tests__/**"]  matches [__tests__/a.ts] and [src/__tests__/x/a.ts];
    - ["**/*.test.ts"]     matches [a.test.ts] and [src/x/a.test.ts];
    - ["x/*/*.test.ts"]    matches [x/a/b.test.ts], not [x/b.test.ts] or
                           [x/a/c/b.test.ts];
    - ["**"]               matches every path. *)


(** {1 Patterns} *)

type t
(** The type of a parsed pattern. *)

val parse : string -> (t, string) result
(** [parse s] parses the pattern [s].  A string that is not a pattern is an
    [Error] with a message naming the problem. *)

val to_string : t -> string
(** [to_string p] is the string [p] was parsed from: if [parse s] is
    [Ok p], then [to_string p] is [s]. *)


(** {1 Matching} *)

val matches : t -> Path.t -> bool
(** [matches p path] is [true] iff [p] describes [path] as documented
    above. *)


(** {1 Specificity} *)

(** When several patterns match one path, the more specific one wins.  The
    type is abstract, so the order can change without touching callers.

    The current order compares, in turn:

    - whether the pattern has no wildcards, so that a pattern naming exactly
      the path beats every other pattern;
    - the number of literal characters after the first wildcard.  These
      are not anchored to the repository root, so [**/*.test.ts] and
      [**/__tests__/**] beat [src/ui/hooks/**] at any depth;
    - the number of literal characters before the first wildcard.  These
      are anchored to the repository root;
    - whether the pattern has no ["**"], so that a fixed depth beats any
      depth.

    Characters are counted as bytes; separators are not counted.  Patterns
    with equal specificity are a tie, which the caller reports. *)
module Specificity : sig
  type t
  (** The type of pattern specificity. *)

  val compare : t -> t -> int
  (** [compare a b] is negative if [a] is less specific than [b], zero if
      they tie, positive otherwise. *)

  val to_string : t -> string
  (** [to_string s] renders [s] for diagnostics. *)
end

val specificity : t -> Specificity.t
(** [specificity p] is the specificity of [p]. *)
