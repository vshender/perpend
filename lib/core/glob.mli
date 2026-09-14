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
(** [matches p path] is [true] iff [p] describes [path] as documented above. *)


(** {1 Specificity} *)

(** Several patterns can match one path.  Specificity tells which of two
    patterns describes a path more closely, so that one of them can be
    chosen.  It is defined by two questions asked in turn.

    - Does exactly one pattern include the other?  Then the included one is more
      specific, whatever its shape: ["src/db/migrations/**"] beats
      ["**/migrations/**"], and ["**/*.test.ts"] beats ["**/*.ts"].
    - Otherwise, is exactly one of them cross-cutting?  A cross-cutting pattern
      has a literal after a wildcard, so some name in it is not tied to a fixed
      place: ["**/__tests__/**"], ["*/migrations/**"] and ["**/*.test.ts"] are
      cross-cutting, ["src/core/**"] and ["src/plugins/*/**"] are not.
      ["**/__tests__/**"] selects the tests wherever they are, and it beats a
      pattern that only names a place, such as ["src/ui/hooks/**"].

    Every other pair is ambiguous: ["**/__tests__/**"] and ["**/*.test.ts"] both
    match [src/__tests__/a.test.ts], and neither question tells them apart.  No
    rule on the patterns alone can settle such a pair: which one should win
    depends on what their author meant.  The author says so with a pattern for
    the overlap, here ["**/__tests__/**/*.test.ts"]: both patterns include it,
    so by inclusion it beats either.

    Specificity is not an order.  A pattern can be more specific than a second
    one, and the second than a third, while the first and the third are
    ambiguous, for example ["src/a/**"], ["**/a/**"] and ["src/*/*"].  It does
    not sort patterns; among several, the most specific one is the one that no
    other beats, if there is exactly one. *)

(** Why one pattern is more specific than another. *)
type reason =
  | Included
  (** It is included in the other pattern. *)
  | Cross_cutting
  (** Neither includes the other; it is cross-cutting and the other is not. *)

(** How two patterns compare. *)
type specificity =
  | More_specific of reason
  (** The first pattern is more specific. *)
  | Less_specific of reason
  (** The second pattern is more specific. *)
  | Ambiguous
  (** Neither is: the two match the same paths, as ["*/**"] and
      ["**/*/*"] do, or neither question tells them apart. *)

val specificity : t -> t -> specificity
(** [specificity p q] compares [p] with [q] as described above.
    [specificity p p] is [Ambiguous], and [specificity q p] is
    [specificity p q] with [More_specific] and [Less_specific] swapped. *)
