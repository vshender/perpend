(** Path patterns that select files.  See [glob.mli] for the semantics. *)

open Result.Syntax


(** {1 Representation} *)

(** A piece of one path segment. *)
type piece =
  | Literal of string
  (** Literal characters. *)
  | Star
  (** ["*"]: any run of characters except ['/']. *)

(** One segment of a pattern. *)
type segment =
  | Pieces of piece list
  (** A segment as a list of pieces.  The list is never empty, and literals and
      stars alternate: no two literals are adjacent. *)
  | Globstar of int
  (** [Globstar n]: at least [n] whole segments.  [parse_segment] reads ["**"]
      as [Globstar 0]; [normalize] then merges each run of ["**"] and ["*"]
      segments that has a ["**"] in it into one [Globstar] that counts the
      ["*"]s.  The name comes from bash, where the [globstar] option gives
      ["**"] its meaning. *)

(** The type of a parsed pattern. *)
type t = {
  source : string;
  (** The pattern as written. *)
  segments : segment list;
  (** The parsed pattern, as [normalize] leaves it: no two [Globstar] are
      adjacent, and no bare ["*"] segment is next to a [Globstar]. *)
}

let to_string t =
  t.source


(** {1 Parsing} *)

(** [parse_segment seg] parses one segment of a path. *)
let parse_segment seg =
  if seg = "**" then
    Ok (Globstar 0)
  else
    let n = String.length seg in
    let rec parse_pieces i acc =
      if i >= n then
        Ok (Pieces (List.rev acc))
      else if seg.[i] = '*' then
        if i + 1 < n && seg.[i + 1] = '*' then
          Error "'**' must be a whole segment"
        else
          parse_pieces (i + 1) (Star :: acc)
      else
        let j =
          match String.index_from_opt seg i '*' with
          | Some j -> j
          | None   -> n
        in
        parse_pieces j (Literal (String.sub seg i (j - i)) :: acc)
    in
    parse_pieces 0 []

(** [normalize segments] merges every run of ["**"] and ["*"] segments that has
    a ["**"] in it into one [Globstar n], where [n] is the number of ["*"]s.
    A trailing ["**"] counts as one more, so that it takes at least one segment.

    One form for a run keeps the inclusion check simple: runs that mean the same
    thing, as ["*/**"] and ["**/*/*"] do, become the same segments, so the check
    compares structure and never has to reason about different spellings of one
    pattern. *)
let normalize segments =
  (* [at_least_one segments] turns a trailing "**" into "**/*". *)
  let rec at_least_one = function
    | []           -> []
    | [Globstar n] -> [Globstar n; Pieces [Star]]
    | seg :: rest  -> seg :: at_least_one rest
  in

  (* [flush stars globstar rest] ends the run read so far, which had [stars]
     "*" segments and, if [globstar], a "**"; [rest] is what follows the run.

     - A run with a "**" becomes one [Globstar stars].
     - A run without one keeps its "*" segments. *)
  let flush stars globstar rest =
    if globstar then
      Globstar stars :: rest
    else
      List.init stars (fun _ -> Pieces [Star]) @ rest
  in

  (* [merge stars globstar segments] walks [segments] and merges each run of "*"
     and "**" segments with [flush]; [stars] and [globstar] are what it has seen
     of the current run so far. *)
  let rec merge stars globstar = function
    | []                    -> flush stars globstar []
    | Globstar _ :: rest    -> merge stars true rest
    | Pieces [Star] :: rest -> merge (stars + 1) globstar rest
    | seg :: rest           -> flush stars globstar (seg :: merge 0 false rest)
  in

  segments |> at_least_one |> merge 0 false

let parse source =
  let* path = Path.of_string source in
  let rec parse_all = function
    | []        -> Ok []
    | seg :: rest ->
      let* seg = parse_segment seg in
      let+ segs = parse_all rest in
      seg :: segs
  in
  let+ segments = parse_all (Path.segments path) in
  let segments = normalize segments in
  { source; segments }


(** {1 Matching} *)

(** [literal_at seg i lit] is [true] iff [seg] contains [lit] at position
    [i]. *)
let literal_at seg i lit =
  let n = String.length lit in
  i + n <= String.length seg
  &&
  let rec check j =
    j = n || (seg.[i + j] = lit.[j] && check (j + 1))
  in
  check 0

(** [match_pieces pieces seg] is [true] iff [pieces] describe the whole segment
    [seg].

    After Russ Cox, {{:https://research.swtch.com/glob} Glob Matching Can Be
    Simple And Fast Too}: on a mismatch, go back to the last ["*"] seen and let
    it take one more character.  An earlier star never needs to be revisited:
    any characters it could still take, the last star can take instead.

    O(n * m) in the worst case, with a constant stack. *)
let match_pieces pieces seg =
  let n = String.length seg in
  (* [pieces] at position [i] is what is left to match; [star] is the pieces
     after the last "*" and the position they are tried at. *)
  let rec match_pieces_aux pieces i star =
    match pieces with
    | Star :: rest ->
      match_pieces_aux rest i (Some (rest, i))
    | Literal lit :: rest when literal_at seg i lit ->
      match_pieces_aux rest (i + String.length lit) star
    | [] when i = n ->
      true
    | _ ->
      begin match star with
        | Some (rest, j) when j < n ->
          match_pieces_aux rest (j + 1) (Some (rest, j + 1))
        | _ ->
          false
      end
  in
  match_pieces_aux pieces 0 None

(** [drop n l] is [l] without its first [n] elements, or [None] if [l] has
    fewer than [n]. *)
let rec drop n l =
  if n = 0 then
    Some l
  else
    match l with
    | []        -> None
    | _ :: rest -> drop (n - 1) rest

(** [match_segments segments path] is [true] iff [segments] describe the whole
    [path].

    The same algorithm as [match_pieces], with [Globstar] over segments in place
    of ["*"] over characters: [Globstar n] first takes [n] segments, then one
    more at each backtrack.  When fewer than [n] are left, no backtrack can
    help, since each one leaves fewer still. *)
let match_segments segments path =
  (* [segments] and [path] are what is left to match; [star] is the segments
     after the last [Globstar] and the path they are tried on. *)
  let rec match_segments_aux segments path star =
    match segments, path with
    | Globstar n :: rest, _ ->
      begin match drop n path with
        | Some path -> match_segments_aux rest path (Some (rest, path))
        | None      -> false
      end
    | Pieces pieces :: rest, seg :: path_rest when match_pieces pieces seg ->
      match_segments_aux rest path_rest star
    | [], [] ->
      true
    | _ ->
      begin match star with
        | Some (rest, _ :: path_rest) ->
          match_segments_aux rest path_rest (Some (rest, path_rest))
        | _ ->
          false
      end
  in
  match_segments_aux segments path None

let matches t path =
  match_segments t.segments (Path.segments path)


(** {1 Specificity} *)

(** An atom of a segment, for comparing two patterns character by character: the
    literals of the two need not break at the same places. *)
type atom =
  | Char of char
  (** One literal character. *)
  | Any
  (** ["*"]. *)

(** [atoms pieces] is [pieces] as a list of atoms. *)
let atoms pieces =
  List.concat_map
    (function
      | Star      -> [Any]
      | Literal lit -> List.init (String.length lit) (fun i -> Char lit.[i]))
    pieces

(** [include_pieces p q] is [true] iff every segment matched by the pieces [q]
    is matched by the pieces [p].

    The algorithm of [match_pieces] with a pattern in place of the string:

    - a ["*"] of [p] takes any atoms of [q], a ["*"] of [q] included;
    - a ["*"] of [q] against anything else in [p] is a mismatch. *)
let include_pieces p q =
  (* [p] and [q] are what is left to compare; [star] is the atoms of [p]
     after its last "*" and the atoms of [q] they are tried on. *)
  let rec include_atoms p q star =
    match p, q with
    | Any :: p_rest, _ ->
      include_atoms p_rest q (Some (p_rest, q))
    | Char c :: p_rest, Char d :: q_rest when c = d ->
      include_atoms p_rest q_rest star
    | [], [] ->
      true
    | _ ->
      begin match star with
        | Some (p_rest, _ :: q_rest) ->
          include_atoms p_rest q_rest (Some (p_rest, q_rest))
        | _ ->
          false
      end
  in
  include_atoms (atoms p) (atoms q) None

(** [absorb n q] is the segments of [q] left after taking segments from its
    front until the taken segments guarantee at least [n] path segments in
    total, or [None] if [q] is too short.  A [Pieces] guarantees one path
    segment, a [Globstar m] guarantees [m]. *)
let rec absorb n q =
  if n <= 0 then
    Some q
  else
    match q with
    | []                 -> None
    | Globstar m :: rest -> absorb (n - m) rest
    | Pieces _ :: rest   -> absorb (n - 1) rest

(** [include_segments p q] is [true] iff every path matched by the segments [q]
    is matched by the segments [p].

    The same algorithm as [include_pieces], with [Globstar] over segments in
    place of ["*"] over atoms.  A [Globstar] of [q] against a [Pieces] of [p] is
    a mismatch.  When the next segment of [p] is a [Globstar n]:

    - it first takes segments of [q] until the taken segments guarantee at least
      [n] path segments in total, then one more at each backtrack;
    - it takes a [Globstar] of [q] whole, whatever the counts are: by the
      invariant of [normalize], what follows the [Globstar n] in [p] is nothing
      or a [Pieces] with a literal, and neither can take any part of the
      [Globstar] of [q];
    - when [q] has too few segments left to guarantee [n], no backtrack can
      help, since each one takes segments away from [q]. *)
let include_segments p q =
  (* [p] and [q] are what is left to compare; [star] is the segments of [p]
     after its last [Globstar] and the segments of [q] they are tried on. *)
  let rec include_segments_aux p q star =
    match p, q with
    | Globstar n :: p_rest, _ ->
      begin match absorb n q with
        | Some q -> include_segments_aux p_rest q (Some (p_rest, q))
        | None   -> false
      end
    | Pieces a :: p_rest, Pieces b :: q_rest when include_pieces a b ->
      include_segments_aux p_rest q_rest star
    | [], [] ->
      true
    | _ ->
      begin match star with
        | Some (p_rest, _ :: q_rest) ->
          include_segments_aux p_rest q_rest (Some (p_rest, q_rest))
        | _ ->
          false
      end
  in
  include_segments_aux p q None

(** [includes p q] is [true] iff every path matched by [q] is matched by [p].

    The walk is sound: it pairs a part of [p] only with a part of [q] whose
    every instance it matches, so when it reaches the end, [p] matches every
    path of [q].

    It is also complete: when [p] matches every path of [q], the walk says so.
    In short, call an {i alignment} a pairing of every segment of [p] with a
    block of segments of [q] that the rules of the walk allow.

    - An alignment exists whenever [p] matches every path of [q]: it is built
      from the matches of [p] on two paths made from [q].  In the first, every
      [Globstar] of [q] becomes more segments than [p] has, which shows which
      [Globstar] of [p] takes it whole; in the second, it becomes exactly its
      count, which shows that every block a [Globstar] of [p] takes has enough
      segments.  The two matches are then joined into one alignment.
    - The walk finds an alignment whenever one exists, by Cox's argument: a
      [Globstar] of [p] takes the least prefix that guarantees its count, then
      one segment more at each backtrack, and the next [Globstar] can take
      anything the previous one left over. *)
let includes p q =
  include_segments p.segments q.segments

(** [cross_cutting_segments segments] is [true] iff a literal comes after a
    wildcard in [segments]. *)
let cross_cutting_segments segments =
  (* [seen] is whether a wildcard has been passed. *)
  let rec walk_segments seen = function
    | []                    -> false
    | Globstar _ :: rest    -> walk_segments true rest
    | Pieces pieces :: rest -> walk_pieces seen pieces rest
  and walk_pieces seen pieces rest =
    match pieces with
    | []                  -> walk_segments seen rest
    | Star :: pieces      -> walk_pieces true pieces rest
    | Literal _ :: pieces -> seen || walk_pieces seen pieces rest
  in
  walk_segments false segments

(** [is_cross_cutting t] is [true] iff [t] has a literal after a wildcard. *)
let is_cross_cutting t =
  cross_cutting_segments t.segments

type reason =
  | Included
  | Cross_cutting

type specificity =
  | More_specific of reason
  | Less_specific of reason
  | Ambiguous

let specificity p q =
  match includes p q, includes q p with
  | false, true  -> More_specific Included
  | true,  false -> Less_specific Included
  | true,  true  -> Ambiguous
  | false, false ->
    begin match is_cross_cutting p, is_cross_cutting q with
      | true,  false -> More_specific Cross_cutting
      | false, true  -> Less_specific Cross_cutting
      | _            -> Ambiguous
    end
