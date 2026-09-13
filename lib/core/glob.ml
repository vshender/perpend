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
  (** A segment as a list of pieces.  The list is never empty, and literals
      and stars alternate: no two literals are adjacent. *)
  | Globstar
  (** ["**"]: zero or more whole segments.  The name comes from bash, where the
      [globstar] option gives ["**"] this meaning. *)

(** [is_literal segment] is [true] iff [segment] has no wildcards. *)
let is_literal = function
  | Pieces [Literal _] -> true
  | _                  -> false

(** [literal_chars segment] is the number of literal characters in
    [segment]. *)
let literal_chars = function
  | Globstar      -> 0
  | Pieces pieces ->
    List.fold_left
      (fun n piece ->
         match piece with
         | Literal l -> n + String.length l
         | Star      -> n)
      0
      pieces

module Specificity = struct
  (** A labeled tuple: [compare] goes left to right, so the order of the
      components is the priority order; [true] is more specific than [false].

      - [literal]: whether the pattern has no wildcards and so names one path;
      - [unanchored]: the number of literal characters after the first
        wildcard, not anchored to the repository root;
      - [anchored]: the number of literal characters before the first wildcard,
        anchored to the repository root;
      - [fixed_depth]: whether the pattern has no ["**"] and so matches paths
        of one depth only.

      Characters are counted as bytes; separators are not counted. *)
  type t = literal:bool * unanchored:int * anchored:int * fixed_depth:bool

  (** [of_segments segments] is the specificity of a pattern made of
      [segments]. *)
  let of_segments segments =
    (* [anchored_chars segments] counts the literal characters before the
       first wildcard: whole literal segments, then the literal start of the
       segment that holds the wildcard.  Separators are not counted. *)
    let rec anchored_chars = function
      | Pieces [Literal l] :: rest   -> String.length l + anchored_chars rest
      | Pieces (Literal l :: _) :: _ -> String.length l
      | _                            -> 0
    in
    let total = segments |> List.map literal_chars |> List.fold_left (+) 0 in
    let anchored = anchored_chars segments in
    (
      ~literal:(List.for_all is_literal segments),
      ~unanchored:(total - anchored),
      ~anchored,
      ~fixed_depth:(not (List.mem Globstar segments))
    )

  let compare = Stdlib.compare

  let to_string (~literal, ~unanchored, ~anchored, ~fixed_depth) =
    Printf.sprintf "literal=%b unanchored=%d anchored=%d fixed_depth=%b"
      literal unanchored anchored fixed_depth
end

(** The type of a parsed pattern. *)
type t = {
  source : string;
  (** The pattern as written. *)
  segments : segment list;
  (** The parsed pattern; a trailing ["**"] is stored as ["**/*"]. *)
  specificity : Specificity.t;
  (** The specificity of the pattern, computed once at parse time. *)
}

let to_string t =
  t.source


(** {1 Parsing} *)

(** [parse_segment s] parses one segment of a path. *)
let parse_segment s =
  if s = "**" then
    Ok Globstar
  else
    let n = String.length s in
    let rec parse_pieces i acc =
      if i >= n then
        Ok (Pieces (List.rev acc))
      else if s.[i] = '*' then
        if i + 1 < n && s.[i + 1] = '*' then
          Error "'**' must be a whole segment"
        else
          parse_pieces (i + 1) (Star :: acc)
      else
        let j =
          match String.index_from_opt s i '*' with
          | Some j -> j
          | None   -> n
        in
        parse_pieces j (Literal (String.sub s i (j - i)) :: acc)
    in
    parse_pieces 0 []

(** [normalize segments] collapses repeated ["**"] and turns a trailing
    ["**"] into ["**/*"], so that it takes at least one segment. *)
let rec normalize = function
  | []                                  -> []
  | [Globstar]                          -> [Globstar; Pieces [Star]]
  | Globstar :: (Globstar :: _ as rest) -> normalize rest
  | seg :: rest                         -> seg :: normalize rest

let parse source =
  let* path = Path.of_string source in
  let rec parse_all = function
    | []        -> Ok []
    | s :: rest ->
      let* seg = parse_segment s in
      let+ segs = parse_all rest in
      seg :: segs
  in
  let+ segments = parse_all (Path.segments path) in
  let segments = normalize segments in
  { source; segments; specificity = Specificity.of_segments segments }


(** {1 Matching} *)

(** [literal_at s i l] is [true] iff [s] contains [l] at position [i]. *)
let literal_at s i l =
  let k = String.length l in
  i + k <= String.length s
  &&
  let rec check j =
    j = k || (s.[i + j] = l.[j] && check (j + 1))
  in
  check 0

(** [match_pieces pieces s] is [true] iff [pieces] describe the whole segment
    [s].

    After Russ Cox, {{:https://research.swtch.com/glob} Glob Matching Can Be
    Simple And Fast Too}: on a mismatch, go back to the last ["*"] seen and let
    it take one more character.  An earlier star never needs to be revisited:
    any characters it could still take, the last star can take instead.

    O(n * m) in the worst case, with a constant stack. *)
let match_pieces pieces s =
  let n = String.length s in
  (* [pieces] at position [i] is what is left to match; [star] is the pieces
     after the last "*" and the position they are tried at. *)
  let rec match_pieces_aux pieces i star =
    match pieces with
    | Star :: rest ->
      match_pieces_aux rest i (Some (rest, i))
    | Literal l :: rest when literal_at s i l ->
      match_pieces_aux rest (i + String.length l) star
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

(** [match_segments segments path] is [true] iff [segments] describe the whole
    [path].

    The same algorithm as [match_pieces], with ["**"] over segments in place of
    ["*"] over characters. *)
let match_segments segments path =
  (* [segments] and [path] are what is left to match; [star] is the segments
     after the last "**" and the path they are tried on. *)
  let rec match_segments_aux segments path star =
    match segments, path with
    | Globstar :: rest, _ ->
      match_segments_aux rest path (Some (rest, path))
    | Pieces pieces :: rest, s :: path_rest when match_pieces pieces s ->
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

let specificity t =
  t.specificity
