(** QCheck2 generators shared by the test executables.

    Everything is over a tiny alphabet; still, only about one random
    pattern in twenty matches a random path, so [instance] builds paths
    that match by construction. *)

open QCheck2.Gen

(** A name starts with a letter, so it is never ["."] or [".."]. *)
let name =
  map2
    (fun c s -> Printf.sprintf "%c%s" c s)
    (oneof_list ['a'; 'b'])
    (string_size ~gen:(oneof_list ['a'; 'b'; '.']) (int_range 0 2))

(** [joined_segments gen] is one to four segments from [gen], joined by
    ['/']. *)
let joined_segments gen =
  map (String.concat "/") (list_size (int_range 1 4) gen)


(** {1 Paths} *)

(** A valid path. *)
let path = joined_segments name

(** A string that is not a path: a path damaged in one of the ways
    [Path.of_string] rejects, or the empty string. *)
let invalid_path =
  let damage =
    oneof_list [
      (fun p -> Printf.sprintf "/%s" p);
      (fun p -> Printf.sprintf "%s/" p);
      (fun p -> Printf.sprintf "%s//%s" p p);
      (fun p -> Printf.sprintf "%s/./%s" p p);
      (fun p -> Printf.sprintf "%s/../%s" p p);
      (fun p -> Printf.sprintf "./%s" p);
      (fun p -> Printf.sprintf "%s/.." p);
    ]
  in
  oneof_weighted [
    (7, map2 (fun damage p -> damage p) damage path);
    (1, return "");
  ]


(** {1 Patterns} *)

(** A segment without wildcards. *)
let literal_segment = name

(** A segment with ["*"] in it: one to three names separated by stars,
    with or without a star at each end; or a bare ["*"]; or ["**"]. *)
let wildcard_segment =
  let starred =
    map3
      (fun lead names trail ->
         (* A single name needs a star at one end to be a wildcard segment. *)
         let lead = lead || (not trail && List.length names = 1) in
         Printf.sprintf "%s%s%s"
           (if lead then "*" else "")
           (String.concat "*" names)
           (if trail then "*" else ""))
      bool
      (list_size (int_range 1 3) name)
      bool
  in
  oneof_weighted [
    (6, starred);
    (1, return "*");
    (2, return "**");
  ]

(** A valid pattern. *)
let pattern = joined_segments (oneof [literal_segment; wildcard_segment])

(** A pattern without wildcards. *)
let literal_pattern = joined_segments literal_segment

(** [instance pattern] is a path that matches [pattern]: each ["*"] becomes
    a short run of letters, each ["**"] zero to two names, a trailing ["**"]
    one or two. *)
let instance pattern =
  (* What a "*" becomes: zero to two letters. *)
  let fragment = string_size ~gen:(oneof_list ['a'; 'b']) (int_range 0 2) in

  (* [segment seg] is a path segment matching the pattern segment [seg]: its
     literal pieces with a [fragment] in place of each "*".  A segment that
     comes out empty (a bare "*" replaced with an empty [fragment]) becomes
     "a", since a path segment cannot be empty. *)
  let segment seg =
    let pieces = String.split_on_char '*' seg in
    let+ fragments = list_size (return (List.length pieces - 1)) fragment in
    (* [interleave pieces fragments] is p0 f0 p1 f1 ... pn. *)
    let rec interleave pieces fragments =
      match pieces, fragments with
      | p :: ps, f :: fs -> p :: f :: interleave ps fs
      | ps, _            -> ps
    in
    match interleave pieces fragments |> String.concat "" with
    | ""  -> "a"
    | seg -> seg
  in

  (* [expand segments] is the list of path segments for the pattern
     segments; "**" in the middle expands to zero to two names, at the end
     to one or two, as the semantics require. *)
  let rec expand = function
    | []           -> return []
    | ["**"]       -> list_size (int_range 1 2) name
    | "**" :: rest ->
      let* names = list_size (int_range 0 2) name in
      let+ tail = expand rest in
      names @ tail
    | seg :: rest  ->
      let* seg = segment seg in
      let+ tail = expand rest in
      seg :: tail
  in

  pattern |> String.split_on_char '/' |> expand |> map (String.concat "/")

(** A pattern and a path to match it against.  Random pairs rarely match, so
    half of the pairs are built to be hard: a path that matches the pattern
    by construction, or the path just before a trailing ["**"], which the
    pattern must not match. *)
let pattern_and_path =
  oneof_weighted [
    (2, tup2 pattern path);
    (1, pattern >>= fun pat -> instance pat >|= fun inst -> (pat, inst));
    (1, literal_pattern >|= fun base -> (base ^ "/**", base));
  ]


(** {1 Inclusion} *)

(** [replace l i j x] is [l] with the elements [i] to [j] inclusive replaced by
    the single element [x], for [0 <= i <= j < List.length l]. *)
let replace l i j x =
  List.filteri (fun k _ -> k < i) l
  @ [x]
  @ List.filteri (fun k _ -> k > j) l

(** [generalize pattern] is a pattern that includes [pattern] by construction:
    [pattern] loosened one to three times, each time in one of two ways.

    - A non-empty substring of a segment other than ["**"] becomes ["*"]; stars
      that meet are merged.
    - A run of segments becomes ["**"].  A run of ["**"] segments only is
      skipped: replacing it changes nothing.

    A way that does not apply leaves the pattern as it is. *)
let generalize pattern =
  (* [merge_stars seg] is the segment [seg] with each run of stars as one. *)
  let merge_stars seg =
    let pieces = String.split_on_char '*' seg in
    let last = List.length pieces - 1 in
    pieces
    |> List.filteri (fun k piece -> piece <> "" || k = 0 || k = last)
    |> String.concat "*"
  in

  (* [substring_to_star segments] replaces a non-empty substring of one segment,
     from [first] to [last] inclusive, by "*". *)
  let substring_to_star segments =
    let* i = int_range 0 (List.length segments - 1) in
    let seg = List.nth segments i in
    if seg = "**" then
      return segments
    else
      let n = String.length seg in
      let* first = int_range 0 (n - 1) in
      let+ last = int_range first (n - 1) in
      let seg =
        String.sub seg 0 first ^ "*" ^ String.sub seg (last + 1) (n - last - 1)
      in
      replace segments i i (merge_stars seg)
  in

  (* [run_to_globstar segments] replaces a run of segments, from [first] to
     [last] inclusive, by "**". *)
  let run_to_globstar segments =
    let n = List.length segments in
    let* first = int_range 0 (n - 1) in
    let+ last = int_range first (n - 1) in
    let run = List.filteri (fun k _ -> first <= k && k <= last) segments in
    if List.exists (fun seg -> seg <> "**") run then
      replace segments first last "**"
    else
      segments
  in

  (* [loosen k segments] applies [k] more of the two ways, each chosen at
     random. *)
  let rec loosen k segments =
    if k = 0 then
      return segments
    else
      let* segments =
        oneof [substring_to_star segments; run_to_globstar segments]
      in
      loosen (k - 1) segments
  in

  let* k = int_range 1 3 in
  let+ segments = loosen k (String.split_on_char '/' pattern) in
  String.concat "/" segments

(** A pair of patterns for [Glob.specificity].  Random pairs are rarely nested,
    so half of the pairs are nested by construction, a pattern and a
    generalization of it, in either order. *)
let pattern_pair =
  oneof_weighted [
    (2, tup2 pattern pattern);
    (1, pattern >>= fun q -> generalize q >|= fun p -> (p, q));
    (1, pattern >>= fun q -> generalize q >|= fun p -> (q, p));
  ]
