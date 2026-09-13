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

  (* [segment s] is a path segment matching the pattern segment [s]: its
     literal pieces with a [fragment] in place of each "*".  A segment that
     comes out empty (a bare "*" replaced with an empty [fragment]) becomes
     "a", since a path segment cannot be empty. *)
  let segment s =
    let pieces = String.split_on_char '*' s in
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
    | s :: rest    ->
      let* seg = segment s in
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
