(** Tests for [Glob]. *)

open Perpend_core


(** {1 Test helpers} *)

(** [parse_exn s] is [s] parsed as a pattern; it fails the test if [s] does
    not parse. *)
let parse_exn s =
  match Glob.parse s with
  | Ok p    -> p
  | Error e -> failwith (Printf.sprintf "%s: %s" s e)

(** [path_exn s] is [s] as a path; it fails the test if [s] is not a valid
    path. *)
let path_exn s =
  match Path.of_string s with
  | Ok p    -> p
  | Error e -> failwith (Printf.sprintf "%s: %s" s e)

(** [matches pattern path] is [Glob.matches] on strings. *)
let matches pattern path =
  Glob.matches (parse_exn pattern) (path_exn path)

(** [specificity p q] is [Glob.specificity] on strings. *)
let specificity p q =
  Glob.specificity (parse_exn p) (parse_exn q)

(** [specificity_to_string answer] renders [answer] for test output. *)
let specificity_to_string =
  let reason = function
    | Glob.Included      -> "Included"
    | Glob.Cross_cutting -> "Cross_cutting"
  in
  function
  | Glob.More_specific r -> "More_specific " ^ reason r
  | Glob.Less_specific r -> "Less_specific " ^ reason r
  | Glob.Ambiguous       -> "Ambiguous"

(** [mirror_specificity answer] is [answer] with the two patterns swapped. *)
let mirror_specificity = function
  | Glob.More_specific r -> Glob.Less_specific r
  | Glob.Less_specific r -> Glob.More_specific r
  | Glob.Ambiguous       -> Glob.Ambiguous

(** A naive matcher, straight from the specification in [glob.mli], to check
    the real one against. *)
module Reference = struct
  (** [match_pieces pieces seg] is [true] iff the ["*"]-separated literals
      [pieces] describe the whole segment [seg]. *)
  let rec match_pieces pieces seg =
    match pieces with
    | []          -> seg = ""
    | [lit]       -> lit = seg
    | lit :: rest ->
      String.starts_with ~prefix:lit seg
      &&
      let k = String.length lit in
      let seg = String.sub seg k (String.length seg - k) in
      (* The star takes 0, 1, 2, ... characters. *)
      let rec try_from i =
        i <= String.length seg
        &&
        begin
          match_pieces rest (String.sub seg i (String.length seg - i))
          || try_from (i + 1)
        end
      in
      try_from 0

  (** [match_segments pattern path] is [true] iff the pattern segments
      describe the whole path, both given as segment lists. *)
  let rec match_segments pattern path =
    match pattern, path with
    | [], []           -> true
    | ["**"], _        -> path <> []
    | "**" :: rest, [] -> match_segments rest []
    | "**" :: rest, _ :: path_rest ->
      match_segments rest path || match_segments pattern path_rest
    | pat :: rest, seg :: path_rest ->
      match_pieces (String.split_on_char '*' pat) seg
      && match_segments rest path_rest
    | _ -> false

  (** [matches pattern path] is [true] iff [pattern] describes [path]. *)
  let matches pattern path =
    match_segments
      (String.split_on_char '/' pattern)
      (String.split_on_char '/' path)
end


(** {1 parse} *)

(** Test cases for the strings [Glob.parse] rejects. *)
let parse_error_tests =
  let open Alcotest in
  let parse_error_test pattern =
    test_case
      (Printf.sprintf "%S is an error" pattern)
      `Quick
      (fun () ->
         match Glob.parse pattern with
         | Error _ -> ()
         | Ok _    -> failf "%S should not parse" pattern)
  in [
    parse_error_test "";
    parse_error_test "/src";
    parse_error_test "src/";
    parse_error_test "src//core";
    parse_error_test "./src";
    parse_error_test "src/../core";
    parse_error_test "src/a**";
    parse_error_test "src/**a";
    parse_error_test "**a/b";
    parse_error_test "a***b";
  ]

(** Check that [Glob.to_string] gives back the string a pattern was parsed
    from. *)
let round_trip_test =
  Util.property_test
    ~name:"to_string (parse s) = s for every pattern s"
    ~print:QCheck2.Print.string
    Generators.pattern
    (fun s -> Glob.to_string (parse_exn s) = s)


(** {1 matches} *)

(** Test cases for [Glob.matches]; they read as the specification. *)
let match_tests =
  let open Alcotest in
  let match_test pattern path expected =
    test_case
      (Printf.sprintf "%S %s %S"
         pattern
         (if expected then "matches" else "does not match")
         path)
      `Quick
      (fun () ->
         (check bool)
           (Printf.sprintf "matches %S %S" pattern path)
           expected
           (matches pattern path))
  in [
    (* A plain path names exactly one file. *)
    match_test "src/core" "src/core" true;
    match_test "src/core" "src/core/api.py" false;
    match_test "src/core" "src" false;
    match_test "README.md" "README.md" true;
    match_test "README.md" "docs/README.md" false;

    (* "*" stays inside one segment. *)
    match_test "src/*" "src/a.py" true;
    match_test "src/*" "src/x/b.py" false;
    match_test "src/*" "src" false;
    match_test "src/*.py" "src/a.py" true;
    match_test "src/*.py" "src/.py" true;
    match_test "src/*.py" "src/a.pyc" false;
    match_test "src/test_*.py" "src/test_a.py" true;
    match_test "src/test_*.py" "src/a_test.py" false;
    match_test "src/*_*.py" "src/a_b_c.py" true;
    match_test "x/*/*.test.ts" "x/a/b.test.ts" true;
    match_test "x/*/*.test.ts" "x/b.test.ts" false;
    match_test "x/*/*.test.ts" "x/a/c/b.test.ts" false;

    (* A trailing "**" is everything inside, not the path itself. *)
    match_test "src/core/**" "src/core/api.py" true;
    match_test "src/core/**" "src/core/x/y.py" true;
    match_test "src/core/**" "src/core" false;
    match_test "src/core/**" "src/corelib/a.py" false;
    match_test "**" "a.py" true;
    match_test "**" "anything/at/all.py" true;

    (* "**" is zero or more whole segments. *)
    match_test "**/*.test.ts" "a.test.ts" true;
    match_test "**/*.test.ts" "src/x/a.test.ts" true;
    match_test "**/*.test.ts" "src/x/a.ts" false;
    match_test "src/**/api.py" "src/api.py" true;
    match_test "src/**/api.py" "src/core/api.py" true;
    match_test "src/**/api.py" "src/core/v1/api.py" true;
    match_test "src/**/api.py" "core/api.py" false;

    (* A run of "*" and "**" is a minimum number of segments, whatever its
       order. *)
    match_test "*/**" "a" false;
    match_test "*/**" "a/b" true;
    match_test "**/*/**" "a/b" true;
    match_test "**/*/**" "a" false;
    match_test "x/**/*/*/y" "x/a/b/y" true;
    match_test "x/**/*/*/y" "x/a/y" false;

    (* A directory at any depth needs "**" on both sides. *)
    match_test "**/__tests__/**" "__tests__/a.ts" true;
    match_test "**/__tests__/**" "src/__tests__/x/a.ts" true;
    match_test "**/__tests__/**" "src/__tests__x/a.ts" false;
    match_test "**/__tests__" "src/__tests__/a.ts" false;
    match_test "**/__tests__" "src/__tests__" true;

    (* Families: "*" and then everything inside. *)
    match_test "src/plugins/*/**" "src/plugins/a/x.py" true;
    match_test "src/plugins/*/**" "src/plugins/a/x/y.py" true;
    match_test "src/plugins/*/**" "src/plugins/a" false;

    (* Case and anchoring. *)
    match_test "src/Core" "src/core" false;
    match_test "core" "src/core/api.py" false;
    match_test "**/core" "src/core" true;

    (* Nothing else is special. *)
    match_test "a?.ml" "a?.ml" true;
    match_test "a?.ml" "ab.ml" false;
    match_test "[ab].ml" "[ab].ml" true;
    match_test "[ab].ml" "a.ml" false;
    match_test "a\\*.ml" "a\\b.ml" true;
    match_test "a\\*.ml" "a*.ml" false;
  ]

(** Cases where naive backtracking would be exponentially slow. *)
let pathological_tests =
  let open Alcotest in
  let pathological_test pattern path expected =
    test_case
      (Printf.sprintf "%S against %d characters" pattern (String.length path))
      `Quick
      (fun () ->
         (check bool)
           "matches"
           expected
           (matches pattern path))
  in
  let a n = String.make n 'a' in [
    pathological_test "*a*a*a*a*a*a*a*a*b" (a 200) false;
    pathological_test "*a*a*a*a*a*a*a*a*b" (a 200 ^ "b") true;
    pathological_test
      (String.concat "/" (List.init 8 (fun _ -> "**/a") @ ["b"]))
      (String.concat "/" (List.init 200 (fun _ -> "a")))
      false;
  ]

(** Check that a pattern without wildcards matches the path it names. *)
let literal_matches_itself_test =
  Util.property_test
    ~name:"a literal pattern matches itself"
    ~print:QCheck2.Print.string
    Generators.literal_pattern
    (fun pat -> matches pat pat)

(** Check that [p/**] matches every path inside [p] and not [p] itself. *)
let trailing_globstar_test =
  Util.property_test
    ~name:"p/** matches everything inside p and not p itself"
    ~print:QCheck2.Print.(tup2 string string)
    QCheck2.Gen.(tup2 Generators.literal_pattern Generators.path)
    (fun (base, inside) ->
       let pat = base ^ "/**" in
       matches pat (base ^ "/" ^ inside) && not (matches pat base))

(** Check that ["**"] matches every path. *)
let globstar_matches_everything_test =
  Util.property_test
    ~name:"** matches every path"
    ~print:QCheck2.Print.string
    Generators.path
    (fun path -> matches "**" path)

(** Check that a pattern matches the paths built from it by
    [Generators.instance]. *)
let instances_match_test =
  Util.property_test
    ~name:"a pattern matches its instances"
    ~print:QCheck2.Print.(tup2 string string)
    QCheck2.Gen.(Generators.pattern >>= fun pat ->
                 Generators.instance pat >|= fun inst ->
                 (pat, inst))
    (fun (pat, path) -> matches pat path)

(** Check that [Glob.matches] and the reference matcher agree, on random
    pairs and on the hard pairs [Generators.pattern_and_path] builds. *)
let agrees_with_reference_test =
  Util.property_test
    ~name:"matches agrees with the naive reference"
    ~print:QCheck2.Print.(tup2 string string)
    Generators.pattern_and_path
    (fun (pat, path) -> matches pat path = Reference.matches pat path)


(** {1 specificity} *)

(** Test cases for [Glob.specificity]; each pair is checked both ways. *)
let specificity_tests =
  let open Alcotest in
  let specificity_testable =
    testable
      (fun fmt answer ->
         Format.pp_print_string fmt (specificity_to_string answer))
      (=)
  in
  (* [check_specificity expected p q] checks [specificity p q] against
     [expected], and [specificity q p] against its mirror. *)
  let check_specificity expected p q =
    (check specificity_testable)
      (Printf.sprintf "specificity %S %S" p q)
      expected
      (specificity p q);
    (check specificity_testable)
      (Printf.sprintf "specificity %S %S" q p)
      (mirror_specificity expected)
      (specificity q p)
  in
  (* The rows of the table: [includes_test p q] expects [p] to include [q]
     strictly, [cross_cutting_test p q] expects [p] to win as the cross-cutting
     one, [ambiguous_test p q] expects neither to win. *)
  let includes_test p q =
    test_case
      (Printf.sprintf "%S includes %S" p q)
      `Quick
      (fun () ->
         check_specificity (Glob.Less_specific Glob.Included) p q)
  and cross_cutting_test p q =
    test_case
      (Printf.sprintf "%S is cross-cutting, %S is not" p q)
      `Quick
      (fun () ->
         check_specificity (Glob.More_specific Glob.Cross_cutting) p q)
  and ambiguous_test p q =
    test_case
      (Printf.sprintf "%S and %S are ambiguous" p q)
      `Quick
      (fun () ->
         check_specificity Glob.Ambiguous p q)
  in [
    (* A directory includes what is under it; "*" in a segment includes a
       name in its place; "**" includes everything. *)
    includes_test "src/**" "src/core/**";
    includes_test "src/api/**" "src/api/v1/**";
    includes_test "src/**" "src/*";
    includes_test "src/plugins/**" "src/plugins/*/**";
    includes_test "packages/*/src/**" "packages/billing/src/**";
    includes_test "apps/*/**" "apps/web/legacy/**";
    includes_test "**" "src/**";
    includes_test "**" "a.py";

    (* A pattern without wildcards names one path, so every pattern that
       matches that path includes it. *)
    includes_test "lib/*.ml" "lib/glob.ml";
    includes_test "src/*" "src/a.py";

    (* A cross-cutting pattern includes the same name under a fixed
       directory. *)
    includes_test "**/migrations/**" "src/billing/migrations/**";
    includes_test "**/core/**" "src/core/**";
    includes_test "**/__tests__/**" "src/ui/hooks/__tests__/**";
    includes_test "**/__tests__/**" "src/ui/hooks/**/__tests__/**";

    (* A cross-cutting pattern includes a cross-cutting pattern that asks
       for more: a longer suffix, a subdirectory, or a second name. *)
    includes_test "**/*.ts" "**/*.test.ts";
    includes_test "**/*.test.ts" "**/*.integration.test.ts";
    includes_test "**/__tests__/**" "**/__tests__/fixtures/**";
    includes_test "**/__tests__/**" "**/__tests__/**/*.test.ts";
    includes_test "**/*.test.ts" "**/__tests__/**/*.test.ts";

    (* A directory includes a cross-cutting pattern under it. *)
    includes_test "src/**" "src/**/*.py";

    (* Inside a segment, "*" stands for any characters, so it includes a
       literal, a star, or both in its place, wherever the literals of the
       two patterns start and end. *)
    includes_test "a*" "ab*";
    includes_test "*a*" "a";
    includes_test "*a*" "*a";
    includes_test "*.py" "test_*.py";
    includes_test "*_*" "*_*_*";

    (* A pattern that requires fewer segments in a place includes one that
       requires more there. *)
    includes_test "a/**" "a/*/**";
    includes_test "src/**/*.py" "src/*/*.py";
    includes_test "**/*" "*/**";
    includes_test "**/x" "*/**/x";

    (* When neither includes the other, the cross-cutting one is the more
       specific. *)
    cross_cutting_test "**/__tests__/**" "src/ui/hooks/**";
    cross_cutting_test "**/*.test.ts" "src/**";
    cross_cutting_test "**/migrations/**" "src/db/**";
    cross_cutting_test "*/migrations/**" "billing/**";
    cross_cutting_test "packages/*/src/**" "packages/billing/**";
    cross_cutting_test "src/*/models/**" "src/billing/**";
    cross_cutting_test "src/**/generated/**" "src/api/**";
    cross_cutting_test "**/*.pb.go" "internal/api/**";

    (* When neither includes the other and both or neither are cross-cutting,
       the pair is ambiguous. *)
    ambiguous_test "**/__tests__/**" "**/*.test.ts";
    ambiguous_test "src/test_*.py" "src/*_test.py";
    ambiguous_test "a/*/*" "a/b/**";
    ambiguous_test "cmd/*/**" "internal/**";

    (* Two spellings of one pattern match the same paths, so neither is more
       specific. *)
    ambiguous_test "src/**" "src/**/**";
    ambiguous_test "*/**" "**/*/*";
    ambiguous_test "**/*/**" "*/**";
    ambiguous_test "a/**/*/**/b" "a/*/**/b";

    (* A trailing "**" does not match the directory itself, so "src/**" and
       "src" match different paths. *)
    ambiguous_test "src/**" "src";
  ]

(** Check that a pattern is ambiguous with itself. *)
let reflexive_test =
  Util.property_test
    ~name:"a pattern is ambiguous with itself"
    ~print:QCheck2.Print.string
    Generators.pattern
    (fun p -> specificity p p = Glob.Ambiguous)

(** Check that [specificity q p] is the mirror of [specificity p q]. *)
let mirror_test =
  Util.property_test
    ~name:"specificity q p mirrors specificity p q"
    ~print:QCheck2.Print.(tup2 string string)
    Generators.pattern_pair
    (fun (p, q) -> specificity q p = mirror_specificity (specificity p q))

(** Check that inclusion is sound: when [p] includes [q], [p] matches the paths
    that [q] matches. *)
let sound_test =
  Util.property_test
    ~name:"an including pattern matches the instances of the included one"
    ~print:QCheck2.Print.(tup3 string string string)
    QCheck2.Gen.(Generators.pattern_pair >>= fun (p, q) ->
                 Generators.instance q >|= fun path ->
                 (p, q, path))
    (fun (p, q, path) ->
       specificity p q <> Glob.Less_specific Glob.Included || matches p path)

(** Check that inclusion is recognized for every pattern built by loosening
    another.  The answer must be one of these:

    - the loosened pattern includes the original;
    - the two are ambiguous, which is right only when they match the same paths,
      so a random path matched by the loosened pattern must be matched by the
      original too. *)
let generalization_test =
  Util.property_test
    ~name:"a pattern is included in its generalizations"
    ~print:QCheck2.Print.(tup3 string string string)
    QCheck2.Gen.(Generators.pattern >>= fun q ->
                 Generators.generalize q >>= fun p ->
                 Generators.instance p >|= fun path ->
                 (p, q, path))
    (fun (p, q, path) ->
       match specificity p q with
       | Glob.Less_specific Glob.Included -> true
       | Glob.Ambiguous                   -> matches q path
       | _                                -> false)

(** Check that ["**"] includes every pattern, except one that also matches
    every path. *)
let globstar_includes_everything_test =
  Util.property_test
    ~name:"** includes every pattern"
    ~print:QCheck2.Print.(tup2 string string)
    QCheck2.Gen.(tup2 Generators.pattern Generators.path)
    (fun (p, path) ->
       match specificity p "**" with
       | Glob.More_specific Glob.Included -> true
       | Glob.Ambiguous                   -> matches p path
       | _                                -> false)

(** Check inclusion exactly on literal patterns: a literal [q] is included
    in [p] iff [p] matches the one path [q] names, and is ambiguous with [p]
    only if [p] is [q] itself. *)
let literal_test =
  Util.property_test
    ~name:"a literal pattern is included in the patterns that match it"
    ~print:QCheck2.Print.(tup2 string string)
    QCheck2.Gen.(oneof [
        tup2 Generators.pattern Generators.literal_pattern;
        Generators.pattern >>= fun p ->
        Generators.instance p >|= fun q ->
        (p, q);
      ])
    (fun (p, q) ->
       let included =
         match specificity p q with
         | Glob.Less_specific Glob.Included -> true
         | Glob.Ambiguous                   -> p = q
         | _                                -> false
       in
       included = matches p q)


(** {1 Inclusion against the sets of paths} *)

(** Check the inclusion question against the sets of matched paths, over every
    pattern of up to three segments built from a few segment shapes and every
    path of up to five segments built from a few names.

    - For every pattern, the set of paths it matches is computed once.
    - For every ordered pair of patterns, the answer of [specificity] is checked
      against their two sets: it must be [Included] exactly when one set is a
      strict subset of the other, [Ambiguous] when the sets are equal, and
      anything but [Included] when neither set includes the other. *)
let inclusion_against_paths_test =
  Alcotest.test_case
    "specificity agrees with the sets of matched paths"
    `Quick
    (fun () ->
       (* [sequences n items] is every list of [n] elements of [items]. *)
       let rec sequences n items =
         if n = 0 then
           [[]]
         else
           List.concat_map
             (fun x ->
                List.map
                  (fun l -> x :: l)
                  (sequences (n - 1) items))
             items
       in

       (* [joined max items] is every non-empty list of up to [max] elements
          of [items], joined by '/': every pattern or path that can be made
          of them. *)
       let joined max items =
         List.init max (fun n -> sequences (n + 1) items)
         |> List.concat
         |> List.map (String.concat "/")
       in

       let paths = List.map path_exn (joined 5 ["a"; "b"; "ab"; "ba"]) in

       (* Every pattern with its source and, for every path, whether it
          matches it. *)
       let table =
         List.map
           (fun source ->
              let pattern = parse_exn source in
              let matched =
                paths |> List.map (Glob.matches pattern) |> Array.of_list
              in
              (source, pattern, matched))
           (joined 3 ["a"; "b"; "ab"; "*"; "a*"; "*a"; "*b"; "**"])
       in

       (* [subset a b] is whether every path matched in [a] is matched in
          [b]. *)
       let subset a b = Array.for_all2 (fun a b -> not a || b) a b in

       table |> List.iter
         (fun (ps, p, mp) ->
            table |> List.iter
              (fun (qs, q, mq) ->
                 let answer = Glob.specificity p q in
                 let agrees =
                   match subset mp mq, subset mq mp, answer with
                   | true,  false, Glob.More_specific Glob.Included -> true
                   | false, true,  Glob.Less_specific Glob.Included -> true
                   | true,  true,  Glob.Ambiguous                   -> true
                   | false, false, Glob.Ambiguous                   -> true
                   | false, false, Glob.More_specific Glob.Cross_cutting
                   | false, false, Glob.Less_specific Glob.Cross_cutting -> true
                   | _                                              -> false
                 in
                 if not agrees then
                   Alcotest.failf "specificity %S %S = %s, but %s"
                     ps qs (specificity_to_string answer)
                     begin match subset mp mq, subset mq mp with
                       | true,  false -> "the paths of p are a strict subset"
                       | false, true  -> "the paths of q are a strict subset"
                       | true,  true  -> "they match the same paths"
                       | false, false -> "neither includes the other"
                     end)))


(** {1 Test runner} *)

let () =
  Alcotest.run ~compact:true "Glob"
    [
      ("parse errors", parse_error_tests);
      ("round-trip", [round_trip_test]);
      ("matches", match_tests);
      ("pathological", pathological_tests);
      ("literal matches itself", [literal_matches_itself_test]);
      ("trailing globstar", [trailing_globstar_test]);
      ("globstar matches everything", [globstar_matches_everything_test]);
      ("instances match", [instances_match_test]);
      ("agrees with reference", [agrees_with_reference_test]);
      ("specificity", specificity_tests);
      ("reflexive", [reflexive_test]);
      ("mirror", [mirror_test]);
      ("sound", [sound_test]);
      ("generalizations", [generalization_test]);
      ("globstar includes everything", [globstar_includes_everything_test]);
      ("literal", [literal_test]);
      ("inclusion against paths", [inclusion_against_paths_test]);
    ]
