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

(** A naive matcher, straight from the specification in [glob.mli], to check
    the real one against. *)
module Reference = struct
  (** [match_pieces pieces s] is [true] iff the ["*"]-separated literals
      [pieces] describe the whole segment [s]. *)
  let rec match_pieces pieces s =
    match pieces with
    | []          -> s = ""
    | [lit]       -> lit = s
    | lit :: rest ->
      String.starts_with ~prefix:lit s
      &&
      let k = String.length lit in
      let s = String.sub s k (String.length s - k) in
      (* The star takes 0, 1, 2, ... characters. *)
      let rec try_from i =
        i <= String.length s
        &&
        begin
          match_pieces rest (String.sub s i (String.length s - i))
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
    | seg :: rest, s :: path_rest ->
      match_pieces (String.split_on_char '*' seg) s
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

(** Test cases for [Glob.Specificity.compare]: [winner] and [loser] both match
    [path], and [winner] is the more specific one. *)
let specificity_tests =
  let open Alcotest in
  (* [both_match a b path] checks that the comparison is meaningful: both
     patterns match [path]. *)
  let both_match a b path =
    List.iter
      (fun p ->
         (check bool)
           (Printf.sprintf "%S matches" p)
           true
           (matches p path))
      [a; b]
  in
  let beats winner loser path =
    test_case
      (Printf.sprintf "%S beats %S for %S" winner loser path)
      `Quick
      (fun () ->
         both_match winner loser path;
         let sw = Glob.specificity (parse_exn winner)
         and sl = Glob.specificity (parse_exn loser) in
         if Glob.Specificity.compare sw sl <= 0 then
           failf "%S (%s) should beat %S (%s)"
             winner (Glob.Specificity.to_string sw)
             loser (Glob.Specificity.to_string sl))
  and ties a b path =
    test_case
      (Printf.sprintf "%S ties with %S for %S" a b path)
      `Quick
      (fun () ->
         both_match a b path;
         (check int)
           "Specificity.compare"
           0
           (Glob.Specificity.compare
              (Glob.specificity (parse_exn a))
              (Glob.specificity (parse_exn b))))
  in [
    beats "src/core/api.py" "src/**/core/api.py" "src/core/api.py";
    beats "src/core/api.py" "src/core/*" "src/core/api.py";
    beats "src/core/**" "src/**" "src/core/api.py";
    beats "src/core/*.py" "src/core/**" "src/core/api.py";
    beats "src/plugins/core/**" "src/plugins/*/**" "src/plugins/core/x.py";
    beats "src/**/api.py" "src/core/**" "src/core/api.py";
    beats "**/*.test.ts" "src/ui/hooks/**" "src/ui/hooks/a.test.ts";
    beats "**/__tests__/**" "src/ui/**" "src/ui/__tests__/a.ts";
    beats "**/*.py" "src/core/internal/**" "src/core/internal/x.py";
    beats "src/**/*.test.ts" "**/*.test.ts" "src/a.test.ts";
    beats "**/*.test.ts" "**/*.ts" "a.test.ts";
    beats "src/*/*.test.ts" "**/*.test.ts" "src/a/b.test.ts";
    beats "src/ui/*.test.ts" "**/*.test.ts" "src/ui/a.test.ts";
    beats "src/*" "src/**" "src/a.py";
    beats "src/core/*_test.py" "src/core/test_*.py" "src/core/test_a_test.py";
    ties "src/**" "src/**/**" "src/a.py";
    ties "src/core/*a*.py" "src/core/*b*.py" "src/core/ab.py";
  ]


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
    ]
