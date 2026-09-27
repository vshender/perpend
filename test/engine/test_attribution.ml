(** Tests for [Attribution]. *)

open Perpend_core
open Perpend_manifest
open Perpend_engine
open Attribution


(** {1 Test helpers} *)

(** [pattern s] is the pattern [s]. *)
let pattern s =
  Result.get_ok (Glob.of_string s)

(** [id s] is the module id [s]. *)
let id s =
  Result.get_ok (Manifest.Id.of_string s)

(** [path s] is the path [s]. *)
let path s =
  Result.get_ok (Path.of_string s)

(** [attribution lines] is the attribution over the manifest with the YAML
    [lines]. *)
let attribution lines =
  let text = lines |> List.map (fun l -> l ^ "\n") |> String.concat "" in
  match Manifest.of_string text with
  | Ok manifest -> create manifest
  | Error e     -> failwith (Manifest.error_to_string e)

(** The lines of the manifest for most cases: nested places, a place with a
    wildcard, cross-cutting patterns, and a pattern with the text of a
    package. *)
let example_lines = [
  "version: 1";
  "modules:";
  "  core:    {paths: ['src/core/**']}";
  "  db:      {paths: ['src/core/db/**']}";
  "  plugins: {paths: ['src/plugins/*/**']}";
  "  tests:   {paths: ['**/__tests__/**', '**/*.test.ts']}";
  "  styles:  {paths: ['**/*.css']}";
  "  vendor:  {paths: [sqlalchemy]}";
  "  orm:     {external: [sqlalchemy, drizzle-orm]}";
  "  http:    {external: [httpx]}";
]

(** The attribution over [example_lines]. *)
let example =
  attribution example_lines

(** [example] plus a module for the overlap of [tests] and [styles]. *)
let resolved =
  attribution
    (example_lines @ ["  test_styles: {paths: ['**/__tests__/**/*.css']}"])

(** An attribution where a beaten pattern still beats a third one on [src/a/f]:
    [a] beats [b] by inclusion, [b] beats [c] as cross-cutting, and [a] and [c]
    are ambiguous. *)
let chain =
  attribution [
    "version: 1";
    "modules:";
    "  a: {paths: ['src/a/**']}";
    "  b: {paths: ['**/a/**']}";
    "  c: {paths: ['src/*/*']}";
  ]

(** [chain] with [**/a/**] and [src/*/*] in one module, [b].  The winner on
    [src/a/f] depends on the first beating the second. *)
let chain_in_two =
  attribution [
    "version: 1";
    "modules:";
    "  a: {paths: ['src/a/**']}";
    "  b: {paths: ['**/a/**', 'src/*/*']}";
  ]

(** An attribution where two patterns of [a] and one of [b] are ambiguous with
    each other on [x/y/f.ts]: all three are cross-cutting and none includes
    another. *)
let crossing =
  attribution [
    "version: 1";
    "modules:";
    "  a: {paths: ['**/x/**', '**/*.ts']}";
    "  b: {paths: ['**/y/**']}";
  ]

(** An attribution with two patterns that match the same paths. *)
let twins =
  attribution [
    "version: 1";
    "modules:";
    "  a: {paths: ['*/**']}";
    "  b: {paths: ['**/*/*']}";
  ]

(** An attribution whose patterns beat each other in a circle on [src/main.py]:
    [a] beats [b] by inclusion, [b] beats [c] as cross-cutting, [c] beats [d]
    by inclusion, and [d] beats [a] as cross-cutting. *)
let circle =
  attribution [
    "version: 1";
    "modules:";
    "  a: {paths: ['src/**']}";
    "  b: {paths: ['**/src/**']}";
    "  c: {paths: ['src*/*']}";
    "  d: {paths: ['*src*/*']}";
  ]

(** [circle] plus a pattern that no pattern of the circle beats. *)
let circle_and_winner =
  attribution [
    "version: 1";
    "modules:";
    "  a: {paths: ['src/**']}";
    "  b: {paths: ['**/src/**']}";
    "  c: {paths: ['src*/*']}";
    "  d: {paths: ['*src*/*']}";
    "  e: {paths: ['**/main.py']}";
  ]

(** The patterns of [circle] in one module. *)
let circle_in_one =
  attribution [
    "version: 1";
    "modules:";
    "  a: {paths: ['src/**', '**/src/**', 'src*/*', '*src*/*']}";
  ]

(** [pp_verdict fmt v] prints [v] for a failed check. *)
let pp_verdict fmt = function
  | Module id    -> Format.fprintf fmt "Module %s" (Manifest.Id.to_string id)
  | Unattributed -> Format.pp_print_string fmt "Unattributed"
  | Ambiguous candidates ->
    let candidate (p, id) =
      Glob.to_string p ^ ": " ^ Manifest.Id.to_string id
    in
    Format.fprintf fmt "Ambiguous [%s]"
      (String.concat "; " (List.map candidate candidates))

(** [verdict_equal a b] is [true] iff [a] and [b] are the same verdict. *)
let verdict_equal a b =
  let candidate_equal (p, id) (q, id') =
    Glob.equal p q && Manifest.Id.equal id id'
  in
  match a, b with
  | Module id, Module id'      -> Manifest.Id.equal id id'
  | Unattributed, Unattributed -> true
  | Ambiguous xs, Ambiguous ys -> List.equal candidate_equal xs ys
  | _                          -> false

(** [Attribution.verdict] for Alcotest. *)
let verdict_testable =
  Alcotest.testable pp_verdict verdict_equal

(** [Manifest.Id.t] for Alcotest. *)
let id_testable =
  Alcotest.testable
    (fun fmt id -> Format.pp_print_string fmt (Manifest.Id.to_string id))
    Manifest.Id.equal


(** {1 Files} *)

(** Test cases for [Attribution.of_path]: each names the attribution, the
    path and the expected verdict. *)
let of_path_tests =
  let open Alcotest in
  let path_test name t s expected =
    test_case
      name
      `Quick
      (fun () ->
         check verdict_testable
           s
           expected (of_path t (path s)))
  in
  (* [ambiguous candidates] is [Ambiguous] with [candidates], each a pattern and
     a module id as strings. *)
  let ambiguous candidates =
    Ambiguous (List.map (fun (p, m) -> (pattern p, id m)) candidates)
  in [
    path_test "one matching pattern"
      example "src/core/a.ts"
      (Module (id "core"));
    path_test "the included pattern beats the one that includes it"
      example "src/core/db/a.ts"
      (Module (id "db"));
    path_test "a cross-cutting pattern beats a place"
      example "src/core/__tests__/a.ts"
      (Module (id "tests"));
    path_test "a cross-cutting pattern beats a place with a wildcard"
      example "src/plugins/x/a.test.ts"
      (Module (id "tests"));
    path_test "a cross-cutting pattern beats a nested place"
      example "src/core/db/__tests__/a.ts"
      (Module (id "tests"));
    path_test "two unbeaten patterns of one module"
      example "src/core/__tests__/a.test.ts"
      (Module (id "tests"));
    path_test "two unbeaten patterns of different modules"
      example "src/core/__tests__/a.css"
      (ambiguous [("**/__tests__/**", "tests"); ("**/*.css", "styles")]);
    path_test "a pattern for the overlap settles it"
      resolved "src/core/__tests__/a.css"
      (Module (id "test_styles"));
    path_test "a beaten pattern still beats a third one"
      chain "src/a/f"
      (Module (id "a"));
    path_test "patterns of one module still beat each other"
      chain_in_two "src/a/f"
      (Module (id "a"));
    path_test "two candidates of one module and one of another"
      crossing "x/y/f.ts"
      (ambiguous [("**/x/**", "a"); ("**/*.ts", "a"); ("**/y/**", "b")]);
    path_test "two patterns that match the same paths"
      twins "src/main.py"
      (ambiguous [("*/**", "a"); ("**/*/*", "b")]);
    path_test "every matching pattern is beaten"
      circle "src/main.py"
      (ambiguous
         [
           ("src/**", "a");
           ("**/src/**", "b");
           ("src*/*", "c");
           ("*src*/*", "d");
         ]);
    path_test "every matching pattern is beaten, all of one module"
      circle_in_one "src/main.py"
      (Module (id "a"));
    path_test "a circle and an unbeaten pattern"
      circle_and_winner "src/main.py"
      (Module (id "e"));
    path_test "a subset of the circle has a winner"
      circle "src/x/main.py"
      (Module (id "a"));
    path_test "no matching pattern"
      example "README.md"
      Unattributed;
    path_test "a pattern with the text of a package"
      example "sqlalchemy"
      (Module (id "vendor"));
  ]


(** {1 Packages} *)

(** Test cases for [Attribution.of_package]. *)
let of_package_tests =
  let open Alcotest in
  let package_test name expected =
    test_case
      (Printf.sprintf "%S is %slisted" name
         (if Option.is_some expected then "" else "not "))
      `Quick
      (fun () ->
         check (option id_testable) name
           (Option.map id expected) (of_package example name))
  in [
    package_test "sqlalchemy" (Some "orm");
    package_test "drizzle-orm" (Some "orm");
    package_test "httpx" (Some "http");
    package_test "requests" None;
    package_test "SQLAlchemy" None;
    package_test "src/core/**" None;
  ]


(** {1 Test runner} *)

let () =
  Alcotest.run ~compact:true "Attribution" [
    ("of_path", of_path_tests);
    ("of_package", of_package_tests);
  ]
