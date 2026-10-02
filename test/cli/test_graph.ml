(** Tests for [Graph]. *)

open Perpend_manifest
open Perpend_cli
open Test_helpers.Facts_lines


(** {1 Test helpers} *)

(** [id s] is the module id [s]. *)
let id s =
  Result.get_ok (Manifest.Id.of_string s)

(** The manifest for every case:

    - three modules with code;
    - an overlap of two cross-cutting patterns;
    - the module [infra] and two modules under it;
    - the module [infrastructure], whose id starts with the letters of [infra]
      but is not under it;
    - an external module. *)
let manifest =
  let text =
    String.concat "\n" [
      "version: 1";
      "modules:";
      "  api:            {paths: ['src/api/**']}";
      "  core:           {paths: ['src/core/**']}";
      "  ui:             {paths: ['src/ui/**']}";
      "  tests:          {paths: ['**/tests/**']}";
      "  styles:         {paths: ['**/*.css']}";
      "  infra:          {paths: ['infra/**']}";
      "  infra/build:    {paths: ['Makefile']}";
      "  infra/ci:       {paths: ['ci/**']}";
      "  infrastructure: {paths: ['terraform/**']}";
      "  ext/orm:        {external: [sqlalchemy]}";
    ]
  in
  match Manifest.of_string text with
  | Ok manifest -> manifest
  | Error e     -> failwith (Manifest.error_to_string e)

(** The facts for most cases.  The edges appear in another order than the
    modules have in the manifest, and one edge has two facts.  Coverage has an
    unattributed file, an ambiguous file, two unlisted packages and an
    unresolved import. *)
let example = [
  file_fact 1 "src/ui/a.py" "src/core/b.py";
  file_fact 2 "src/api/a.py" "src/ui/a.py";
  package_fact 3 "src/api/a.py" "sqlalchemy";
  file_fact 4 "src/api/a.py" "src/core/b.py";
  file_fact 5 "src/api/c.py" "src/core/d.py";
  file_fact 6 "src/core/b.py" "src/core/d.py";
  file_fact 7 "ci/run.py" "src/api/a.py";
  file_fact 8 "src/api/a.py" "setup.py";
  file_fact 9 "src/api/a.py" "src/ui/tests/a.css";
  package_fact 10 "src/core/b.py" "requests";
  package_fact 11 "src/core/b.py" "httpx";
  unresolved 12 "src/core/b.py";
]

(** [report hide lines] is the report for the graph of the JSONL [lines] over
    [manifest], without the modules that the ids [hide] hide. *)
let report hide lines =
  Graph.report
    ~hide:(List.map id hide)
    (Perpend_engine.Graph.create manifest (facts lines))


(** {1 Reports} *)

(** Test cases for [Graph.report]: each names the hidden ids, the facts and the
    expected text, given as lines. *)
let report_tests =
  let open Alcotest in
  let report_test name hide lines expected =
    test_case
      name
      `Quick
      (fun () ->
         check (result string string) "report"
           (Ok (String.concat "\n" expected ^ "\n"))
           (report hide lines))
  in [
    report_test "no facts: every module, no edge"
      []
      []
      [
        "api";
        "core";
        "ui";
        "tests";
        "styles";
        "infra";
        "infra/build";
        "infra/ci";
        "infrastructure";
        "ext/orm";
        "";
        "unattributed (0 files)";
        "";
        "ambiguous (0 files)";
        "";
        "unlisted packages: 0";
        "unresolved imports: 0";
        "";
        "10 modules, 0 edges, 0 facts";
      ];
    report_test "edges under their sources, in the order of the manifest"
      []
      example
      [
        "api";
        "  -> core (2 facts)";
        "  -> ui (1 fact)";
        "  -> ext/orm (1 fact)";
        "core";
        "ui";
        "  -> core (1 fact)";
        "tests";
        "styles";
        "infra";
        "infra/build";
        "infra/ci";
        "  -> api (1 fact)";
        "infrastructure";
        "ext/orm";
        "";
        "unattributed (1 file)";
        "  setup.py";
        "";
        "ambiguous (1 file)";
        "  src/ui/tests/a.css: **/tests/** (tests), **/*.css (styles)";
        "";
        "unlisted packages: 2";
        "unresolved imports: 1";
        "";
        "10 modules, 5 edges, 6 facts";
      ];
    report_test "a hidden module has no line, and no edge points to it"
      ["core"]
      example
      [
        "api";
        "  -> ui (1 fact)";
        "  -> ext/orm (1 fact)";
        "ui";
        "tests";
        "styles";
        "infra";
        "infra/build";
        "infra/ci";
        "  -> api (1 fact)";
        "infrastructure";
        "ext/orm";
        "";
        "unattributed (1 file)";
        "  setup.py";
        "";
        "ambiguous (1 file)";
        "  src/ui/tests/a.css: **/tests/** (tests), **/*.css (styles)";
        "";
        "unlisted packages: 2";
        "unresolved imports: 1";
        "";
        "9 modules (1 hidden), 3 edges, 3 facts";
      ];
    report_test "an id hides its module and the modules under it, by segment"
      ["infra"]
      example
      [
        "api";
        "  -> core (2 facts)";
        "  -> ui (1 fact)";
        "  -> ext/orm (1 fact)";
        "core";
        "ui";
        "  -> core (1 fact)";
        "tests";
        "styles";
        "infrastructure";
        "ext/orm";
        "";
        "unattributed (1 file)";
        "  setup.py";
        "";
        "ambiguous (1 file)";
        "  src/ui/tests/a.css: **/tests/** (tests), **/*.css (styles)";
        "";
        "unlisted packages: 2";
        "unresolved imports: 1";
        "";
        "7 modules (3 hidden), 4 edges, 5 facts";
      ];
    report_test "several hidden ids"
      ["infra"; "ext"; "tests"; "styles"; "infrastructure"]
      example
      [
        "api";
        "  -> core (2 facts)";
        "  -> ui (1 fact)";
        "core";
        "ui";
        "  -> core (1 fact)";
        "";
        "unattributed (1 file)";
        "  setup.py";
        "";
        "ambiguous (1 file)";
        "  src/ui/tests/a.css: **/tests/** (tests), **/*.css (styles)";
        "";
        "unlisted packages: 2";
        "unresolved imports: 1";
        "";
        "3 modules (7 hidden), 3 edges, 4 facts";
      ];
    report_test "ids that hide the same module count it once"
      ["infra"; "infra/ci"; "infra"]
      example
      [
        "api";
        "  -> core (2 facts)";
        "  -> ui (1 fact)";
        "  -> ext/orm (1 fact)";
        "core";
        "ui";
        "  -> core (1 fact)";
        "tests";
        "styles";
        "infrastructure";
        "ext/orm";
        "";
        "unattributed (1 file)";
        "  setup.py";
        "";
        "ambiguous (1 file)";
        "  src/ui/tests/a.css: **/tests/** (tests), **/*.css (styles)";
        "";
        "unlisted packages: 2";
        "unresolved imports: 1";
        "";
        "7 modules (3 hidden), 4 edges, 5 facts";
      ];
    report_test "every module hidden: the report starts with the files"
      ["api"; "core"; "ui"; "tests"; "styles"; "infra"; "infrastructure"; "ext"]
      example
      [
        "unattributed (1 file)";
        "  setup.py";
        "";
        "ambiguous (1 file)";
        "  src/ui/tests/a.css: **/tests/** (tests), **/*.css (styles)";
        "";
        "unlisted packages: 2";
        "unresolved imports: 1";
        "";
        "0 modules (10 hidden), 0 edges, 0 facts";
      ];
  ]

(** Test cases for the ids that [Graph.report] refuses to hide. *)
let error_tests =
  let open Alcotest in
  let error_test name hide expected =
    test_case
      name
      `Quick
      (fun () ->
         check (result string string) "report"
           (Error expected)
           (report hide example))
  in [
    error_test "an id of no module"
      ["docs"]
      "no module is 'docs' or under it";
    error_test "an id that is only the start of a segment"
      ["inf"]
      "no module is 'inf' or under it";
    error_test "an id under a module"
      ["core/db"]
      "no module is 'core/db' or under it";
    error_test "the first useless id is reported"
      ["core"; "docs"; "inf"]
      "no module is 'docs' or under it";
  ]


(** {1 Test runner} *)

let () =
  Alcotest.run ~compact:true "Graph" [
    ("report", report_tests);
    ("errors", error_tests);
  ]
