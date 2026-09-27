(** Tests for [Files]. *)

open Perpend_core
open Perpend_manifest
open Perpend_cli


(** {1 Test helpers} *)

(** [path s] is the path [s]. *)
let path s =
  Result.get_ok (Path.of_string s)

(** The manifest for every case: a place, a nested place, two cross-cutting
    patterns of different modules that compete on CSS files under [__tests__],
    and an external module, which gets no section. *)
let manifest =
  let text =
    String.concat "\n" [
      "version: 1";
      "modules:";
      "  core:   {paths: ['src/core/**']}";
      "  db:     {paths: ['src/core/db/**']}";
      "  tests:  {paths: ['**/__tests__/**']}";
      "  styles: {paths: ['**/*.css']}";
      "  orm:    {external: [sqlalchemy]}";
    ]
  in
  match Manifest.of_string text with
  | Ok manifest -> manifest
  | Error e     -> failwith (Manifest.error_to_string e)


(** {1 Reports} *)

(** Test cases for [Files.report]: each names the paths and the expected text,
    given as lines. *)
let report_tests =
  let open Alcotest in
  let report_test name paths expected =
    test_case
      name
      `Quick
      (fun () ->
         check string "report"
           (String.concat "\n" expected ^ "\n")
           (Files.report manifest (List.map path paths)))
  in [
    report_test "no file: every section is empty"
      []
      [
        "core (0 files)";
        "";
        "db (0 files)";
        "";
        "tests (0 files)";
        "";
        "styles (0 files)";
        "";
        "unattributed (0 files)";
        "";
        "ambiguous (0 files)";
        "";
        "0 files, 0 unattributed, 0 ambiguous";
      ];
    report_test "files of every kind, in the order given"
      [
        "src/core/db/a.py";
        "README.md";
        "src/core/__tests__/a.css";
        "src/core/b.py";
        "src/core/a.py";
        "src/__tests__/b.css";
      ]
      [
        "core (2 files)";
        "  src/core/b.py";
        "  src/core/a.py";
        "";
        "db (1 file)";
        "  src/core/db/a.py";
        "";
        "tests (0 files)";
        "";
        "styles (0 files)";
        "";
        "unattributed (1 file)";
        "  README.md";
        "";
        "ambiguous (2 files)";
        "  src/core/__tests__/a.css: **/__tests__/** (tests), **/*.css (styles)";
        "  src/__tests__/b.css: **/__tests__/** (tests), **/*.css (styles)";
        "";
        "6 files, 1 unattributed, 2 ambiguous";
      ];
  ]


(** {1 Test runner} *)

let () =
  Alcotest.run ~compact:true "Files" [
    ("report", report_tests);
  ]
