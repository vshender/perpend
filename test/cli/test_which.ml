(** Tests for [Which]. *)

open Perpend_core
open Perpend_manifest
open Perpend_cli


(** {1 Test helpers} *)

(** [path s] is the path [s]. *)
let path s =
  Result.get_ok (Path.of_string s)

(** The manifest for every case: a place and two cross-cutting patterns of
    different modules that compete on CSS files under [__tests__]. *)
let manifest =
  let text =
    String.concat "\n" [
      "version: 1";
      "modules:";
      "  core:   {paths: ['src/core/**']}";
      "  tests:  {paths: ['**/__tests__/**']}";
      "  styles: {paths: ['**/*.css']}";
    ]
  in
  match Manifest.of_string text with
  | Ok manifest -> manifest
  | Error e     -> failwith (Manifest.error_to_string e)


(** {1 Reports} *)

(** Test cases for [Which.report]: each names the path and the expected line. *)
let report_tests =
  let open Alcotest in
  let report_test name p expected =
    test_case
      name
      `Quick
      (fun () ->
         check string "report"
           (expected ^ "\n") (Which.report manifest (path p)))
  in [
    report_test "a module"
      "src/core/a.py"
      "core";
    report_test "no matching pattern"
      "README.md"
      "unattributed";
    report_test "an ambiguous file"
      "src/core/__tests__/a.css"
      "ambiguous: **/__tests__/** (tests), **/*.css (styles)";
  ]


(** {1 Test runner} *)

let () =
  Alcotest.run ~compact:true "Which" [
    ("report", report_tests);
  ]
