(** Tests for [Path]. *)

open Perpend_core
open Test_helpers


(** Test cases for [Path.of_string] on valid paths: they parse and split
    into the expected segments. *)
let valid_tests =
  let open Alcotest in
  let valid_test s segments =
    test_case
      (Printf.sprintf "%S is a path" s)
      `Quick
      (fun () ->
         match Path.of_string s with
         | Ok p    -> check (list string) "segments" segments (Path.segments p)
         | Error e -> failf "%S: %s" s e)
  in [
    valid_test "a" ["a"];
    valid_test "src/core/api.py" ["src"; "core"; "api.py"];
    valid_test ".hidden" [".hidden"];
    valid_test "..." ["..."];
    valid_test "a b/c d" ["a b"; "c d"];
    valid_test "a/*/b" ["a"; "*"; "b"];
  ]

(** Test cases for the strings [Path.of_string] rejects. *)
let invalid_tests =
  let open Alcotest in
  let invalid_test s =
    test_case
      (Printf.sprintf "%S is not a path" s)
      `Quick
      (fun () ->
         match Path.of_string s with
         | Error _ -> ()
         | Ok _    -> failf "%S should not parse" s)
  in [
    invalid_test "";
    invalid_test "/";
    invalid_test "/a";
    invalid_test "a/";
    invalid_test "a//b";
    invalid_test ".";
    invalid_test "..";
    invalid_test "./a";
    invalid_test "a/../b";
    invalid_test "a/.";
  ]

(** Check that a path damaged in one of the ways [Path.of_string] rejects
    does not parse. *)
let damaged_path_test =
  Util.property_test
    ~name:"a damaged path does not parse"
    ~print:QCheck2.Print.string
    Generators.invalid_path
    (fun s -> Result.is_error (Path.of_string s))

(** Check that [Path.to_string] gives back the string a path was parsed
    from. *)
let round_trip_test =
  Util.property_test
    ~name:"to_string (of_string s) = s for every valid path s"
    ~print:QCheck2.Print.string
    Generators.path
    (fun s ->
       match Path.of_string s with
       | Ok p    -> Path.to_string p = s
       | Error e -> failwith (Printf.sprintf "%s: %s" s e))

(** Check that the segments joined by ['/'] give back the string a path was
    parsed from. *)
let segments_join_test =
  Util.property_test
    ~name:"concat \"/\" (segments (of_string s)) = s for every valid path s"
    ~print:QCheck2.Print.string
    Generators.path
    (fun s ->
       match Path.of_string s with
       | Ok p    -> String.concat "/" (Path.segments p) = s
       | Error e -> failwith (Printf.sprintf "%s: %s" s e))


let () =
  Alcotest.run ~compact:true "Path"
    [
      ("valid", valid_tests);
      ("invalid", invalid_tests);
      ("damaged path", [damaged_path_test]);
      ("round-trip", [round_trip_test]);
      ("segments join", [segments_join_test]);
    ]
