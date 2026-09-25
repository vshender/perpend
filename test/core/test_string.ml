(** Tests for [String]: the substring search. *)

open Perpend_core


(** Test cases for [find_sub], [rfind_sub] and [contains_sub]: each row gives
    a string, a substring, and the first and last positions of the substring,
    if any. *)
let search_tests =
  let open Alcotest in
  let search_test (s, sub, first, last) =
    test_case
      (Printf.sprintf "%S in %S" sub s)
      `Quick
      (fun () ->
         check (option int) "find_sub" first (String.find_sub s ~sub);
         check (option int) "rfind_sub" last (String.rfind_sub s ~sub);
         check bool "contains_sub"
           (Option.is_some first) (String.contains_sub s ~sub))
  in
  List.map search_test [
    ("abc", "b", Some 1, Some 1);
    ("abcabc", "bc", Some 1, Some 4);
    ("aaa", "aa", Some 0, Some 1);
    ("abc", "abc", Some 0, Some 0);
    ("abc", "abcd", None, None);
    ("abc", "x", None, None);
    ("", "a", None, None);
    ("abc", "", Some 0, Some 3);
    ("", "", Some 0, Some 0);
    ("a character character 0", " character ", Some 1, Some 11);
  ]


let () =
  Alcotest.run "String" [
    ("search", search_tests);
  ]
