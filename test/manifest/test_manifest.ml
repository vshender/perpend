(** Tests for [Manifest]. *)

open Perpend_core
open Perpend_manifest
open Manifest


(** {1 Test helpers} *)

(** [pattern s] is the pattern [s]. *)
let pattern s =
  Result.get_ok (Glob.parse s)

(** [id s] is the module id [s]. *)
let id s =
  Result.get_ok (Id.of_string s)

(** [text lines] is the YAML text with [lines], each ended by a newline. *)
let text lines =
  lines |> List.map (fun l -> l ^ "\n") |> String.concat ""

(** [utf16le s] is the ASCII text [s] encoded as UTF-16LE. *)
let utf16le s =
  String.to_seq s
  |> Seq.map (fun c -> String.make 1 c ^ "\000")
  |> List.of_seq
  |> String.concat ""

(** The lines of a manifest with a module of every kind, in block style. *)
let example = [
  "version: 1";
  "modules:";
  "  core:";
  "    paths:";
  "      - \"src/core/**\"";
  "  core/api:";
  "    paths:";
  "      - \"src/core/api/**\"";
  "      - \"src/core/api.py\"";
  "  ext.orm:";
  "    external:";
  "      - sqlalchemy";
  "      - drizzle-orm";
]

(** What [example] reads to. *)
let example_value = {
  modules = [
    {
      id = id "core";
      contents = Paths [pattern "src/core/**"];
    };
    {
      id = id "core/api";
      contents = Paths [pattern "src/core/api/**"; pattern "src/core/api.py"];
    };
    {
      id = id "ext.orm";
      contents = Packages ["sqlalchemy"; "drizzle-orm"];
    };
  ];
}

(** [pp_manifest fmt t] prints [t] for a failed check. *)
let pp_manifest fmt t =
  t.modules |> List.iter
    (fun (m : Module.t) ->
       let contents =
         match m.contents with
         | Paths ps ->
           "paths: " ^ String.concat ", " (List.map Glob.to_string ps)
         | Packages ns ->
           "external: " ^ String.concat ", " ns
       in
       Format.fprintf fmt "%s: {%s}@\n" (Id.to_string m.id) contents)

(** [Manifest.t] for Alcotest. *)
let manifest_testable =
  Alcotest.testable pp_manifest ( = )

(** [Manifest.error] for Alcotest. *)
let error_testable =
  Alcotest.testable
    (fun fmt e -> Format.pp_print_string fmt (Manifest.error_to_string e))
    ( = )


(** {1 Ids} *)

(** Test cases for [Id]: which strings are ids, kept as written. *)
let id_tests =
  let open Alcotest in
  let valid_test s =
    test_case
      (Printf.sprintf "%S is an id" s)
      `Quick
      (fun () ->
         match Id.of_string s with
         | Ok id   -> check string "to_string" s (Id.to_string id)
         | Error e -> fail e)
  and invalid_test s part =
    test_case
      (Printf.sprintf "%S is not an id" s)
      `Quick
      (fun () ->
         match Id.of_string s with
         | Ok _    -> fail "should not parse"
         | Error e ->
           if not (String.contains_sub e ~sub:part) then
             failf "%S not in %S" part e)
  in [
    valid_test "core";
    valid_test "core/api";
    valid_test "ext.orm";
    valid_test "ui-kit_2";
    valid_test "my module";
    valid_test "core/./api";
    invalid_test "" "empty segment";
    invalid_test "/core" "empty segment";
    invalid_test "core/" "empty segment";
    invalid_test "core//api" "empty segment";
    invalid_test "core/*" "'*' is not allowed";
    invalid_test "a\001b" "control characters are not allowed";
    invalid_test "a\127b" "control characters are not allowed";
  ]

(** Test cases for [Id.equal]. *)
let id_equal_tests =
  let open Alcotest in
  let equal_test a b expected =
    test_case
      (Printf.sprintf "%S and %S" a b)
      `Quick
      (fun () -> check bool "equal" expected (Id.equal (id a) (id b)))
  in [
    equal_test "core" "core" true;
    equal_test "core" "util" false;
    equal_test "core" "core/api" false;
    equal_test "core/api" "core/api" true;
    equal_test "core/api" "core/impl" false;
  ]


(** {1 Reading} *)

(** Test cases for the texts that [Manifest.of_string] accepts: some checked
    against their value, the others for acceptance only. *)
let of_string_tests =
  let open Alcotest in
  let reads_test name lines expected =
    test_case
      name
      `Quick
      (fun () ->
         check (result manifest_testable error_testable)
           "of_string"
           (Ok expected) (Manifest.of_string (text lines)))
  in
  let valid_test name s =
    test_case
      name
      `Quick
      (fun () ->
         match Manifest.of_string s with
         | Ok _    -> ()
         | Error e -> fail (Manifest.error_to_string e))
  in
  (* [manifest_of_pattern s] is the manifest with one module, [m], whose only
     pattern is [s]. *)
  let manifest_of_pattern s =
    { modules = [{ id = id "m"; contents = Paths [pattern s] }] }
  in [
    reads_test "the example reads to its value" example example_value;
    reads_test "flow style reads to the same value"
      [
        "version: 1";
        "modules:";
        "  core:     {paths: [\"src/core/**\"]}";
        "  core/api: {paths: [\"src/core/api/**\", \"src/core/api.py\"]}";
        "  ext.orm:  {external: [sqlalchemy, drizzle-orm]}";
      ]
      example_value;
    reads_test "JSON reads to the same value"
      [
        {|{"version": 1, "modules": {"core": {"paths": ["src/core/**"]}, "core/api": {"paths": ["src/core/api/**", "src/core/api.py"]}, "ext.orm": {"external": ["sqlalchemy", "drizzle-orm"]}}}|};
      ]
      example_value;
    reads_test "key order does not matter"
      [
        "modules: {m: {paths: [\"src/**\"]}}";
        "version: 1";
      ]
      (manifest_of_pattern "src/**");
    reads_test "a quoted pattern may start with a star"
      [
        "version: 1";
        "modules: {m: {paths: ['**/*.test.ts']}}";
      ]
      (manifest_of_pattern "**/*.test.ts");
    reads_test "a plain scalar is text"
      [
        "version: 1";
        "modules: {m: {paths: [yes]}}";
      ]
      (manifest_of_pattern "yes");
    reads_test "a plain key is text"
      [
        "version: 1";
        "modules: {~: {paths: [x]}}";
      ]
      { modules = [{ id = id "~"; contents = Paths [pattern "x"] }] };
    reads_test "a module of packages only"
      [
        "version: 1";
        "modules: {m: {external: [\"@scope/name\", \"node:fs\"]}}";
      ]
      {
        modules = [
          { id = id "m"; contents = Packages ["@scope/name"; "node:fs"] };
        ];
      };
    valid_test "a byte order mark is fine"
      "\xef\xbb\xbfversion: 1\nmodules: {m: {paths: [x]}}\n";
    valid_test "a comment after the document end"
      (text
         [
           "version: 1";
           "modules: {m: {paths: [x]}}";
           "...";
           "# done";
         ]);
    valid_test "comments and a document marker are fine"
      (text
         [
           "# the manifest"; "---";
           "version: 1";
           "modules:";
           "  m: {paths: [\"src/**\"]}  # all of it";
         ]);
    valid_test "no final newline is fine"
      "version: 1\nmodules: {m: {paths: [x]}}";
    valid_test "Windows newlines are fine"
      (text
         [
           "version: 1\r";
           "modules: {m: {paths: [\"src/**\"]}}\r";
         ]);
    valid_test "an overlap between modules is not an error"
      (text
         [
           "version: 1";
           "modules: {a: {paths: [\"src/**\"]}, b: {paths: [\"src/x/**\"]}}";
         ]);
    valid_test "a pattern and a package with the same text"
      (text
         [
           "version: 1";
           "modules: {a: {paths: [requests]}, b: {external: [requests]}}";
         ]);
  ]

(** Test cases for the texts that [Manifest.of_string] rejects: each names the
    line, the location and a part of the message of the error. *)
let of_string_error_tests =
  let open Alcotest in
  let invalid_test name lines ~line ~at ~contains:part =
    test_case
      name
      `Quick
      (fun () ->
         match Manifest.of_string (text lines) with
         | Ok _    -> fail "should not parse"
         | Error e ->
           check int "line" line e.line;
           check string "location" at e.location;
           if not (String.contains_sub e.message ~sub:part) then
             failf "%S not in %S" part e.message)
  in
  (* [modules m] is the lines of a manifest whose only module is [m]. *)
  let modules m = ["version: 1"; "modules:"; "  " ^ m]
  in [
    invalid_test "empty input"
      []
      ~line:1 ~at:"" ~contains:"empty document";
    invalid_test "only a comment"
      [
        "# nothing";
      ]
      ~line:1 ~at:"" ~contains:"empty document";
    invalid_test "not UTF-8"
      [
        "version: 1";
        "modules: {m: {paths: [\xff]}}";
      ]
      ~line:2 ~at:"" ~contains:"not valid UTF-8";
    invalid_test "a control character"
      [
        "version: 1";
        "modules: {m: {paths: [x]}}";
        "\001";
      ]
      ~line:3 ~at:"" ~contains:"control characters are not allowed";
    invalid_test "UTF-16"
      [
        "\xff\xfe" ^ utf16le "version: 1\nmodules: {m: {paths: [x]}}";
      ]
      ~line:1 ~at:"" ~contains:"not valid UTF-8";
    invalid_test "a control character after multibyte text"
      [
        "# \xc3\xa9";
        "\001";
      ]
      ~line:2 ~at:"" ~contains:"control characters are not allowed";
    (* The root of a document that is only a marker is an empty scalar, and the
       parser puts it on the next line. *)
    invalid_test "a document marker alone"
      [
        "---";
      ]
      ~line:2 ~at:"" ~contains:"must be a mapping";
    invalid_test "a list"
      [
        "- version";
      ]
      ~line:1 ~at:"" ~contains:"must be a mapping";
    (* A syntax error is located at the last line that the parser read, not
       at the problem; see [Manifest.of_string]. *)
    invalid_test "a package name that starts with an at sign"
      [
        "version: 1";
        "modules: {m: {external: [@scope/name]}}";
      ]
      ~line:2 ~at:"" ~contains:"found character that cannot start any token";
    invalid_test "a pattern that starts with a star"
      [
        "version: 1";
        "modules:";
        "  m: {paths: [**/x]}";
      ]
      ~line:3 ~at:"" ~contains:"did not find expected alphabetic or numeric character";
    invalid_test "a tab as indentation"
      [
        "version: 1";
        "modules:";
        "\tm: {paths: [x]}";
      ]
      ~line:2 ~at:"" ~contains:"found character that cannot start any token";
    invalid_test "an unterminated list"
      [
        "version: 1";
        "modules:";
        "  m: {paths: [\"src/**\"";
      ]
      ~line:3 ~at:"" ~contains:"did not find expected ',' or ']'";
    invalid_test "text after the root"
      [
        "{version: 1, modules: {m: {paths: [\"src/**\"]}}}";
        "rules: []";
      ]
      ~line:2 ~at:"" ~contains:"did not find expected <document start>";
    invalid_test "a second document"
      [
        "version: 1";
        "modules: {m: {paths: [\"src/**\"]}}";
        "---";
        "version: 1";
      ]
      ~line:3 ~at:"" ~contains:"second document";
    invalid_test "an anchor"
      [
        "version: &v 1";
        "modules: {m: {paths: [\"src/**\"]}}";
      ]
      ~line:1 ~at:"version" ~contains:"anchors are not supported";
    invalid_test "an anchor under mixed nesting"
      [
        "version: 1";
        "modules: {m: {paths: [{x: [1, &a 2]}]}}";
      ]
      ~line:2 ~at:"modules.m.paths[0].x[1]" ~contains:"anchors are not supported";
    invalid_test "an anchor on a pattern"
      [
        "version: 1";
        "modules: {m: {paths: [&p \"src/**\"]}}";
      ]
      ~line:2 ~at:"modules.m.paths[0]" ~contains:"anchors are not supported";
    invalid_test "a tag on a mapping"
      [
        "version: 1";
        "modules: {m: !!map {paths: [\"src/**\"]}}";
      ]
      ~line:2 ~at:"modules.m" ~contains:"tags are not supported";
    invalid_test "an anchor on a mapping"
      [
        "version: 1";
        "modules: &m {m: {paths: [\"src/**\"]}}";
      ]
      ~line:2 ~at:"modules" ~contains:"anchors are not supported";
    invalid_test "a tag on a key"
      [
        "version: 1";
        "modules: {!!str m: {paths: [\"src/**\"]}}";
      ]
      ~line:2 ~at:"modules" ~contains:"tags are not supported";
    invalid_test "an anchor on a key"
      [
        "version: 1";
        "modules: {&m m: {paths: [\"src/**\"]}}";
      ]
      ~line:2 ~at:"modules" ~contains:"anchors are not supported";
    invalid_test "an alias"
      [
        "version: 1";
        "modules: *m";
      ]
      ~line:2 ~at:"modules" ~contains:"aliases are not supported";
    invalid_test "an alias as a key"
      [
        "version: 1";
        "modules: {*m : {paths: [\"src/**\"]}}";
      ]
      ~line:2 ~at:"modules" ~contains:"aliases are not supported";
    invalid_test "a tag"
      [
        "version: !!int 1";
        "modules: {m: {paths: [\"src/**\"]}}";
      ]
      ~line:1 ~at:"version" ~contains:"tags are not supported";
    invalid_test "a tag on a list"
      [
        "version: 1";
        "modules: {m: {paths: !!seq [\"src/**\"]}}";
      ]
      ~line:2 ~at:"modules.m.paths" ~contains:"tags are not supported";
    invalid_test "a key that is not a scalar"
      [
        "? [version]";
        ": 1";
      ]
      ~line:1 ~at:"" ~contains:"key must be a string";
    invalid_test "no version"
      [
        "modules: {m: {paths: [\"src/**\"]}}";
      ]
      ~line:1 ~at:"" ~contains:{|missing key "version"|};
    invalid_test "version without a value"
      [
        "version:";
        "modules: {m: {paths: [\"src/**\"]}}";
      ]
      ~line:1 ~at:"version" ~contains:"must be a plain integer";
    invalid_test "version with a sign"
      [
        "version: +1";
        "modules: {m: {paths: [\"src/**\"]}}";
      ]
      ~line:1 ~at:"version" ~contains:"must be a plain integer";
    invalid_test "version as a string"
      [
        "version: \"1\"";
        "modules: {m: {paths: [\"src/**\"]}}";
      ]
      ~line:1 ~at:"version" ~contains:"must be a plain integer";
    invalid_test "version as a float"
      [
        "version: 1.0";
        "modules: {m: {paths: [\"src/**\"]}}";
      ]
      ~line:1 ~at:"version" ~contains:"must be a plain integer";
    invalid_test "version as a list"
      [
        "version: [1]";
        "modules: {m: {paths: [\"src/**\"]}}";
      ]
      ~line:1 ~at:"version" ~contains:"must be a plain integer";
    invalid_test "version with a leading zero"
      [
        "version: 01";
        "modules: {m: {paths: [\"src/**\"]}}";
      ]
      ~line:1 ~at:"version" ~contains:"must be a plain integer";
    invalid_test "version zero"
      [
        "version: 0";
        "modules: {m: {paths: [\"src/**\"]}}";
      ]
      ~line:1 ~at:"version" ~contains:"unsupported version 0, expected 1";
    invalid_test "unsupported version"
      [
        "version: 2";
        "modules: {m: {paths: [\"src/**\"]}}";
      ]
      ~line:1 ~at:"version" ~contains:"unsupported version 2, expected 1";
    invalid_test "unsupported version before unknown keys"
      [
        "version: 2";
        "modules: {m: {paths: [\"src/**\"]}}";
        "rules: []";
      ]
      ~line:1 ~at:"version" ~contains:"unsupported version 2, expected 1";
    invalid_test "no modules"
      [
        "version: 1";
      ]
      ~line:1 ~at:"" ~contains:{|missing key "modules"|};
    invalid_test "unknown top-level key"
      [
        "version: 1";
        "modules: {m: {paths: [\"src/**\"]}}";
        "rules: []";
      ]
      ~line:3 ~at:"" ~contains:{|unknown key "rules"|};
    invalid_test "duplicate top-level key"
      [
        "version: 1";
        "modules: {m: {paths: [\"src/**\"]}}";
        "version: 1";
      ]
      ~line:3 ~at:"" ~contains:{|duplicate key "version"|};
    invalid_test "modules not a mapping"
      [
        "version: 1";
        "modules: []";
      ]
      ~line:2 ~at:"modules" ~contains:"must be a mapping";
    invalid_test "no module"
      [
        "version: 1";
        "modules: {}";
      ]
      ~line:2 ~at:"modules" ~contains:"at least one module expected";
    invalid_test "duplicate module"
      [
        "version: 1";
        "modules:";
        "  m: {paths: [\"src/**\"]}";
        "  m: {paths: [\"lib/**\"]}";
      ]
      ~line:4 ~at:"modules" ~contains:{|duplicate key "m"|};
    invalid_test "id with a control character"
      (modules "\"a\\tb\": {paths: [x]}")
      ~line:3 ~at:"modules.a\\tb" ~contains:"control characters are not allowed";
    invalid_test "id not an id"
      (modules "/m: {paths: [\"src/**\"]}")
      ~line:3 ~at:"modules./m" ~contains:"invalid module id: empty segment";
    invalid_test "module not a mapping"
      (modules "m: [\"src/**\"]")
      ~line:3 ~at:"modules.m" ~contains:"must be a mapping";
    invalid_test "unknown module key"
      (modules "m: {paths: [\"src/**\"], family: true}")
      ~line:3 ~at:"modules.m" ~contains:{|unknown key "family"|};
    invalid_test "neither paths nor external"
      (modules "m: {}")
      ~line:3 ~at:"modules.m" ~contains:{|"paths" or "external" expected|};
    invalid_test "both paths and external"
      (modules "m: {paths: [\"src/**\"], external: [x]}")
      ~line:3 ~at:"modules.m" ~contains:{|either "paths" or "external", not both|};
    invalid_test "paths not a list"
      (modules "m: {paths: \"src/**\"}")
      ~line:3 ~at:"modules.m.paths" ~contains:"must be a list";
    invalid_test "duplicate key in a module"
      (modules "m: {paths: [\"src/**\"], paths: [\"lib/**\"]}")
      ~line:3 ~at:"modules.m" ~contains:{|duplicate key "paths"|};
    invalid_test "a control character in a pattern"
      (modules "m: {paths: [\"src/\\t\"]}")
      ~line:3 ~at:"modules.m.paths[0]" ~contains:"control characters are not allowed";
    invalid_test "empty pattern"
      (modules "m: {paths: ['']}")
      ~line:3 ~at:"modules.m.paths[0]" ~contains:"invalid pattern: empty";
    invalid_test "no pattern"
      (modules "m: {paths: []}")
      ~line:3 ~at:"modules.m.paths" ~contains:"at least one pattern expected";
    invalid_test "pattern not a string"
      (modules "m: {paths: [[\"src/**\"]]}")
      ~line:3 ~at:"modules.m.paths[0]" ~contains:"must be a string";
    invalid_test "pattern not a pattern"
      (modules "m: {paths: [\"src/***\"]}")
      ~line:3 ~at:"modules.m.paths[0]" ~contains:"invalid pattern: '**' must be a whole segment";
    invalid_test "duplicate pattern"
      (modules "m: {paths: [\"src/**\", \"lib/**\", \"src/**\"]}")
      ~line:3 ~at:"modules.m.paths[2]" ~contains:{|duplicate pattern "src/**"|};
    invalid_test "pattern of another module"
      [
        "version: 1";
        "modules:";
        "  a: {paths: [\"src/**\"]}";
        "  b: {paths: [\"src/**\"]}";
      ]
      ~line:4 ~at:"modules.b.paths[0]" ~contains:{|pattern "src/**" already in module "a"|};
    invalid_test "external not a list"
      (modules "m: {external: x}")
      ~line:3 ~at:"modules.m.external" ~contains:"must be a list";
    invalid_test "no package"
      (modules "m: {external: []}")
      ~line:3 ~at:"modules.m.external" ~contains:"at least one package expected";
    invalid_test "package not a string"
      (modules "m: {external: [[x]]}")
      ~line:3 ~at:"modules.m.external[0]" ~contains:"must be a string";
    invalid_test "a control character in a package name"
      (modules "m: {external: [\"a\\nb\"]}")
      ~line:3 ~at:"modules.m.external[0]" ~contains:"control characters are not allowed";
    invalid_test "empty package name"
      (modules "m: {external: [x, \"\"]}")
      ~line:3 ~at:"modules.m.external[1]" ~contains:"empty package name";
    invalid_test "duplicate package"
      (modules "m: {external: [x, x]}")
      ~line:3 ~at:"modules.m.external[1]" ~contains:{|duplicate package "x"|};
    invalid_test "package of another module"
      [
        "version: 1";
        "modules:";
        "  a: {external: [x]}";
        "  b: {external: [x]}";
      ]
      ~line:4 ~at:"modules.b.external[0]" ~contains:{|package "x" already in module "a"|};
    invalid_test "the line is the node's own"
      [
        "version: 1";
        "modules:";
        "  m:";
        "    paths:";
        "      - \"src/**\"";
        "      - \"src/**\"";
      ]
      ~line:6 ~at:"modules.m.paths[1]" ~contains:{|duplicate pattern "src/**"|};
  ]

(** Test cases for [Manifest.error_to_string]. *)
let error_text_tests =
  let open Alcotest in
  let error_text_test name error expected =
    test_case
      name
      `Quick
      (fun () ->
         check string "text" expected (Manifest.error_to_string error))
  in [
    error_text_test "with a location"
      { line = 4; location = "modules.m"; message = {|unknown key "x"|} }
      {|line 4: modules.m: unknown key "x"|};
    error_text_test "at the root"
      { line = 1; location = ""; message = "must be a mapping" }
      "line 1: must be a mapping";
  ]


let () =
  Alcotest.run ~compact:true "Manifest" [
    ("ids", id_tests);
    ("id equality", id_equal_tests);
    ("of_string", of_string_tests);
    ("of_string errors", of_string_error_tests);
    ("error text", error_text_tests);
  ]
