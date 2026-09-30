(** Tests for [Graph]. *)

open Perpend_core
open Perpend_facts
open Perpend_manifest
open Perpend_engine
open Graph


(** {1 Test helpers} *)

(** The manifest for every case: nested places, an overlap of two cross-cutting
    patterns, a module that no fact mentions and an external module. *)
let manifest =
  let lines = [
    "version: 1";
    "modules:";
    "  api:     {paths: ['src/api/**']}";
    "  core:    {paths: ['src/core/**']}";
    "  core/db: {paths: ['src/core/db/**']}";
    "  tests:   {paths: ['**/tests/**']}";
    "  styles:  {paths: ['**/*.css']}";
    "  docs:    {paths: ['docs/**']}";
    "  ext/orm: {external: [sqlalchemy]}";
  ] in
  let text = lines |> List.map (fun l -> l ^ "\n") |> String.concat "" in
  match Manifest.of_string text with
  | Ok manifest -> manifest
  | Error e     -> failwith (Manifest.error_to_string e)

(** The header of a facts file from a provider with every capability. *)
let header =
  {|{"type":"provider","protocol":"perpend.facts/1","name":"py-grimp","version":"0.1","languages":["python"],"capabilities":["file_imports","imported_names","package_imports","unresolved","context_flags","file_inventory"]}|}

(** [file path] is a file record for [path]. *)
let file path =
  Printf.sprintf {|{"type":"file","path":"%s"}|} path

(** [file_fact line subject object_] is a fact on [line] of the file [subject]
    about the file [object_]. *)
let file_fact line subject object_ =
  Printf.sprintf
    {|{"type":"fact","subject":"%s","object":{"kind":"file","id":"%s"},"precision":"syntactic","names":[],"flags":[],"evidence":{"line":%d}}|}
    subject object_ line

(** [package_fact line subject name] is a fact on [line] of the file [subject]
    about the package [name]. *)
let package_fact line subject name =
  Printf.sprintf
    {|{"type":"fact","subject":"%s","object":{"kind":"package","id":"%s"},"precision":"resolved","names":[],"flags":[],"evidence":{"line":%d}}|}
    subject name line

(** [unresolved line subject] is an unresolved record on [line] of the file
    [subject]. *)
let unresolved line subject =
  Printf.sprintf
    {|{"type":"unresolved","subject":"%s","target":"importlib.import_module(...)","evidence":{"line":%d}}|}
    subject line

(** [facts lines] is the facts that are read from the header and the JSONL
    [lines]. *)
let facts lines =
  let text =
    (header :: lines) |> List.map (fun l -> l ^ "\n") |> String.concat ""
  in
  match Facts.of_string text with
  | Ok facts -> facts
  | Error e  -> failwith (Facts.error_to_string e)

(** [graph lines] is the graph of [facts lines] over [manifest], where [lines]
    are JSONL lines. *)
let graph lines =
  create manifest (facts lines)

(** [edge_summary e] is the edge [e] as the id strings of its ends and the lines
    of its occurrences: enough to tell every fact of the fixtures apart. *)
let edge_summary (e : Edge.t) =
  (
    Manifest.Id.to_string e.source,
    Manifest.Id.to_string e.target,
    List.map (fun (f : Facts.Fact.t) -> Option.get f.evidence.line)
      e.occurrences
  )

(** [edge_summaries g] is [edge_summary] over the edges of the graph [g]. *)
let edge_summaries g =
  List.map edge_summary g.edges

(** [ids l] is the list of module ids [l] as strings. *)
let ids l =
  List.map Manifest.Id.to_string l

(** [paths l] is the list of paths [l] as strings. *)
let paths l =
  List.map Path.to_string l

(** [ambiguous l] is the list of ambiguous files [l] as strings: each path with
    its candidates as pattern and module. *)
let ambiguous l =
  (* [candidate (p, id)] is the pattern [p] and the module id [id] as
     strings. *)
  let candidate (p, id) =
    (Glob.to_string p, Manifest.Id.to_string id)
  in
  List.map
    (fun (path, candidates) ->
       (Path.to_string path, List.map candidate candidates))
    l

(** [Facts.Fact.t] for Alcotest, printed with every field that the fixtures
    set. *)
let fact_testable =
  let pp fmt (f : Facts.Fact.t) =
    let object_ =
      match f.object_ with
      | File path    -> Path.to_string path
      | Package name -> name
    in
    Format.fprintf fmt "%s:%d -> %s, %s, names [%s], flags [%s], text %S"
      (Path.to_string f.subject)
      (Option.get f.evidence.line)
      object_
      (Facts.Precision.to_string f.precision)
      (String.concat "; " f.names)
      (String.concat "; " (List.map Facts.Flag.to_string f.flags))
      (Option.value f.evidence.text ~default:"")
  in
  Alcotest.testable pp ( = )

(** [Facts.Unresolved.t] for Alcotest, printed as its subject and line. *)
let unresolved_testable =
  Alcotest.testable
    (fun fmt (u : Facts.Unresolved.t) ->
       Format.fprintf fmt "%s:%d"
         (Path.to_string u.subject) (Option.get u.evidence.line))
    ( = )


(** {1 Edges} *)

(** Test cases for the edges of [Graph.create]: each names the facts and the
    expected edges as [edge_summary] gives them. *)
let edges_tests =
  let open Alcotest in
  let edges_test name lines expected =
    test_case
      name
      `Quick
      (fun () ->
         check (list (triple string string (list int)))
           "edges"
           expected (edge_summaries (graph lines)))
  in [
    edges_test "a fact between two modules is an edge"
      [file_fact 1 "src/api/a.py" "src/core/b.py"]
      [("api", "core", [1])];
    edges_test "two facts between the same modules are one edge"
      [
        file_fact 1 "src/api/a.py" "src/core/b.py";
        file_fact 2 "src/api/c.py" "src/core/d.py";
      ]
      [("api", "core", [1; 2])];
    edges_test "two identical facts are two occurrences"
      [
        file_fact 1 "src/api/a.py" "src/core/b.py";
        file_fact 1 "src/api/a.py" "src/core/b.py";
      ]
      [("api", "core", [1; 1])];
    edges_test "occurrences keep the order of the facts"
      [
        file_fact 2 "src/api/a.py" "src/core/b.py";
        file_fact 1 "src/api/a.py" "src/core/b.py";
      ]
      [("api", "core", [2; 1])];
    edges_test "edges keep the order of their first occurrence"
      [
        file_fact 1 "src/core/b.py" "src/core/db/c.py";
        file_fact 2 "src/api/a.py" "src/core/b.py";
        file_fact 3 "src/api/a.py" "src/core/d.py";
        file_fact 4 "src/api/e.py" "src/core/b.py";
        file_fact 5 "src/core/d.py" "src/core/db/e.py";
      ]
      [("core", "core/db", [1; 5]); ("api", "core", [2; 3; 4])];
    edges_test "the two directions are two edges"
      [
        file_fact 1 "src/api/a.py" "src/core/b.py";
        file_fact 2 "src/core/b.py" "src/api/a.py";
      ]
      [("api", "core", [1]); ("core", "api", [2])];
    edges_test "edges that share one end are different edges"
      [
        file_fact 1 "src/api/a.py" "src/core/b.py";
        file_fact 2 "src/core/db/c.py" "src/core/b.py";
        file_fact 3 "src/api/a.py" "src/core/db/c.py";
      ]
      [("api", "core", [1]); ("core/db", "core", [2]); ("api", "core/db", [3])];
    edges_test "a fact inside one module is not an edge"
      [file_fact 1 "src/core/a.py" "src/core/b.py"]
      [];
    edges_test "a nested module is another module"
      [file_fact 1 "src/core/a.py" "src/core/db/b.py"]
      [("core", "core/db", [1])];
    edges_test "a package fact is an edge to its module"
      [package_fact 1 "src/api/a.py" "sqlalchemy"]
      [("api", "ext/orm", [1])];
    edges_test "a package that no module lists is not an edge"
      [package_fact 1 "src/api/a.py" "requests"]
      [];
    edges_test "an unattributed subject is not an edge"
      [file_fact 1 "setup.py" "src/core/a.py"]
      [];
    edges_test "an unattributed object is not an edge"
      [file_fact 1 "src/core/a.py" "setup.py"]
      [];
    edges_test "an ambiguous subject is not an edge"
      [file_fact 1 "src/core/tests/a.css" "src/core/b.py"]
      [];
    edges_test "an ambiguous object is not an edge"
      [file_fact 1 "src/api/a.py" "src/core/tests/a.css"]
      [];
    edges_test "an edge keeps its occurrences next to facts that are not edges"
      [
        file_fact 1 "src/api/a.py" "src/core/b.py";
        file_fact 2 "src/api/a.py" "src/api/c.py";
        file_fact 3 "src/api/a.py" "setup.py";
        file_fact 4 "src/api/a.py" "src/core/b.py";
      ]
      [("api", "core", [1; 4])];
    edges_test "no facts, no edges"
      [file "src/api/a.py"]
      [];
  ]

(** Test cases for the occurrences: the facts themselves, with every field as
    the provider wrote it. *)
let occurrences_tests =
  let open Alcotest in [
    test_case
      "occurrences are the facts as read"
      `Quick
      (fun () ->
         let read =
           facts [
             {|{"type":"fact","subject":"src/api/a.py","object":{"kind":"file","id":"src/core/b.py"},"precision":"syntactic","names":["foo","bar"],"flags":["lazy","guarded"],"evidence":{"line":3,"text":"from core.b import foo, bar"}}|};
             package_fact 2 "src/api/a.py" "sqlalchemy";
             file_fact 4 "src/api/c.py" "src/core/d.py";
           ]
         in
         let g = create manifest read in
         (* Two facts of the edge from [api] to [core], and one of the edge
            from [api] to [ext/orm]. *)
         let expected =
           match read.facts with
           | [first; package; second] -> [[first; second]; [package]]
           | _                        -> fail "three facts expected"
         in
         check (list (list fact_testable))
           "occurrences"
           expected
           (List.map (fun (e : Edge.t) -> e.occurrences) g.edges));
  ]


(** {1 Nodes} *)

(** Test cases for the nodes: every module of the manifest, in its order. *)
let nodes_tests =
  let open Alcotest in
  let all =
    ["api"; "core"; "core/db"; "tests"; "styles"; "docs"; "ext/orm"]
  in
  let nodes_test name lines =
    test_case
      name
      `Quick
      (fun () -> check (list string) "nodes" all (ids (graph lines).nodes))
  in [
    nodes_test "every module, with edges or not"
      [file_fact 1 "src/api/a.py" "src/core/b.py"];
    nodes_test "every module, when there are no facts" [];
  ]


(** {1 Coverage} *)

(** Test cases for the unattributed files of coverage.  Each checks that the
    files are unattributed and not ambiguous too. *)
let unattributed_tests =
  let open Alcotest in
  let unattributed_test name lines expected =
    test_case
      name
      `Quick
      (fun () ->
         let coverage = (graph lines).coverage in
         let patterns = list (pair string string) in
         check (pair (list string) (list (pair string patterns)))
           "unattributed and ambiguous"
           (expected, [])
           (paths coverage.unattributed, ambiguous coverage.ambiguous))
  in [
    unattributed_test "a file record"
      [file "setup.py"]
      ["setup.py"];
    unattributed_test "a subject"
      [file_fact 1 "setup.py" "src/core/a.py"]
      ["setup.py"];
    unattributed_test "an object"
      [file_fact 1 "src/core/a.py" "setup.py"]
      ["setup.py"];
    unattributed_test "the subject of an unresolved record"
      [unresolved 1 "setup.py"]
      ["setup.py"];
    unattributed_test "a file with a module is not unattributed"
      [
        file "src/api/a.py";
        file_fact 1 "src/api/a.py" "src/core/b.py";
        unresolved 2 "src/core/b.py";
      ]
      [];
    unattributed_test "every file once, in the order of first mention"
      [
        file "x.txt";
        file_fact 1 "y.txt" "src/core/a.py";
        file_fact 2 "src/api/a.py" "x.txt";
        file_fact 3 "v.txt" "z.txt";
        file_fact 4 "y.txt" "z.txt";
        unresolved 5 "w.txt";
        unresolved 6 "x.txt";
      ]
      ["x.txt"; "y.txt"; "v.txt"; "z.txt"; "w.txt"];
    unattributed_test "records of one kind keep their order"
      [
        file "z.txt";
        file "a.txt";
        unresolved 1 "y.txt";
        unresolved 2 "b.txt";
      ]
      ["z.txt"; "a.txt"; "y.txt"; "b.txt"];
    unattributed_test "file records come first, unresolved records last"
      [
        unresolved 1 "w.txt";
        file_fact 2 "y.txt" "src/core/a.py";
        file "x.txt";
      ]
      ["x.txt"; "y.txt"; "w.txt"];
    unattributed_test "the subject of a fact about a package"
      [package_fact 1 "setup.py" "requests"]
      ["setup.py"];
    unattributed_test "a package is not a file"
      [package_fact 1 "src/api/a.py" "requests"]
      [];
  ]

(** Test cases for the ambiguous files of coverage.  Each checks that the
    files are ambiguous and not unattributed too. *)
let ambiguous_tests =
  let open Alcotest in
  let ambiguous_test name lines expected =
    test_case
      name
      `Quick
      (fun () ->
         let coverage = (graph lines).coverage in
         let patterns = list (pair string string) in
         check (pair (list string) (list (pair string patterns)))
           "unattributed and ambiguous"
           ([], expected)
           (paths coverage.unattributed, ambiguous coverage.ambiguous))
  in
  (* The candidates of a [.css] file under a [tests] directory. *)
  let candidates = [("**/tests/**", "tests"); ("**/*.css", "styles")] in [
    ambiguous_test "a file with candidates of two modules"
      [file_fact 1 "src/api/a.py" "src/core/tests/a.css"]
      [("src/core/tests/a.css", candidates)];
    ambiguous_test "an ambiguous subject and an ambiguous object"
      [file_fact 1 "src/core/tests/a.css" "src/core/tests/b.css"]
      [
        ("src/core/tests/a.css", candidates);
        ("src/core/tests/b.css", candidates);
      ];
    ambiguous_test "the subject of an unresolved record"
      [unresolved 1 "src/core/tests/a.css"]
      [("src/core/tests/a.css", candidates)];
    ambiguous_test "every file once, in the order of first mention"
      [
        file "src/core/tests/c.css";
        file_fact 1 "src/core/tests/b.css" "src/core/tests/c.css";
        unresolved 2 "src/core/tests/a.css";
        unresolved 3 "src/core/tests/b.css";
      ]
      [
        ("src/core/tests/c.css", candidates);
        ("src/core/tests/b.css", candidates);
        ("src/core/tests/a.css", candidates);
      ];
    ambiguous_test "a file of two modules is not ambiguous when one wins"
      [file "src/core/tests/a.py"]
      [];
  ]

(** Test cases for the packages of coverage. *)
let packages_tests =
  let open Alcotest in
  let packages_test name lines expected =
    test_case
      name
      `Quick
      (fun () ->
         check (list string) "unlisted_packages"
           expected (graph lines).coverage.unlisted_packages)
  in [
    packages_test "a package that no module lists"
      [package_fact 1 "src/api/a.py" "requests"]
      ["requests"];
    packages_test "a listed package is not unlisted"
      [package_fact 1 "src/api/a.py" "sqlalchemy"]
      [];
    packages_test "every package once, in the order of first mention"
      [
        package_fact 1 "src/api/a.py" "requests";
        package_fact 2 "src/api/a.py" "httpx";
        package_fact 3 "src/core/b.py" "requests";
      ]
      ["requests"; "httpx"];
    packages_test "an unattributed subject does not hide the package"
      [package_fact 1 "setup.py" "requests"]
      ["requests"];
    packages_test "an ambiguous subject does not hide the package"
      [package_fact 1 "src/core/tests/a.css" "requests"]
      ["requests"];
  ]

(** Test cases for the unresolved records of coverage. *)
let unresolved_tests =
  let open Alcotest in [
    test_case
      "unresolved records pass through as read"
      `Quick
      (fun () ->
         let read =
           facts [
             unresolved 2 "src/core/b.py";
             unresolved 1 "setup.py";
             unresolved 3 "src/core/b.py";
             unresolved 3 "src/core/b.py";
           ]
         in
         check (list unresolved_testable) "unresolved"
           read.unresolved (create manifest read).coverage.unresolved);
  ]


(** {1 Test runner} *)

let () =
  Alcotest.run ~compact:true "Graph" [
    ("edges", edges_tests);
    ("occurrences", occurrences_tests);
    ("nodes", nodes_tests);
    ("coverage unattributed", unattributed_tests);
    ("coverage ambiguous", ambiguous_tests);
    ("coverage packages", packages_tests);
    ("coverage unresolved", unresolved_tests);
  ]
