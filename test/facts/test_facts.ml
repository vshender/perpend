(** Tests for [Facts]. *)

open Perpend_core
open Perpend_facts
open Facts
open Test_helpers


(** {1 Test helpers} *)

(** [path_of_string s] is the path [s]. *)
let path_of_string s =
  Result.get_ok (Path.of_string s)

(** [file lines] is the JSONL text with [lines], each ended by a newline. *)
let file lines =
  String.concat "" (List.map (fun l -> l ^ "\n") lines)

(** The header of a file from a provider with every capability. *)
let header =
  {|{"type":"provider","protocol":"perpend.facts/1","name":"py-grimp","version":"0.1","languages":["python"],"capabilities":["file_imports","imported_names","package_imports","unresolved","context_flags","file_inventory"]}|}

(** A file with records of every kind, as [Facts.to_string] prints it. *)
let example =
  file [
    header;
    {|{"type":"file","path":"src/a.py","language":"python"}|};
    {|{"type":"file","path":"src/b.py"}|};
    {|{"type":"fact","subject":"src/a.py","object":{"kind":"file","id":"src/b.py"},"precision":"syntactic","names":["foo","bar"],"flags":["lazy","guarded"],"evidence":{"line":3,"text":"from b import foo, bar"}}|};
    {|{"type":"fact","subject":"src/a.py","object":{"kind":"package","id":"requests"},"precision":"resolved","names":[],"flags":[],"evidence":{}}|};
    {|{"type":"unresolved","subject":"src/b.py","target":"importlib.import_module(...)","evidence":{"line":40}}|};
  ]

(** The records of [example] in another order: every kind mixed with the
    others.  It reads to the same value. *)
let interleaved =
  file [
    header;
    {|{"type":"unresolved","subject":"src/b.py","target":"importlib.import_module(...)","evidence":{"line":40}}|};
    {|{"type":"fact","subject":"src/a.py","object":{"kind":"file","id":"src/b.py"},"precision":"syntactic","names":["foo","bar"],"flags":["lazy","guarded"],"evidence":{"line":3,"text":"from b import foo, bar"}}|};
    {|{"type":"file","path":"src/a.py","language":"python"}|};
    {|{"type":"fact","subject":"src/a.py","object":{"kind":"package","id":"requests"},"precision":"resolved","names":[],"flags":[],"evidence":{}}|};
    {|{"type":"file","path":"src/b.py"}|};
  ]

(** What [example] reads to. *)
let example_value =
  let a = path_of_string "src/a.py" and b = path_of_string "src/b.py" in {
    Facts.provider = {
      name = "py-grimp";
      version = "0.1";
      languages = ["python"];
      capabilities = [
        File_imports;
        Imported_names;
        Package_imports;
        Unresolved;
        Context_flags;
        File_inventory;
      ];
    };
    files = [
      { path = a; language = Some "python" };
      { path = b; language = None };
    ];
    facts = [
      {
        subject = a;
        object_ = File b;
        precision = Syntactic;
        names = ["foo"; "bar"];
        flags = [Lazy; Guarded];
        evidence = { line = Some 3; text = Some "from b import foo, bar" };
      };
      {
        subject = a;
        object_ = Package "requests";
        precision = Resolved;
        names = [];
        flags = [];
        evidence = { line = None; text = None };
      };
    ];
    unresolved = [
      {
        subject = b;
        target = "importlib.import_module(...)";
        evidence = { line = Some 40; text = None };
      };
    ];
  }

(** [Facts.t] for Alcotest, printed as JSONL. *)
let facts_testable =
  Alcotest.testable
    (fun fmt t -> Format.pp_print_string fmt (Facts.to_string t))
    ( = )

(** [Facts.error] for Alcotest. *)
let error_testable =
  Alcotest.testable
    (fun fmt e -> Format.pp_print_string fmt (Facts.error_to_string e))
    ( = )


(** {1 Reading and writing} *)

(** Test cases for the files [Facts.of_string] accepts. *)
let valid_tests =
  let open Alcotest in
  let valid_test name text =
    test_case
      name
      `Quick
      (fun () ->
         match Facts.of_string text with
         | Ok _    -> ()
         | Error e -> fail (Facts.error_to_string e))
  in [
    test_case
      "the example reads to its value"
      `Quick
      (fun () ->
         check (result facts_testable error_testable)
           "of_string"
           (Ok example_value) (Facts.of_string example));
    test_case
      "the interleaved example reads to the same value"
      `Quick
      (fun () ->
         check (result facts_testable error_testable)
           "of_string"
           (Ok example_value) (Facts.of_string interleaved));
    test_case
      "the example value writes to the example"
      `Quick
      (fun () ->
         check string
           "to_string"
           example (Facts.to_string example_value));
    test_case
      "a header alone is a file"
      `Quick
      (fun () ->
         check (result facts_testable error_testable)
           "of_string"
           (Ok { example_value with files = []; facts = []; unresolved = [] })
           (Facts.of_string (file [header])));
    valid_test "no final newline is fine" header;
    valid_test "a Windows newline is fine" (header ^ "\r\n");
    valid_test "field order does not matter"
      {|{"name":"py-grimp","type":"provider","capabilities":[],"languages":[],"protocol":"perpend.facts/1","version":"0.1"}|};
  ]

(** A fact with every field, to damage in the invalid tests. *)
let fact =
  {|{"type":"fact","subject":"src/a.py","object":{"kind":"file","id":"src/b.py"},"precision":"syntactic","names":[],"flags":[],"evidence":{"line":3}}|}

(** [damaged from to_] is [fact] with [from] replaced by [to_]. *)
let damaged from to_ =
  match String.find_sub fact ~sub:from with
  | None   -> failwith (Printf.sprintf "%S not in the fact" from)
  | Some i ->
    let n = String.length fact and m = String.length from in
    String.sub fact 0 i ^ to_ ^ String.sub fact (i + m) (n - i - m)

(** Test cases for the files [Facts.of_string] rejects: each names the line of
    the error and a part of its message. *)
let invalid_tests =
  let open Alcotest in
  let invalid_test name lines ~line ~contains:part =
    test_case
      name
      `Quick
      (fun () ->
         match Facts.of_string (file lines) with
         | Ok _    -> fail "should not parse"
         | Error e ->
           check int "line" line e.line;
           if not (String.contains_sub e.message ~sub:part) then
             failf "%S not in %S" part e.message)
  in [
    invalid_test "empty input"
      []
      ~line:1 ~contains:"provider header expected";
    invalid_test "empty line"
      [""]
      ~line:1 ~contains:"blank line";
    invalid_test "line of spaces"
      ["  "]
      ~line:1 ~contains:"blank line";
    invalid_test "line of a carriage return"
      ["\r"]
      ~line:1 ~contains:"blank line";
    invalid_test "not UTF-8"
      ["\xff"]
      ~line:1 ~contains:"not valid UTF-8";
    invalid_test "not UTF-8 after unescaping"
      [header; {|{"type":"file","path":"a\uDC00"}|}]
      ~line:2 ~contains:"not valid UTF-8";
    invalid_test "not an object"
      ["[1]"]
      ~line:1 ~contains:"object expected";
    invalid_test "no type"
      [{|{"id":"a"}|}]
      ~line:1 ~contains:{|missing "type"|};
    invalid_test "type not a string"
      [{|{"type":1}|}]
      ~line:1 ~contains:{|"type" must be a string|};
    invalid_test "duplicate type"
      [{|{"type":"file","type":"file"}|}]
      ~line:1 ~contains:{|duplicate "type"|};
    invalid_test "first line not a header"
      [{|{"type":"file","path":"a"}|}]
      ~line:1 ~contains:"provider header expected";
    invalid_test "no protocol"
      [{|{"type":"provider","name":"p","version":"1","languages":[],"capabilities":[]}|}]
      ~line:1 ~contains:{|missing "protocol"|};
    invalid_test "unknown protocol"
      [{|{"type":"provider","protocol":"perpend.facts/2","name":"p","version":"1","languages":[],"capabilities":[]}|}]
      ~line:1 ~contains:{|unknown protocol "perpend.facts/2"|};
    invalid_test "second header"
      [header; header]
      ~line:2 ~contains:"second provider header";
    invalid_test "empty provider name"
      [{|{"type":"provider","protocol":"perpend.facts/1","name":"","version":"1","languages":[],"capabilities":[]}|}]
      ~line:1 ~contains:"empty provider name";
    invalid_test "empty provider version"
      [{|{"type":"provider","protocol":"perpend.facts/1","name":"p","version":"","languages":[],"capabilities":[]}|}]
      ~line:1 ~contains:"empty provider version";
    invalid_test "empty language"
      [{|{"type":"provider","protocol":"perpend.facts/1","name":"p","version":"1","languages":[""],"capabilities":[]}|}]
      ~line:1 ~contains:"empty language";
    invalid_test "duplicate language"
      [{|{"type":"provider","protocol":"perpend.facts/1","name":"p","version":"1","languages":["python","python"],"capabilities":[]}|}]
      ~line:1 ~contains:"duplicate language: python";
    invalid_test "unknown capability"
      [{|{"type":"provider","protocol":"perpend.facts/1","name":"p","version":"1","languages":[],"capabilities":["magic"]}|}]
      ~line:1 ~contains:{|unknown capability: "magic"|};
    invalid_test "duplicate capability"
      [{|{"type":"provider","protocol":"perpend.facts/1","name":"p","version":"1","languages":[],"capabilities":["package_imports","package_imports"]}|}]
      ~line:1 ~contains:"duplicate capability: package_imports";
    invalid_test "unknown record type"
      [header; {|{"type":"edge"}|}]
      ~line:2 ~contains:{|unknown record type "edge"|};
    invalid_test "missing field"
      [header; {|{"type":"file"}|}]
      ~line:2 ~contains:"undefined: path";
    invalid_test "missing list field"
      [header; damaged {|"names":[],|} ""]
      ~line:2 ~contains:"undefined: names";
    invalid_test "missing list field in the header"
      [{|{"type":"provider","protocol":"perpend.facts/1","name":"p","version":"1","languages":[]}|}]
      ~line:1 ~contains:"undefined: capabilities";
    invalid_test "duplicate field"
      [header; {|{"type":"file","path":"a","path":"a"}|}]
      ~line:2 ~contains:"duplicate fields: path";
    invalid_test "file path not a path"
      [header; {|{"type":"file","path":"/a"}|}]
      ~line:2 ~contains:"not a path: must not start with '/'";
    invalid_test "empty file language"
      [header; {|{"type":"file","path":"a","language":""}|}]
      ~line:2 ~contains:"empty language";
    invalid_test "duplicate file"
      [header; {|{"type":"file","path":"a"}|}; {|{"type":"file","path":"a"}|}]
      ~line:3 ~contains:"duplicate file record: a";
    invalid_test "the error is on its line"
      [header; {|{"type":"file","path":"a"}|}; fact; "{"]
      ~line:4 ~contains:"Unexpected end of input";
    invalid_test "subject not a path"
      [header; damaged {|"subject":"src/a.py"|} {|"subject":"src//a.py"|}]
      ~line:2 ~contains:"not a path: empty segment";
    invalid_test "unknown artifact kind"
      [header; damaged {|"kind":"file","id":"src/b.py"|} {|"kind":"module","id":"b"|}]
      ~line:2 ~contains:"unknown artifact kind: module";
    invalid_test "empty package id"
      [header; damaged {|"kind":"file","id":"src/b.py"|} {|"kind":"package","id":""|}]
      ~line:2 ~contains:"empty package id";
    invalid_test "artifact with an extra field"
      [header; damaged {|"kind":"file","id":"src/b.py"|} {|"kind":"file","id":"src/b.py","x":1|}]
      ~line:2 ~contains:"extra fields: x";
    invalid_test "precision not a string"
      [header; damaged {|"syntactic"|} "1"]
      ~line:2 ~contains:"string needed: 1";
    invalid_test "unknown precision"
      [header; damaged {|"syntactic"|} {|"sintactic"|}]
      ~line:2 ~contains:{|unknown precision: "sintactic"|};
    invalid_test "empty name"
      [header; damaged {|"names":[]|} {|"names":["a",""]|}]
      ~line:2 ~contains:"empty name";
    invalid_test "duplicate name"
      [header; damaged {|"names":[]|} {|"names":["a","b","a"]|}]
      ~line:2 ~contains:"duplicate name: a";
    invalid_test "names not a list"
      [header; damaged {|"names":[]|} {|"names":"a"|}]
      ~line:2 ~contains:{|list needed: "a"|};
    invalid_test "unknown flag"
      [header; damaged {|"flags":[]|} {|"flags":["lazi"]|}]
      ~line:2 ~contains:{|unknown flag: "lazi"|};
    invalid_test "duplicate flag"
      [header; damaged {|"flags":[]|} {|"flags":["lazy","lazy"]|}]
      ~line:2 ~contains:"duplicate flag: lazy";
    invalid_test "line zero"
      [header; damaged {|"line":3|} {|"line":0|}]
      ~line:2 ~contains:"line must be at least 1";
    invalid_test "line as a string"
      [header; damaged {|"line":3|} {|"line":"3"|}]
      ~line:2 ~contains:{|integer needed: "3"|};
    invalid_test "line null"
      [header; damaged {|"line":3|} {|"line":null|}]
      ~line:2 ~contains:"integer needed: null";
    invalid_test "empty text"
      [header; damaged {|"line":3|} {|"line":3,"text":""|}]
      ~line:2 ~contains:"empty text";
    invalid_test "no evidence"
      [header; damaged {|,"evidence":{"line":3}|} ""]
      ~line:2 ~contains:"undefined: evidence";
    invalid_test "empty target"
      [header; {|{"type":"unresolved","subject":"a","target":"","evidence":{}}|}]
      ~line:2 ~contains:"empty target";
  ]

(** Test cases for the exact text of errors: one from the derived reader, one
    from the JSON parser, and how [Facts.error_to_string] prints them. *)
let error_text_tests =
  let open Alcotest in
  let error_test name lines expected =
    test_case
      name
      `Quick
      (fun () ->
         check (result facts_testable error_testable)
           "of_string"
           (Error expected) (Facts.of_string (file lines)))
  in [
    error_test "a derived reader names only the field"
      [header; {|{"type":"file","path":"a","lang":"python"}|}]
      { line = 2; message = "extra fields: lang" };
    error_test "a syntax error has no position of its own"
      [header; "{"]
      { line = 2; message = "Unexpected end of input" };
    test_case
      "error_to_string"
      `Quick
      (fun () ->
         let e = { line = 2; message = "extra fields: lang" } in
         check string
           "text"
           "line 2: extra fields: lang" (Facts.error_to_string e));
  ]


(** {1 Vocabulary} *)

(** Test cases for the string conversions of the enumerated types: every value
    has the expected JSON name and comes back from it, and an unknown name is
    [None]. *)
let vocabulary_tests =
  let open Alcotest in
  let name_test what of_string to_string (v, name) =
    test_case
      (Printf.sprintf "%s %s" what name)
      `Quick
      (fun () ->
         check string "to_string" name (to_string v);
         check bool "of_string" true (of_string name = Some v))
  in
  let unknown_test what of_string =
    test_case
      (Printf.sprintf "%s unknown" what)
      `Quick
      (fun () ->
         check bool "of_string" true (Option.is_none (of_string "unknown")))
  in
  let precision_test =
    name_test "precision" Precision.of_string Precision.to_string
  and flag_test =
    name_test "flag" Flag.of_string Flag.to_string
  and capability_test =
    name_test "capability" Capability.of_string Capability.to_string
  in [
    precision_test (Syntactic, "syntactic");
    precision_test (Resolved, "resolved");
    unknown_test "precision" Precision.of_string;
    flag_test (Type_only, "type_only");
    flag_test (Lazy, "lazy");
    flag_test (Guarded, "guarded");
    flag_test (Dynamic, "dynamic");
    unknown_test "flag" Flag.of_string;
    capability_test (File_imports, "file_imports");
    capability_test (Imported_names, "imported_names");
    capability_test (Package_imports, "package_imports");
    capability_test (Unresolved, "unresolved");
    capability_test (Context_flags, "context_flags");
    capability_test (File_inventory, "file_inventory");
    unknown_test "capability" Capability.of_string;
  ]


(** {1 Properties} *)

(** Generators for random files that respect the documented constraints, so that
    they read back after writing. *)
module Gen = struct
  open QCheck2.Gen

  (** [distinct key xs] is [xs] without the elements whose [key] an earlier
      element already has. *)
  let rec distinct key = function
    | []        -> []
    | x :: rest ->
      x :: distinct key (List.filter (fun y -> key y <> key x) rest)

  (** A non-empty printable string. *)
  let non_empty =
    string_size ~gen:printable (int_range 1 6)

  (** A short list of [gen] without repeated elements. *)
  let set gen =
    map (distinct Fun.id) (list_small gen)

  (** A path. *)
  let path =
    map path_of_string Generators.path

  (** An artifact of any kind. *)
  let artifact =
    oneof [
      map (fun p -> Artifact.File p) path;
      map (fun s -> Artifact.Package s) non_empty;
    ]

  (** Evidence. *)
  let evidence =
    let+ line = option (int_range 1 100000)
    and+ text = option non_empty in
    { Evidence.line; text }

  (** A fact. *)
  let fact =
    let+ subject = path
    and+ object_ = artifact
    and+ precision = oneof_list Precision.[Syntactic; Resolved]
    and+ names = set non_empty
    and+ flags = set (oneof_list Flag.[Type_only; Lazy; Guarded; Dynamic])
    and+ evidence = evidence in
    {
      Fact.subject;
      object_;
      precision;
      names;
      flags;
      evidence;
    }

  (** An unresolved record. *)
  let unresolved =
    let+ subject = path
    and+ target = non_empty
    and+ evidence = evidence in
    { Unresolved.subject; target; evidence }

  (** A header. *)
  let provider =
    let+ name = non_empty
    and+ version = non_empty
    and+ languages = set non_empty
    and+ capabilities =
      set
        (oneof_list
           Capability.[
             File_imports;
             Imported_names;
             Package_imports;
             Unresolved;
             Context_flags;
             File_inventory;
           ]) in
    { Provider.name; version; languages; capabilities }

  (** File records with distinct paths. *)
  let files =
    let file =
      let+ path = path
      and+ language = option non_empty in
      { File.path; language }
    in
    let key (f : File.t) = Path.to_string f.path in
    map (distinct key) (list_small file)

  (** A whole file. *)
  let facts =
    let+ provider = provider
    and+ files = files
    and+ facts = list_small fact
    and+ unresolved = list_small unresolved in
    { Facts.provider; files; facts; unresolved }
end

(** Check that a file printed by [Facts.to_string] reads back to the same
    value. *)
let round_trip_test =
  Util.property_test
    ~name:"of_string (to_string t) = Ok t"
    ~print:Facts.to_string
    Gen.facts
    (fun t ->
       match Facts.of_string (Facts.to_string t) with
       | Ok t'   -> t' = t
       | Error e -> failwith (Facts.error_to_string e))


(** {1 Test runner} *)

let () =
  Alcotest.run ~compact:true "Facts"
    [
      ("valid", valid_tests);
      ("invalid", invalid_tests);
      ("error text", error_text_tests);
      ("vocabulary", vocabulary_tests);
      ("round-trip", [round_trip_test]);
    ]
