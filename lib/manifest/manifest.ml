(** The manifest.  See [manifest.mli]. *)

open Perpend_core

let version = 1


(** {1 Modules} *)

(** [has_control s] is [true] iff [s] has a control character: a byte below the
    space, or the delete byte. *)
let has_control s =
  String.exists (fun c -> c < ' ' || c = '\x7f') s

module Id = struct
  type t = string list

  let equal =
    List.equal String.equal

  let hash = Hashtbl.hash

  let of_string s =
    let segments = String.split_on_char '/' s in
    if List.mem "" segments then
      Error "empty segment"
    else if String.contains s '*' then
      Error "'*' is not allowed"
    else if has_control s then
      Error "control characters are not allowed"
    else
      Ok segments

  let to_string =
    String.concat "/"
end

module Module = struct
  type contents =
    | Paths of Glob.t list
    | Packages of string list

  type t = {
    id : Id.t;
    contents : contents;
  }
end

type t = {
  modules : Module.t list;
}


(** {1 Errors} *)

type error = {
  line : int;
  location : string;
  message : string;
}

let error_to_string { line; location; message } =
  if location = "" then
    Printf.sprintf "line %d: %s" line message
  else
    Printf.sprintf "line %d: %s: %s" line location message

(** One step of a location, from a node to one of its children.  A location is
    the list of steps from the root, innermost first, so that a step is added in
    constant time and the text is built only for an error. *)
type step =
  | Key of string
  (** The key, as written. *)
  | Item of int
  (** The index of the item, from 0. *)

(** The location of the root of the document. *)
let root : step list = []

(** [at_key ~location key] is the location of [key] under [location]. *)
let at_key ~location key =
  Key key :: location

(** [at_item ~location i] is the location of the item [i] of the sequence at
    [location]. *)
let at_item ~location i =
  Item i :: location

(** [location_to_string location] is [location] as an error prints it:
    ["modules.core.paths[1]"].  A key with a control character is escaped as an
    OCaml string literal, so that the text stays on one line. *)
let location_to_string location =
  let buffer = Buffer.create 64 in
  List.rev location |> List.iteri
    (fun i step ->
       match step with
       | Key key ->
         if i > 0 then
           Buffer.add_char buffer '.';
         Buffer.add_string buffer
           (if has_control key then String.escaped key else key)
       | Item idx ->
         Buffer.add_string buffer (Printf.sprintf "[%d]" idx));
  Buffer.contents buffer

(** Raised while reading, with the error. *)
exception Invalid of error

(** [fail ~line ~location fmt] raises [Invalid] with the message [fmt]. *)
let fail ~line ~location fmt =
  Printf.ksprintf
    (fun message ->
       let location = location_to_string location in
       raise (Invalid { line; location; message }))
    fmt


(** {1 The YAML tree}

    The YAML library's [Yaml.of_string] does not fit a strict reader:

    - it reads the root node of the first document and stops, so text after it,
      a second document included, passes unread;
    - its syntax errors carry no position;
    - it reads plain scalars by the YAML 1.1 rules, so [yes] becomes a boolean.

    So this walk over the parser's events reads the whole input, keeps the line
    of every node, and keeps every scalar as text. *)

(** A node of the document. *)
type node = {
  line : int;
  (** The line where the node starts, from 1. *)
  shape : shape;
  (** What the node is: a scalar, a sequence or a mapping. *)
}

(** The kinds of nodes. *)
and shape =
  | Scalar of {
      text : string;
      (** The scalar as text. *)
      plain : bool;
      (** Whether it is a plain scalar: written without quotes and without a
          block indicator. *)
    }
  (** A scalar. *)
  | Sequence of node list
  (** The items, in order. *)
  | Mapping of entry list
  (** The entries, in order. *)

(** One entry of a mapping. *)
and entry = {
  key_line : int;
  (** The line where the key starts, from 1. *)
  key : string;
  (** The text of the key; the reader accepts only scalar keys. *)
  value : node;
  (** The value. *)
}

(** The shape of the YAML library's error message: the problem, the offending
    byte, the byte offset of the problem and the return code. *)
let error_shape =
  Re.compile
    (Re.Perl.re
       ({|^error calling parser: (.*) |}
        ^ {|character -?\d+ position (\d+) returned: -?\d+$|}))

(** [parse_error m] is the problem and the byte offset from the YAML library's
    syntax error message [m], or [None] when [m] has another shape.  The library
    sets the offset for a byte that it cannot read, such as a control character,
    and leaves it at zero for a syntax error. *)
let parse_error m =
  Re.exec_opt error_shape m |> Option.map
    (fun groups ->
       let problem = Re.Group.get groups 1
       and offset = int_of_string (Re.Group.get groups 2) in
       (problem, offset))

(** [line_of_offset s n] is the line of the byte [n] of [s], from 1. *)
let line_of_offset s n =
  let n = min n (String.length s) in
  let rec count line i =
    if i = n then
      line
    else
      count (line + (if s.[i] = '\n' then 1 else 0)) (i + 1)
  in
  count 1 0

(** [events_of_string s] is a function that reads the YAML text [s] event by
    event: each call is the next event and the line where it starts, from 1.

    A byte that the parser cannot read, such as a control character, is reported
    at its line.  A syntax error is reported at the last line that the parser
    read, since the library does not tell where the parser stopped. *)
let events_of_string s =
  let parser =
    match Yaml.Stream.parser s with
    | Ok parser      -> parser
    | Error (`Msg m) -> fail ~line:1 ~location:root "%s" m
  in
  let last_line = ref 1 in
  fun () ->
    match Yaml.Stream.do_parse parser with
    | Ok (event, pos) ->
      last_line := pos.end_mark.line + 1;
      (event, pos.start_mark.line + 1)
    | Error (`Msg m) ->
      let problem, line =
        match parse_error m with
        | Some (problem, offset) when offset > 0 ->
          (problem, line_of_offset s offset)
        | Some (problem, _) -> (problem, !last_line)
        | None              -> (m, !last_line)
      in
      fail ~line ~location:root "%s" problem

(** [check_bare ~line ~location ~anchor ~tag] fails iff the node at [line] and
    [location] has an anchor or a tag.  The reader accepts YAML as a tree of
    mappings, lists and text, and rejects the features of YAML, such as
    anchors and tags, that go beyond that tree. *)
let check_bare ~line ~location ~anchor ~tag =
  if Option.is_some anchor then
    fail ~line ~location "anchors are not supported";
  if Option.is_some tag then
    fail ~line ~location "tags are not supported"

(** [tree_of_string s] is the root node of the YAML text [s].

    The parser gives the events of the text by this grammar, and the walk
    follows it:

    - stream: [STREAM-START document* STREAM-END];
    - document: [DOCUMENT-START node DOCUMENT-END];
    - node: [ALIAS | SCALAR | sequence | mapping];
    - sequence: [SEQUENCE-START node* SEQUENCE-END];
    - mapping: [MAPPING-START (node node)* MAPPING-END].

    The walk carries the location of every node, so that every error is located
    by line and by keys.  The one exception is a syntax error: the parser stops
    before the node is built, so there is no location. *)
let tree_of_string s =
  let module Event = Yaml.Stream.Event in
  let next_event = events_of_string s in

  (* [node ~location (event, line)] is the node at [location] that [event]
     starts. *)
  let rec node ~location (event, line) =
    match event with
    | Event.Alias _ ->
      fail ~line ~location "aliases are not supported"
    | Event.Scalar { anchor; tag; value; style; _ } ->
      check_bare ~line ~location ~anchor ~tag;
      { line; shape = Scalar { text = value; plain = (style = `Plain) } }
    | Event.Sequence_start { anchor; tag; _ } ->
      check_bare ~line ~location ~anchor ~tag;
      { line; shape = Sequence (items ~location 0 []) }
    | Event.Mapping_start { anchor; tag; _ } ->
      check_bare ~line ~location ~anchor ~tag;
      { line; shape = Mapping (entries ~location []) }
    | _ ->
      assert false

  (* [items ~location i acc] reads the rest of the sequence at [location]: the
     next event is its item [i], and [acc] holds the items read so far, last
     first. *)
  and items ~location i acc =
    match next_event () with
    | Event.Sequence_end, _ -> List.rev acc
    | event                 ->
      let item = node ~location:(at_item ~location i) event in
      items ~location (i + 1) (item :: acc)

  (* [entries ~location acc] reads the rest of the mapping at [location]; [acc]
     holds the entries read so far, last first. *)
  and entries ~location acc =
    match next_event () with
    | Event.Mapping_end, _ -> List.rev acc
    | Event.Scalar { anchor; tag; value = key; _ }, key_line ->
      check_bare ~line:key_line ~location ~anchor ~tag;
      let value = node ~location:(at_key ~location key) (next_event ()) in
      entries ~location ({ key_line; key; value } :: acc)
    | Event.Alias _, line ->
      fail ~line ~location "aliases are not supported"
    | _, line ->
      fail ~line ~location "key must be a string"
  in

  begin match next_event () with
    | Event.Stream_start _, _ -> ()
    | _                       -> assert false
  end;
  match next_event () with
  | Event.Stream_end, _ -> fail ~line:1 ~location:root "empty document"
  | Event.Document_start _, _ ->
    let tree = node ~location:root (next_event ()) in
    begin match next_event () with
      | Event.Document_end _, _ -> ()
      | _                       -> assert false
    end;
    begin match next_event () with
      | Event.Stream_end, _ -> tree
      | Event.Document_start _, line ->
        fail ~line ~location:root "second document"
      | _ -> assert false
    end
  | _ -> assert false


(** {1 Reading the tree} *)

(** [mapping ~location node] is the entries of [node], which must be a mapping
    with no key twice. *)
let mapping ~location node =
  match node.shape with
  | Mapping entries ->
    let seen = Hashtbl.create 16 in
    (* [check entries] fails at the first key of [entries] that an earlier entry
       has. *)
    let rec check = function
      | []        -> ()
      | e :: rest ->
        if Hashtbl.mem seen e.key then
          fail ~line:e.key_line ~location "duplicate key %S" e.key
        else begin
          Hashtbl.add seen e.key ();
          check rest
        end
    in
    check entries;
    entries
  | _ -> fail ~line:node.line ~location "must be a mapping"

(** [sequence ~location node] is the items of [node], which must be a
    sequence. *)
let sequence ~location node =
  match node.shape with
  | Sequence items -> items
  | _              -> fail ~line:node.line ~location "must be a list"

(** [text ~location node] is the text of [node], which must be a scalar. *)
let text ~location node =
  match node.shape with
  | Scalar { text; _ } -> text
  | _                  -> fail ~line:node.line ~location "must be a string"

(** [only ~location ~keys entries] fails at the first key of [entries] that is
    not one of [keys]. *)
let only ~location ~keys entries =
  entries |> List.iter
    (fun e ->
       if not (List.mem e.key keys) then
         fail ~line:e.key_line ~location "unknown key %S" e.key)

(** [find key entries] is the value of [key] in [entries], if any. *)
let find key entries =
  Option.map
    (fun e -> e.value)
    (List.find_opt (fun e -> e.key = key) entries)

(** [take ~line ~location key entries] is the value of [key] in [entries], or a
    failure at the mapping, which starts at [line]. *)
let take ~line ~location key entries =
  match find key entries with
  | Some value -> value
  | None       -> fail ~line ~location "missing key %S" key

(** [items_of ~location ~what node] is the items of the sequence [node], each as
    its line, its location and its text.  An empty sequence is an error that
    names an item by [what]: ["pattern"] or ["package"]. *)
let items_of ~location ~what node =
  let items = sequence ~location node in
  if items = [] then
    fail ~line:node.line ~location "at least one %s expected" what;
  items |> List.mapi
    (fun i item ->
       let location = at_item ~location i in
       (item.line, location, text ~location item))


(** {1 Reading the manifest} *)

(** [check_version ~location node] fails unless [node] is the integer [version],
    written exactly as [string_of_int] prints it. *)
let check_version ~location node =
  let given =
    match node.shape with
    | Scalar { text; plain = true } ->
      begin match int_of_string_opt text with
        | Some v when string_of_int v = text -> Some v
        | _                                  -> None
      end
    | _ -> None
  in
  match given with
  | None   -> fail ~line:node.line ~location "must be a plain integer"
  | Some v ->
    if v <> version then
      fail ~line:node.line ~location
        "unsupported version %d, expected %d" v version

(** The patterns and the packages claimed so far, so that none is in two
    modules. *)
type seen = {
  patterns : (string, string) Hashtbl.t;
  (** Maps each pattern to the id of the module that has it. *)
  packages : (string, string) Hashtbl.t;
  (** Maps each package to the id of the module that has it. *)
}

(** One item of the contents of a module, as the manifest writes it. *)
type item =
  | Pattern of string
  (** A pattern, by its text. *)
  | Package of string
  (** A package, by its name. *)

(** [claim seen ~module_id ~line ~location item] records in [seen] that the
    module [module_id] has [item], written at [line] and [location], or fails
    when a module has it already.

    For example, [claim seen ~module_id:"core" ~line:5
    ~location:[Item 1; Key "paths"; Key "core"; Key "modules"]
    (Pattern "src/**")] records that the pattern ["src/**"] belongs to [core].
    It fails when [core] has the pattern already, or when another module has
    it. *)
let claim seen ~module_id ~line ~location item =
  let table, what, name =
    match item with
    | Pattern p -> (seen.patterns, "pattern", p)
    | Package p -> (seen.packages, "package", p)
  in
  match Hashtbl.find_opt table name with
  | Some prev when prev = module_id ->
    fail ~line ~location "duplicate %s %S" what name
  | Some prev ->
    fail ~line ~location "%s %S already in module %S" what name prev
  | None ->
    Hashtbl.add table name module_id

(** [module_of seen ~location entry] is the module declared by [entry] of the
    [modules] mapping; [location] is the location of that mapping. *)
let module_of seen ~location { key_line; key; value = node } =
  let location = at_key ~location key in
  let id =
    match Id.of_string key with
    | Ok id   -> id
    | Error e -> fail ~line:key_line ~location "invalid module id: %s" e
  in

  let entries = mapping ~location node in
  only ~location ~keys:["paths"; "external"] entries;

  let contents =
    match find "paths" entries, find "external" entries with
    | Some paths_node, None ->
      let patterns =
        let location = at_key ~location "paths" in
        items_of ~location ~what:"pattern" paths_node |> List.map
          (fun (line, location, s) ->
             if has_control s then
               fail ~line ~location "control characters are not allowed";
             let pattern =
               match Glob.of_string s with
               | Ok p    -> p
               | Error e -> fail ~line ~location "invalid pattern: %s" e
             in
             claim seen ~module_id:key ~line ~location (Pattern s);
             pattern)
      in
      Module.Paths patterns
    | None, Some external_node ->
      let packages =
        let location = at_key ~location "external" in
        items_of ~location ~what:"package" external_node |> List.map
          (fun (line, location, name) ->
             if name = "" then
               fail ~line ~location "empty package name";
             if has_control name then
               fail ~line ~location "control characters are not allowed";
             claim seen ~module_id:key ~line ~location (Package name);
             name)
      in
      Module.Packages packages
    | Some _, Some _ ->
      fail ~line:node.line ~location {|either "paths" or "external", not both|}
    | None, None ->
      fail ~line:node.line ~location {|"paths" or "external" expected|}
  in
  { Module.id; contents }

(** [manifest_of_tree tree] is the manifest whose document is [tree].  The
    version is checked before the keys, so that a manifest of a later version
    is reported as such and not by its unknown keys. *)
let manifest_of_tree tree =
  let entries = mapping ~location:root tree in
  check_version ~location:(at_key ~location:root "version")
    (take ~line:tree.line ~location:root "version" entries);
  only ~location:root ~keys:["version"; "modules"] entries;

  let modules_node = take ~line:tree.line ~location:root "modules" entries in
  let location = at_key ~location:root "modules" in
  let modules = mapping ~location modules_node in
  if modules = [] then
    fail ~line:modules_node.line ~location "at least one module expected";
  let seen = { patterns = Hashtbl.create 16; packages = Hashtbl.create 16 } in
  { modules = List.map (module_of seen ~location) modules }

(** [check_utf_8 s] fails at the first line of [s] that is not valid UTF-8.

    The parser checks the bytes too, but it also accepts UTF-16 after a byte
    order mark and would read such a file; a manifest must be UTF-8. *)
let check_utf_8 s =
  if not (String.is_valid_utf_8 s) then
    String.split_on_char '\n' s |> List.iteri
      (fun i line ->
         if not (String.is_valid_utf_8 line) then
           fail ~line:(i + 1) ~location:root "not valid UTF-8")

let of_string s =
  try
    check_utf_8 s;
    Ok (manifest_of_tree (tree_of_string s))
  with Invalid e ->
    Error e
