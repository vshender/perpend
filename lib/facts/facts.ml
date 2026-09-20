(** Facts from providers.  See [facts.mli]. *)

open Perpend_core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

let protocol = "perpend.facts/1"


(** {1 Helpers shared by the converters} *)

(** [invalid json fmt] fails the conversion of [json] with the message [fmt].
    It raises the same exception as the derived readers, so that one handler in
    [of_string] covers both. *)
let invalid json fmt =
  Printf.ksprintf
    (fun message ->
       Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error message json)
    fmt

(** [string_of_yojson json] is the string in [json], checked to be valid
    UTF-8: Yojson decodes an escape such as ["\uDC00"] into bytes that are
    not.  It replaces the reader from [Primitives]. *)
let string_of_yojson json =
  let s = string_of_yojson json in
  if not (String.is_valid_utf_8 s) then
    invalid json "not valid UTF-8";
  s

(** [non_empty json what s] fails the conversion of [json] iff the string [s],
    described by [what], is empty. *)
let non_empty json what s =
  if s = "" then
    invalid json "empty %s" what

(** [each_once json to_string what xs] fails the conversion of [json] iff an
    element of [xs], described by [what], appears twice. *)
let each_once json to_string what xs =
  let rec check = function
    | []        -> ()
    | x :: rest ->
      if List.mem x rest then
        invalid json "duplicate %s: %s" what (to_string x);
      check rest
  in
  check xs

(** [json_enum what of_string to_string] is the JSON reader and writer of an
    enumerated type, described by [what], whose values are JSON strings named by
    [of_string] and [to_string]. *)
let json_enum what of_string to_string =
  let t_of_yojson json =
    match of_string (string_of_yojson json) with
    | Some v -> v
    | None   -> invalid json "unknown %s" what
  in
  let yojson_of_t v =
    `String (to_string v)
  in
  (t_of_yojson, yojson_of_t)


(** {1 Vocabulary} *)

(** [Path] with JSON conversions, so that [Path.t] can be a field of a derived
    record. *)
module Path = struct
  include Path

  (** [t_of_yojson json] is the path in the JSON string [json]. *)
  let t_of_yojson json =
    match of_string (string_of_yojson json) with
    | Ok p    -> p
    | Error e -> invalid json "not a path: %s" e

  (** [yojson_of_t p] is [p] as a JSON string. *)
  let yojson_of_t p =
    `String (to_string p)
end

module Precision = struct
  type t = Syntactic | Resolved

  let to_string = function
    | Syntactic -> "syntactic"
    | Resolved  -> "resolved"

  let of_string = function
    | "syntactic" -> Some Syntactic
    | "resolved"  -> Some Resolved
    | _           -> None

  let t_of_yojson, yojson_of_t =
    json_enum "precision" of_string to_string
end

module Flag = struct
  type t = Type_only | Lazy | Guarded | Dynamic

  let to_string = function
    | Type_only -> "type_only"
    | Lazy      -> "lazy"
    | Guarded   -> "guarded"
    | Dynamic   -> "dynamic"

  let of_string = function
    | "type_only" -> Some Type_only
    | "lazy"      -> Some Lazy
    | "guarded"   -> Some Guarded
    | "dynamic"   -> Some Dynamic
    | _           -> None

  let t_of_yojson, yojson_of_t =
    json_enum "flag" of_string to_string
end

module Capability = struct
  type t =
    | File_imports
    | Imported_names
    | Package_imports
    | Unresolved
    | Context_flags
    | File_inventory

  let to_string = function
    | File_imports    -> "file_imports"
    | Imported_names  -> "imported_names"
    | Package_imports -> "package_imports"
    | Unresolved      -> "unresolved"
    | Context_flags   -> "context_flags"
    | File_inventory  -> "file_inventory"

  let of_string = function
    | "file_imports"    -> Some File_imports
    | "imported_names"  -> Some Imported_names
    | "package_imports" -> Some Package_imports
    | "unresolved"      -> Some Unresolved
    | "context_flags"   -> Some Context_flags
    | "file_inventory"  -> Some File_inventory
    | _                 -> None

  let t_of_yojson, yojson_of_t =
    json_enum "capability" of_string to_string
end

module Artifact = struct
  type t =
    | File of Path.t
    | Package of string

  (** The JSON shape of an artifact; the derived reader checks the fields. *)
  type shape = {
    kind : string;
    (** ["file"] or ["package"]. *)
    id : string;
    (** The path or the package name. *)
  } [@@deriving yojson]

  (** [t_of_yojson json] is the artifact in the JSON object [json]. *)
  let t_of_yojson json =
    let { kind; id } = shape_of_yojson json in
    match kind with
    | "file"    -> File (Path.t_of_yojson (`String id))
    | "package" -> non_empty json "package id" id; Package id
    | _         -> invalid json "unknown artifact kind: %s" kind

  (** [yojson_of_t t] is [t] as a JSON object. *)
  let yojson_of_t t =
    let kind, id =
      match t with
      | File p    -> ("file", Path.to_string p)
      | Package s -> ("package", s)
    in
    yojson_of_shape { kind; id }
end


(** {1 Records} *)

module Provider = struct
  type t = {
    name : string;
    version : string;
    languages : string list;
    capabilities : Capability.t list;
  } [@@deriving yojson]

  (** [t_of_yojson json] is the derived reader plus the checks the types cannot
      express. *)
  let t_of_yojson json =
    let t = t_of_yojson json in
    non_empty json "provider name" t.name;
    non_empty json "provider version" t.version;
    List.iter (non_empty json "language") t.languages;
    each_once json Fun.id "language" t.languages;
    each_once json Capability.to_string "capability" t.capabilities;
    t
end

module File = struct
  type t = {
    path : Path.t;
    language : string option [@yojson.option];
  } [@@deriving yojson]

  (** [t_of_yojson json] is the derived reader plus the checks the types cannot
      express. *)
  let t_of_yojson json =
    let t = t_of_yojson json in
    Option.iter (non_empty json "language") t.language;
    t
end

module Evidence = struct
  type t = {
    line : int option [@yojson.option];
    text : string option [@yojson.option];
  } [@@deriving yojson]

  (** [t_of_yojson json] is the derived reader plus the checks the types cannot
      express. *)
  let t_of_yojson json =
    let t = t_of_yojson json in
    Option.iter
      (fun n -> if n < 1 then invalid json "line must be at least 1")
      t.line;
    Option.iter (non_empty json "text") t.text;
    t
end

module Fact = struct
  type t = {
    subject : Path.t;
    object_ : Artifact.t [@key "object"];
    precision : Precision.t;
    names : string list;
    flags : Flag.t list;
    evidence : Evidence.t;
  } [@@deriving yojson]

  (** [t_of_yojson json] is the derived reader plus the checks the types cannot
      express. *)
  let t_of_yojson json =
    let t = t_of_yojson json in
    List.iter (non_empty json "name") t.names;
    each_once json Fun.id "name" t.names;
    each_once json Flag.to_string "flag" t.flags;
    t
end

module Unresolved = struct
  type t = {
    subject : Path.t;
    target : string;
    evidence : Evidence.t;
  } [@@deriving yojson]

  (** [t_of_yojson json] is the derived reader plus the checks the types cannot
      express. *)
  let t_of_yojson json =
    let t = t_of_yojson json in
    non_empty json "target" t.target;
    t
end

type t = {
  provider : Provider.t;
  files : File.t list;
  facts : Fact.t list;
  unresolved : Unresolved.t list;
}


(** {1 Reading} *)

type error = {
  line : int;
  message : string;
}

let error_to_string { line; message } =
  Printf.sprintf "line %d: %s" line message

(** Raised while reading one line, with the message for the error. *)
exception Invalid_line of string

(** [fail fmt] raises [Invalid_line] with the message [fmt]. *)
let fail fmt =
  Printf.ksprintf (fun message -> raise (Invalid_line message)) fmt

(** [after marker s] is the part of [s] after the first [marker], or [s] when
    [marker] does not occur in it. *)
let after marker s =
  let n = String.length s and m = String.length marker in
  (* [occurs_at i j] is [true] iff [marker], from its character [j], occurs
     in [s] at [i + j]. *)
  let rec occurs_at i j =
    j = m || (marker.[j] = s.[i + j] && occurs_at i (j + 1))
  in
  let rec find i =
    if i + m > n then
      s
    else if occurs_at i 0 then
      String.sub s (i + m) (n - i - m)
    else
      find (i + 1)
  in
  find 0

(** [take key fields] is the string value of [key] in the JSON object [fields]
    and the other fields, or a failure if [key] is missing, repeated or not a
    string. *)
let take key fields =
  match List.partition (fun (k, _) -> k = key) fields with
  | [(_, `String v)], rest -> (v, rest)
  | [_], _                 -> fail "%S must be a string" key
  | [], _                  -> fail "missing %S" key
  | _                      -> fail "duplicate %S" key

(** [provider_of_fields fields] is the header with the fields [fields], the
    protocol checked and removed. *)
let provider_of_fields fields =
  let found, rest = take "protocol" fields in
  if found <> protocol then
    fail "unknown protocol %S, expected %S" found protocol;
  Provider.t_of_yojson (`Assoc rest)

(** One record of the file, by its ["type"]. *)
type record =
  | Header of Provider.t
  (** The provider header. *)
  | File of File.t
  (** A file record. *)
  | Fact of Fact.t
  (** A fact record. *)
  | Unresolved of Unresolved.t
  (** An unresolved record. *)

(** [record_of_line line] is the record on [line].  A JSON syntax error is
    reported without the position that Yojson puts on a line of its own. *)
let record_of_line line =
  if String.trim line = "" then
    fail "blank line";
  if not (String.is_valid_utf_8 line) then
    fail "not valid UTF-8";
  let json =
    try
      Yojson.Safe.from_string line
    with Yojson.Json_error message ->
      fail "%s" (after "\n" message)
  in
  let kind, rest =
    match json with
    | `Assoc fields -> take "type" fields
    | _             -> fail "object expected"
  in
  match kind with
  | "provider"   -> Header (provider_of_fields rest)
  | "file"       -> File (File.t_of_yojson (`Assoc rest))
  | "fact"       -> Fact (Fact.t_of_yojson (`Assoc rest))
  | "unresolved" -> Unresolved (Unresolved.t_of_yojson (`Assoc rest))
  | _            -> fail "unknown record type %S" kind

(** [conversion_message exn json] is the message for a failed conversion of
    [json].  The derived readers put their own name before ["_of_yojson: "]; it
    is dropped.  A value that is not an object is added after the message, since
    the message alone does not show it; an object is not, since the message
    names the field. *)
let conversion_message exn json =
  let message =
    match exn with
    | Failure message -> after "_of_yojson: " message
    | exn             -> Printexc.to_string exn
  in
  match json with
  | `Assoc _ -> message
  | json     -> Printf.sprintf "%s: %s" message (Yojson.Safe.to_string json)

(** [guard lineno f] is [Ok (f ())], or the error [f] raised, located at
    line [lineno]. *)
let guard lineno f =
  try Ok (f ()) with
  | Invalid_line message ->
    Error { line = lineno; message }
  | Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (exn, json) ->
    Error { line = lineno; message = conversion_message exn json }

(** [lines s] is [s] split on newlines, without the empty piece after a final
    newline. *)
let lines s =
  if s = "" then
    []
  else if String.ends_with ~suffix:"\n" s then
    String.split_on_char '\n' (String.sub s 0 (String.length s - 1))
  else
    String.split_on_char '\n' s

(** [records_of_lines lines] is the records on [lines], each with its line
    number, or the first error.  Tail recursive, since a file can have many
    thousands of lines. *)
let records_of_lines lines =
  let open Result.Syntax in
  let rec read lineno acc = function
    | []           -> Ok (List.rev acc)
    | line :: rest ->
      let* record = guard lineno (fun () -> record_of_line line) in
      read (lineno + 1) ((lineno, record) :: acc) rest
  in
  read 1 [] lines

(** [header_of_records records] is the header, which must be the first record
    and the only header. *)
let header_of_records records =
  let is_header (_, record) =
    match record with
    | Header _ -> true
    | _        -> false
  in
  match records with
  | (_, Header provider) :: rest ->
    begin match List.find_opt is_header rest with
      | Some (lineno, _) ->
        Error { line = lineno; message = "second provider header" }
      | None -> Ok provider
    end
  | (lineno, _) :: _ ->
    Error { line = lineno; message = "provider header expected" }
  | [] ->
    Error { line = 1; message = "provider header expected" }

(** [check_distinct_files records] is an error at the first file record whose
    path an earlier file record has. *)
let check_distinct_files records =
  let seen = Hashtbl.create 64 in
  let rec check = function
    | [] -> Ok ()
    | (lineno, File file) :: rest ->
      let path = Path.to_string file.path in
      if Hashtbl.mem seen path then
        Error { line = lineno; message = "duplicate file record: " ^ path }
      else begin
        Hashtbl.add seen path ();
        check rest
      end
    | _ :: rest -> check rest
  in
  check records

let of_string s =
  let open Result.Syntax in
  let* records = records_of_lines (lines s) in
  let* provider = header_of_records records in
  let+ () = check_distinct_files records in
  let select f = List.filter_map f records in
  {
    provider;
    files = select (function _, File f -> Some f | _ -> None);
    facts = select (function _, Fact f -> Some f | _ -> None);
    unresolved = select (function _, Unresolved u -> Some u | _ -> None);
  }


(** {1 Writing} *)

(** [fields json] is the fields of the JSON object [json]. *)
let fields json =
  match json with
  | `Assoc fields -> fields
  | _             -> assert false

let to_string t =
  let buffer = Buffer.create 4096 in
  (* [record kind extra json] adds one line: the object [json] with the field
     ["type"] and the fields [extra] in front. *)
  let record kind extra json =
    let json = `Assoc (("type", `String kind) :: extra @ fields json) in
    Buffer.add_string buffer (Yojson.Safe.to_string json);
    Buffer.add_char buffer '\n'
  in
  let header = [("protocol", `String protocol)] in
  record "provider" header (Provider.yojson_of_t t.provider);
  t.files |> List.iter (fun f -> record "file" [] (File.yojson_of_t f));
  t.facts |> List.iter (fun f -> record "fact" [] (Fact.yojson_of_t f));
  t.unresolved |> List.iter
    (fun u -> record "unresolved" [] (Unresolved.yojson_of_t u));
  Buffer.contents buffer
