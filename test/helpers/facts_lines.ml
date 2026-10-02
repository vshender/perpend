(** The lines of a facts file, for fixtures: [header] and the builders of
    records each give one JSONL line, and [facts] reads a list of them. *)

open Perpend_facts

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
