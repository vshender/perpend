(** The [perpend] command line. *)

open Perpend_core
open Perpend_manifest
open Perpend_cli
open Result.Syntax

(** [read_manifest file] is the manifest in [file].  The error names the file
    and, for a manifest error, the line. *)
let read_manifest file =
  let* text =
    try
      if Sys.is_directory file then
        Error (file ^ ": is a directory")
      else
        Ok (In_channel.with_open_bin file In_channel.input_all)
    with
      Sys_error message -> Error message
  in
  Manifest.of_string text |> Result.map_error
    (fun e -> file ^ ": " ^ Manifest.error_to_string e)

(** [paths_of_strings strings] is [strings] as paths, or an error naming the
    first string that is not a path and the problem. *)
let paths_of_strings strings =
  let rec paths_of_strings_aux paths = function
    | []            -> Ok (List.rev paths)
    | first :: rest ->
      begin match Path.of_string first with
        | Ok path -> paths_of_strings_aux (path :: paths) rest
        | Error e -> Error (Printf.sprintf "git ls-files: '%s': %s" first e)
      end
  in
  paths_of_strings_aux [] strings

(** [git_files ()] is the list of the files that git tracks under the current
    directory, in the order of [git ls-files].  A file in conflict is listed
    once, not once per stage of the merge.  When git fails, its own message has
    gone to stderr already; the error gives the exit status. *)
let git_files () =
  let* output =
    try
      let channel =
        Unix.open_process_args_in
          "git" [| "git"; "ls-files"; "-z"; "--deduplicate" |]
      in
      let output = In_channel.input_all channel in
      begin match Unix.close_process_in channel with
        | WEXITED 0    -> Ok output
        | WEXITED code ->
          Error (Printf.sprintf "git ls-files exited with code %d" code)
        | WSIGNALED _ | WSTOPPED _ ->
          Error "git ls-files was killed by a signal"
      end
    with
      Unix.Unix_error (e, _, _) -> Error ("git: " ^ Unix.error_message e)
  in
  (* The output ends with a NUL, or is empty when git tracks no file; either way
     the split has an empty string to drop. *)
  String.split_on_char '\000' output
  |> List.filter (fun s -> s <> "")
  |> paths_of_strings

(** [finish result] is the exit code of a command that ended with [result]:
    0 for [Ok], 1 for [Error].  It prints the error first, if any. *)
let finish = function
  | Ok ()         -> 0
  | Error message ->
    prerr_endline ("perpend: " ^ message);
    1

(** [files manifest_file] runs [perpend files]. *)
let files manifest_file =
  finish begin
    let* manifest = read_manifest manifest_file in
    let+ paths = git_files () in
    print_string (Files.report manifest paths)
  end

(** [path_of_argument s] is the path that a person typed as [s].  Each leading
    [./] is dropped: [./x] names the same file as [x], and a canonical path has
    no [.] segment; shell completion and tools such as [find] write it.  The
    error quotes [s] and names the problem, as [Path] states it. *)
let path_of_argument s =
  let dot_slashes = Re.compile (Re.Perl.re {|^(\./)+|}) in
  s
  |> Re.replace_string dot_slashes ~by:""
  |> Path.of_string
  |> Result.map_error (Printf.sprintf "invalid path '%s': %s" s)

(** [which manifest_file path] runs [perpend which path]. *)
let which manifest_file path =
  finish begin
    let* manifest = read_manifest manifest_file in
    let+ path = path_of_argument path in
    print_string (Which.report manifest path)
  end


(** {1 Command line} *)

open Cmdliner

(** The [--manifest] option. *)
let manifest_arg =
  let doc =
    "The manifest to read.  Its patterns are relative to the current \
     directory."
  in
  Arg.(
    value
    & opt string "perpend.yaml"
    & info ["m"; "manifest"] ~docv:"FILE" ~doc)

(** [exit_codes doc] is the exit codes of a command for its help page: 1,
    documented by [doc], and the codes of cmdliner.  Cmdliner's own code for a
    failed command, 123, is left out: a command here returns 1 instead. *)
let exit_codes doc =
  Cmd.Exit.info 1 ~doc
  ::
  List.filter
    (fun info -> Cmd.Exit.info_code info <> Cmd.Exit.some_error)
    Cmd.Exit.defaults

(** [perpend files]. *)
let files_cmd =
  let doc = "list the files that git tracks, module by module" in
  let exits =
    exit_codes
      "on an error in the manifest, or when git fails or lists a path that \
       is not valid."
  in
  Cmd.v (Cmd.info "files" ~doc ~exits) Term.(const files $ manifest_arg)

(** [perpend which]. *)
let which_cmd =
  let doc = "print the module of one file" in
  let exits = exit_codes "on an error in the manifest or in $(i,PATH)." in
  let path_arg =
    let doc =
      "The path of the file, relative to the current directory; a leading \
       ./ is accepted.  The file need not exist: the verdict comes from the \
       patterns alone."
    in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"PATH" ~doc)
  in
  Cmd.v
    (Cmd.info "which" ~doc ~exits)
    Term.(const which $ manifest_arg $ path_arg)

let () =
  let doc = "deterministic change control for software architecture" in
  let info = Cmd.info "perpend" ~doc ~exits:(exit_codes "on an error.") in
  exit (Cmd.eval' (Cmd.group info [files_cmd; which_cmd]))
