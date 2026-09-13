(** Repo-relative file paths.  See [path.mli]. *)

open Result.Syntax

(** A validated path. *)
type t = {
  source : string;
  (** The path as written. *)
  segments : string list;
  (** [source] split on ['/']. *)
}

(** [check_segment s] is an error iff [s] cannot be a segment of a path. *)
let check_segment s =
  if s = "" then
    Error "empty segment"
  else if s = "." || s = ".." then
    Error (Printf.sprintf "'%s' is not allowed" s)
  else
    Ok ()

let of_string source =
  if source = "" then
    Error "empty"
  else if source.[0] = '/' then
    Error "must not start with '/'"
  else if source.[String.length source - 1] = '/' then
    Error "must not end with '/'"
  else
    let segments = String.split_on_char '/' source in
    let rec check_all = function
      | []        -> Ok ()
      | s :: rest ->
        let* () = check_segment s in
        check_all rest
    in
    let+ () = check_all segments in
    { source; segments }

let to_string t =
  t.source

let segments t =
  t.segments
