(** The pieces of text that the reports share.  See [report.mli]. *)

let count n word =
  if n = 1 then
    Printf.sprintf "%d %s" n word
  else
    Printf.sprintf "%d %ss" n word

let section name files =
  Printf.sprintf "%s (%s)\n" name (count (List.length files) "file")
  ^ String.concat "" (List.map (Printf.sprintf "  %s\n") files)
