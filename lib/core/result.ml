(** The standard [Result] module, plus binding operators. *)

include Stdlib.Result

module Syntax = struct
  let ( let* ) = bind
  let ( let+ ) x f = map f x
end
