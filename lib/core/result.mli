(** The standard [Result] module, plus binding operators. *)

include module type of Stdlib.Result


(** {1 Syntax} *)

(** Binding operators over [result], so that code returning [result] reads top
    to bottom:

    {[
      let* x = may_fail () in
      let+ y = may_fail_too x in
      x + y
    ]} *)
module Syntax : sig
  val ( let* ) : ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result
  (** [let* x = r in e] is [e] with [x] bound to the value of [r], or the error
      of [r]. *)

  val ( let+ ) : ('a, 'e) result -> ('a -> 'b) -> ('b, 'e) result
  (** [let+ x = r in e] is [Ok e] with [x] bound to the value of [r], or the
      error of [r]. *)
end
