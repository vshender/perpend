(** Helpers shared by the test executables. *)

(** [property_test ~name ?count ~print gen f] is the QCheck2 property [f]
    over [gen], run [count] times (500 by default), as an Alcotest test
    case.  The arguments are those of [QCheck2.Test.make]. *)
let property_test ~name ?(count = 500) ~print gen f =
  QCheck_alcotest.to_alcotest
    (QCheck2.Test.make ~name ~count ~print gen f)
