(lang dune 3.0)

(name perpend)

(generate_opam_files true)

(license ISC)

(authors "Vadzim Shender")

(maintainers "vadzim@shender.org")

(source (github vshender/perpend))

(package
 (name perpend)
 (synopsis "Deterministic change-control layer for software architecture")
 (depends
  (ocaml (>= 5.4))
  (alcotest :with-test)
  (qcheck-core (and (>= 0.90) :with-test))
  (qcheck-alcotest (and (>= 0.90) :with-test))))
