(lang dune 3.14)
(using menhir 2.1)

(name latsi)

(generate_opam_files true)

(source (uri https://moule.informatique.univ-paris-diderot.fr/iglesias/gas6-latsi-ty))

(authors "Yago Iglesias Vázquez" "Tony Ly Soan")

(maintainers "Yago Iglesias Vázquez" "Tony Ly Soan")


(package
 (name latsi)
 (synopsis "Interpreter for the LATSI language")
 (description "TODO")
 (depends ocaml dune (alcotest :with-test) qcheck qcheck-alcotest)
 (tags
  (topics "TODO" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
