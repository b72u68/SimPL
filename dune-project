(lang dune 3.4)

(name simpl)

(authors "My Dinh")

(maintainers "My Dinh")

(generate_opam_files true)

(source
  (github b72u68/SimPL))

(package
  (name simpl)
  (synopsis "Simple programming language")
  (description "Simple programming language for CS536: Science of Programming at Illinois Tech")
  (tags
    (topics "compiler" "programming language" "verification"))
  (depends ocaml dune utop menhir ocamllex))

(using menhir 2.1)
