(lang dune 3.18)
(version 0.0.1)
(generate_opam_files)
(name alexis)

(source
 (github kiblitz/alexis))
(license MIT)
(maintainers glee)
(authors glee)

(package
 (name alexis)
 (synopsis "a single-pass lexer library powered by regex deterministic finite automata")
 (description "a single-pass lexer library powered by regex deterministic finite automata")
 (depends
  "core"
  "core_kernel"
  ("dune"
   (>= "3.18.1"))
  ("ocaml"
   (>= "5.3.0"))
  "ppx_jane"))
