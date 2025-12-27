(lang dune 3.18)

(version 0.0.1)

(generate_opam_files)

(name alexis)

(source
 (github kiblitz/alexis))

(license MIT)

(maintainers kiblitz)

(authors kiblitz)

(package
 (name alexis)
 (synopsis
  "an approximately single-pass lexer library powered by regex deterministic finite automata")
 (description
  "Lexers are usually the first phase of a compiler or interpreter where raw characters are converted into meaningful tokens. Alexis provides an API which preprocess generates a DFA from regex configs, allowing it to scan input in an approximately single linear pass.")
 (depends
  "cmdliner"
  "core"
  "core_kernel"
  ("dune"
   (>= "3.18.1"))
  ("ocaml"
   (>= "5.3.0"))
  "ppx_jane"))
