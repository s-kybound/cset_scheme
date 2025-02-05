(lang dune 3.17)

(name cset_scheme)

(generate_opam_files true)

(source
 (github s-kybound/cset_scheme))

(authors "Kyriel Abad <abadkyriel@gmail.com>")

(maintainers "Kyriel Abad <abadkyriel@gmail.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name cset_scheme)
 (synopsis "A Scheme implementation in OCaml using the CSET notional machine")
 (description "A parser and a state-to-state evaluator for R7RS Scheme 
  using states of (Control (instructions and s-expressions), Stash (operand stack), 
  Environment (value environment), Transformers (macro environment)).")
 (depends ocaml)
 (tags
  ("scheme" "lisp" "interpreter" "parser" "evaluator" "r7rs")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
