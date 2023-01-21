(lang dune 3.5)

(name sharik)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "x86monkey")

(maintainers "x86monkeyName")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name sharik)
 (synopsis "Chess engine in ocaml")
 (description "Chess engine in ocaml")
 (depends ocaml dune lwt)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
