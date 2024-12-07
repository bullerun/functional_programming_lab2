(lang dune 3.16)

(name functional_programming_lab2)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name functional_programming_lab2)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  (qcheck
   (and
    (>= 0.22)
    :with-test))
  (ounit2
   (and
    (>= 2.2.7)
    :with-test))
  ocaml
  dune)
 (tags
  (topics "to describe" your project)))