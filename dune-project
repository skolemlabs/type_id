(lang dune 3.11)

(name type_id)

(version 0.0.1)

(generate_opam_files true)

(source
 (github skolemlabs/type_id))

(authors "Zach Baylin <zbaylin@gmail.com>")

(maintainers
 "Zach Baylin <zbaylin@gmail.com>"
 "Trent Small <sixstring982@gmail.com>")

(license MIT)

(documentation https://skolemlabs.github.io/type_id)

(package
 (name type_id)
 (synopsis
  "Type identifiers, useful for runtime type-safe casting/coersions")
 (depends
  (ocaml
   (>= 4.0.3))
  (dune
   (>= 3.1))
  (type_eq
   (>= 0.0.1))))
