(lang dune 3.6)

(source
 (github astahfrom/prettiest))

(license Apache-2.0)

(authors "Asta Halkj\195\166r From")

(maintainers "andro.from@gmail.com")

(package
 (name prettiest)
 (synopsis "Pretty Printer")
 (description
  "Implementation of Jean-Philippe Bernardy's A Pretty But Not Greedy Printer (Functional Pearl). PACMPL 1(ICFP): 6:1-6:21 (2017) in OCaml")
 (depends base))

(generate_opam_files)
