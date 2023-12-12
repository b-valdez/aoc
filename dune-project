(lang dune 3.10)

(name aoc)

(package
 (name aoc)
 (depends core core_kernel iter ppx_jane re angstrom ocaml dune shexp))

(using menhir 2.0)

(implicit_transitive_deps false)

(generate_opam_files)

(dialect
 (name bundle_blob)
 (implementation
  ; .blob are our inputs
  (extension blob)
  (preprocess
   (run bundle_blob %{input-file}))))
