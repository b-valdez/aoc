(lang dune 3.17)

(name aoc)

(package
 (name aoc)
 (depends
  core
  core_kernel
  iter
  ppx_jane
  ppx_deriving
  re
  string_dict
  angstrom
  ocaml
  owl
  lp
  lp-glpk
  dune
  (shexp :dev)
  backoff
  saturn
  moonpool
  picos
  picos_std_sync
  (embedded_ocaml_templates :dev)))

(implicit_transitive_deps false)

(generate_opam_files)