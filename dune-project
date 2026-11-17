(lang dune 3.21)

(using mdx 0.4)

(cram enable)

(name eio-fs)

(generate_opam_files true)
(implicit_transitive_deps false)

(license ISC)
(authors "Thomas Gazagnaire <thomas@gazagnaire.org>" "Thomas Leonard" "Patrick Ferris")
(maintainers "Thomas Gazagnaire <thomas@gazagnaire.org>")

(source (github samoht/ocaml-eio-fs))

(package
 (name eio-fs)
 (synopsis "In-memory filesystem for Eio, to test code under filesystem errors and power cuts")
 (tags (eio filesystem in-memory))
 (description
  "eio-fs is an in-memory filesystem for Eio, to test code under
filesystem errors and power cuts. Each test gets a fresh filesystem of its
own, used through the ordinary Eio.Path API, so the code under test does not
change. The checks of Eio_fs.Chaos run that code again and again, each time
with a different filesystem error or power cut.")
 (depends
  (hermetic (>= 0.1.0))
  (ocaml (>= 5.4))
  (eio (>= 1.7))
  fmt
  cstruct
  optint
  mtime
  psq
  domain-local-await
  uucp
  uunf
  (cmdliner (>= 1.1.0))
  (eio_main (>= 1.7))
  (conf-linux-libc-dev (= :os linux))
  (mdx (and :with-test (>= 2.3.0)))
  (alcotest (and :with-test (>= 1.7.0)))
  (alcobar :with-test)
  (eio_posix (and :with-test (>= 1.7)))))
