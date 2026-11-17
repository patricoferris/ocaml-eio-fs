# Oracles

What the tests compare against, and where each came from.

| oracle | source | regenerate |
|--------|--------|------------|
| `lib/kernel/fs.csv`, read by `Eio_fs.Conformance` and `os/` | Linux and macOS kernels, recorded through `Unix` and eio_posix | `REGEN=1 dune build @lib/kernel/regen-traces`; see `os/README.md` |
| `litmus/` | Ferrite's models, litmus tests and paper, pinned to an upstream revision | `raco test` upstream; see `litmus/README.md` |
| `stat/traces/linux.csv`, read by `test_memfs.ml` | times, link and block counts on Linux's tmpfs and ext4, recorded through eio_posix | `REGEN=1 dune build @test/stat/regen-traces`; see `stat/README.md` |
| `reference/`, `envelope/`, `refm/` | independent models written by hand from the documents each header names | nothing recorded |
| `unix/`, and `absolute links as eio_posix` in `test_memfs.ml` | the kernel of the machine running the tests, live | nothing committed |
