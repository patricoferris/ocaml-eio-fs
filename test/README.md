# Oracles

What the tests compare against, and where each came from.

| oracle | source | regenerate |
|--------|--------|------------|
| `os/traces/fs.csv` | Linux and macOS kernels, recorded through `Unix` and eio_posix | `REGEN=1 dune build @test/os/regen-traces`; see `os/README.md` |
| `litmus/` | Ferrite's models, litmus tests and paper, pinned to an upstream revision | `raco test` upstream; see `litmus/README.md` |
| times, link and block counts in `test_memfs.ml` | measured through eio_posix on tmpfs and ext4, Linux 7.2 with `relatime`, as its header and commit `0895392` say | no recording script is committed; kernel build, other mount options and uid are unknown |
| `reference/`, `reference2/`, `refm/` | independent models written by hand from the documents each header names | nothing recorded |
| `unix/`, and `absolute links as eio_posix` in `test_memfs.ml` | the kernel of the machine running the tests, live | nothing committed |
