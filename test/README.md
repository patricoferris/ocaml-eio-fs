# Oracles

What the tests compare against, and where each came from.

| oracle | source | regenerate |
|--------|--------|------------|
| `interop/kernel/traces/fs.csv`, read by `Eio_fs_chaos.conformance` and `interop/kernel/` | Linux and macOS kernels, recorded through `Unix` and eio_posix; Windows on GitHub's `windows-latest`, through eio_windows | `REGEN=1 dune build @test/interop/kernel/regen-traces`, on macOS and on Windows, the workflow `windows.yml`; see `interop/kernel/README.md` |
| `interop/windows/traces/paths.csv`, read by `interop/windows/` | Win32 paths through eio_windows's `fs` on `windows-latest` | the workflow `windows.yml` runs `REGEN=1 dune build @test/interop/windows/regen-traces`; see `interop/windows/README.md` |
| `interop/stat/traces/linux.csv`, read by `interop/stat/` | times, link and block counts on Linux's tmpfs and ext4, recorded through eio_posix | `REGEN=1 dune build @test/interop/stat/regen-traces`; see `interop/stat/README.md` |
| `litmus/` | Ferrite's models, litmus tests and paper, pinned to an upstream revision | `raco test` upstream; see `litmus/README.md` |
| `reference/`, `envelope/`, `refm/` and `exhaustive/oracle.ml` | independent models written by hand from the documents each header names | nothing recorded |
| `unix/`, and `absolute links as eio_posix` in `test_memfs.ml` | the kernel of the machine running the tests, live | nothing committed |
