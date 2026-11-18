# Kernel traces

`traces/fs.csv` holds what Linux and macOS answer to each scenario of
[`lib/kernel/eio_fs_kernel.ml`](../../../lib/kernel/eio_fs_kernel.ml),
recorded straight from the kernel through `Unix` and through eio_posix. The
library ships both. `test.ml` replays every scenario on the in-memory
filesystem and compares.

## Regenerate

On macOS, with Docker running:

    REGEN=1 dune build @test/interop/kernel/regen-traces

`scripts/generate.sh` records macOS natively in `$TMPDIR` and Linux in the
Docker image it pins by digest, on a tmpfs, both as an ordinary user with a
supplementary group. Both run the eio_posix of the opam switch dune builds in:
Linux installs that version in the image. dune promotes the result into
`traces/fs.csv`.

## Provenance

The trace starts with `# key: value` lines, written by `generate.sh`:

| key | value |
|-----|-------|
| `script` | the script that recorded it |
| `script revision` | the last commit to `scripts/` or the interpreter, noting uncommitted changes |
| `script digest` | MD5 of `scripts/record.ml` then `lib/kernel/eio_fs_kernel.ml` |
| `recorded` | the date, UTC |
| `macos os`, `linux os` | the OS version and `uname -srm`; Linux's is Docker's VM kernel |
| `linux image` | the Docker image, by digest |
| `macos filesystem`, `linux filesystem` | the type, and whether a name's case matters |
| `macos mount`, `linux mount` | the mount options |
| `macos user`, `linux user` | uid and groups |
| `macos eio_posix`, `linux eio_posix` | the eio_posix version the recorder was built with |

The `trace` test fails when a line is missing, when the digest is not that of
the current `record.ml` and `eio_fs_kernel.ml` (the code that decides every outcome),
or when the image is not the one `generate.sh` pins. It cannot check the
revision: tests run in dune's build directory, often from a release tarball,
with no git history. A failing check means re-recording; never edit a value by
hand.
