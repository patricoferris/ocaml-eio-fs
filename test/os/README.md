# Kernel traces

`traces/fs.csv` holds what Linux and macOS answer to each scenario of
`seq/os_seq.ml`, recorded straight from the kernel through `Unix` and through
eio_posix. `test.ml` replays every scenario on the in-memory filesystem and
compares.

## Regenerate

On macOS, with Docker running:

    REGEN=1 dune build @test/os/regen-traces

`scripts/generate.sh` records macOS natively in `$TMPDIR` and Linux in the
Docker image it pins by digest, on a tmpfs, both as an ordinary user with a
supplementary group. dune promotes the result into `traces/fs.csv`.

## Provenance

The trace starts with `# key: value` lines, written by `generate.sh`:

| key | value |
|-----|-------|
| `script` | the script that recorded it |
| `script revision` | the last commit to `scripts/` or `seq/`, noting uncommitted changes |
| `script digest` | MD5 of `scripts/record.ml` then `seq/os_seq.ml` |
| `recorded` | the date, UTC |
| `macos os`, `linux os` | the OS version and `uname -srm`; Linux's is Docker's VM kernel |
| `linux image` | the Docker image, by digest |
| `macos filesystem`, `linux filesystem` | the type, and whether a name's case matters |
| `macos mount`, `linux mount` | the mount options |
| `macos user`, `linux user` | uid and groups |
| `macos eio_posix`, `linux eio_posix` | the eio_posix version in the opam switch that built the recorder |

The `trace` test fails when a line is missing, when the digest is not that of
the current `record.ml` and `os_seq.ml` (the code that decides every outcome),
or when the image is not the one `generate.sh` pins. It cannot check the
revision: tests run in dune's build directory, often from a release tarball,
with no git history. A failing check means re-recording; never edit a value by
hand.

The trace committed in 53f53c4 was recorded before `generate.sh` wrote these
lines. What git and the outcomes establish is filled in, the rest says
`unknown`: its scripts first appear in that same commit and have not changed
since, the digest is theirs, and the outcomes show both users were not root
(the kernels deny permission checks root passes) and the macOS filesystem
folds case.
