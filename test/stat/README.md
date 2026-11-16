# Stat traces

`traces/linux.csv` holds what Linux answers to each workload of
`workload/stat_workload.ml`: which times move at each operation, and the link
and block counts of a small tree. `../test_memfs.ml` runs every workload on
the in-memory filesystem and compares.

## Regenerate

With Docker running:

    REGEN=1 dune build @test/stat/regen-traces

`scripts/generate.sh` records in the Docker image it pins by digest, through
the eio_posix of the opam switch dune builds in, as an ordinary user: once on
a tmpfs and once on an ext4 image with 4096-byte blocks, as on a disk, that
the container loop-mounts, which takes `docker run --privileged`. Each row is
a workload, then its answer on each filesystem. dune promotes the result into
`traces/linux.csv`.

`relatime` moving atime again once a day is not recorded: it takes a day.

## Provenance

The trace starts with `# key: value` lines, written by `generate.sh`:

| key | value |
|-----|-------|
| `script` | the script that recorded it |
| `script revision` | the last commit to `scripts/` or `workload/`, noting uncommitted changes |
| `script digest` | MD5 of `scripts/record.ml` then `workload/stat_workload.ml` |
| `recorded` | the date, UTC |
| `linux os` | `uname -srm`: Docker's VM kernel |
| `linux image` | the Docker image, by digest |
| `linux user` | uid and groups |
| `linux eio_posix` | the eio_posix version the recorder was built with |
| `tmpfs`, `ext4` | the type and mount options; for ext4, how it was made |

The `stat trace` test fails when a line is missing or not measured, when the
digest is not that of the current `record.ml` and `stat_workload.ml`, or when
the image is not the one `generate.sh` pins. A failing check means
re-recording; never edit a value by hand.
