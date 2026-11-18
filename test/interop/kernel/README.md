# Kernel traces

`traces/fs.csv` holds what Linux, macOS and Windows answer to each scenario
of [`lib/kernel/eio_fs_kernel.ml`](../../../lib/kernel/eio_fs_kernel.ml):
on Linux and macOS straight from the kernel through `Unix` and through
eio_posix, on Windows through eio_windows alone, as OCaml's `Unix` reaches
the C runtime there, not the kernel, once as an administrator and once as a
basic user, who may not make a symbolic link. The library ships the trace;
`Eio_fs_chaos.device` finds a column by its name. `test.ml` replays every
scenario on the in-memory filesystem under each `os` and compares.

## Regenerate

Two hosts record the trace, each its own columns, and keep the others from
the trace before: `record merge` writes a row per scenario, and one a host
did not record says `not recorded`.

On macOS, with Docker running:

    REGEN=1 dune build @test/interop/kernel/regen-traces

`scripts/generate.sh` records macOS natively in `$TMPDIR` and Linux in the
Docker image it pins by digest, on a tmpfs, both as an ordinary user with a
supplementary group. Both run the eio_posix of the opam switch dune builds in:
Linux installs that version in the image.

On Windows the same command runs `scripts/generate-windows.sh`, which
records in a fresh directory of the temporary directory, on NTFS, as the user
running it and, through `runas /trustlevel`
([`scripts/basic-user.sh`](scripts/basic-user.sh)), as a basic user. The
workflow [`windows.yml`](../../../.github/workflows/windows.yml) runs it on
GitHub's `windows-latest` with the released Eio 1.7, and with Eio's main to
tell what it fixed since, after turning Developer Mode off, Windows'
default; it uploads `traces/` as the artifact `windows-traces-1.7`. dune
promotes the result into `traces/fs.csv`, beside its `traces/provenance`.

A change to the scenarios or to the recorder means recording on both hosts.

## Provenance

[`traces/provenance`](traces/provenance) says where the trace comes from: the
revision, date, user and host of the last recording, the macOS version, the
Linux kernel of Docker's VM and the image, each filesystem with its mount
options and whether a name's case matters, each user's groups, and the
eio_posix the recorder was built with (from
[`scripts/probe.sh`](scripts/probe.sh)); the Windows version and runner
image, its filesystem, Developer Mode, each user's symbolic-link privilege
and eio_windows's version (from
[`scripts/windows-host.sh`](scripts/windows-host.sh)); and the MD5 of the
trace and of the code that decides every outcome. A recording keeps the
other host's lines, its revision, date and pins as `posix.*` or `windows.*`
([`scripts/carry.sh`](scripts/carry.sh)).

[`test/provenance`](../../provenance) fails when the trace or a pinned input,
either host's, differs from what the file records. A failing check means
re-recording; never edit a value by hand.
