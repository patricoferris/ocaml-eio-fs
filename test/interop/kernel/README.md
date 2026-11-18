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
`traces/fs.csv`, beside its `traces/provenance`.

## Provenance

[`traces/provenance`](traces/provenance) says where the trace comes from: the
revision, date, user and host, the macOS version, the Linux kernel of Docker's
VM and the image, each filesystem with its mount options and whether a name's
case matters, each user's groups, and the eio_posix the recorder was built
with (from [`scripts/probe.sh`](scripts/probe.sh)), and the MD5 of the trace
and of `generate.sh`, `record.ml` and `lib/kernel/eio_fs_kernel.ml`, the code
that decides every outcome. Regenerating rewrites it and drops the
`# key: value` lines the trace recorded before write.sh starts with.

[`test/provenance`](../../provenance) fails when the trace or a pinned input
differs from what the file records. A failing check means re-recording; never
edit a value by hand.
