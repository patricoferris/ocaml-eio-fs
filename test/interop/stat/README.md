# Stat traces

`traces/linux.csv` holds what Linux answers to each workload of
`workload/stat_workload.ml`: which times move at each operation, and the link
and block counts of a small tree. `test_stat.ml` runs every workload on the
in-memory filesystem and compares.

## Regenerate

With Docker running:

    REGEN=1 dune build @test/interop/stat/regen-traces

`scripts/generate.sh` records in the Docker image it pins by digest, through
the eio_posix of the opam switch dune builds in, as an ordinary user: once on
a tmpfs and once on an ext4 image with 4096-byte blocks, as on a disk, that
[`scripts/ext4.sh`](scripts/ext4.sh) makes and the container loop-mounts,
which takes `docker run --privileged`. Each row is
a workload, then its answer on each filesystem. dune promotes the result into
`traces/linux.csv`, beside its `traces/provenance`.

`relatime` moving atime again once a day is not recorded: it takes a day.

## Provenance

[`traces/provenance`](traces/provenance) says where the trace comes from: the
revision, date, user and host, the Linux kernel of Docker's VM and the image,
the container's user, the tmpfs and the ext4 image with their mount options
and how the image was made, and the eio_posix the recorder was built with
(from [`scripts/probe.sh`](scripts/probe.sh)), and the MD5 of the trace and of
`generate.sh`, `record.ml` and `workload/stat_workload.ml`. Regenerating
rewrites it and drops the `# key: value` lines the trace recorded before
write.sh starts with.

[`test/provenance`](../../provenance) fails when the trace or a pinned input
differs from what the file records. A failing check means re-recording; never
edit a value by hand.
