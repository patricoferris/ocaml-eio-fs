# Win32 path traces

`traces/paths.csv` holds what Win32 paths answer through the unrestricted
`fs` of eio_windows ([`scripts/win_paths.ml`](scripts/win_paths.ml)):
absolute, rooted, drive-relative and verbatim paths, a trailing dot and a
reserved name, with `Eio.Path.native` and `Eio.Path.split` of each, a rename
to another drive, and what a read through Win32 itself, by OCaml's `Stdlib`,
answers for the same path. `test.ml` replays the paths on the in-memory
filesystem under `` `Windows `` with drives C: and D:, and compares all but
Win32's own column, which is there for the reader. The kernel scenarios'
Windows answers are columns of [`../kernel`](../kernel)'s trace.

## Regenerate

On Windows, in an opam switch with dune and the eio_windows to record:

    REGEN=1 dune build @test/interop/windows/regen-traces

The workflow [`windows.yml`](../../../.github/workflows/windows.yml) runs it
with the kernel's, and uploads `traces/` in the artifact `windows-traces-1.7`.
`scripts/generate.sh` runs `paths.exe` in a fresh directory of the temporary
directory, with a fresh directory on another fixed drive when there is one
([`scripts/other-drive.sh`](scripts/other-drive.sh)).

## Provenance

[`traces/provenance`](traces/provenance) says where the trace comes from, as
the kernel's says it for Windows, with the other drive (from
[`scripts/probe.sh`](scripts/probe.sh)), and the MD5 of the trace and of the
code that decides it. [`test/provenance`](../../provenance) checks it.
