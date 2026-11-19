## 0.1.0 (unreleased)

Initial release.

- `Eio_fs_mem.v` is a filesystem in memory, used through `Eio.Path`. It
  follows Linux, macOS or, for the common traps, Windows (`?os`): case and
  Unicode names, links, drive letters, and files that cannot be renamed
  while open, from answers recorded on each kernel.
- `Eio_fs_chaos.run` runs a program under filesystem errors and power cuts,
  keeping what each kernel keeps of each kind of sync, and checks that it
  starts again on what the fault left. `Eio_fs_chaos.device` checks that an
  Eio filesystem answers as the recorded kernels do.
- `Eio_fs.Sync` saves and publishes files atomically, durable with the
  barrier it is given (`Eio_fs_unix.barrier` is durable on macOS too).
  `Eio_fs.Crunch` and the `eio-fs-crunch` tool embed a directory in a
  program.
- `eio-fs.unix` binds the POSIX calls Eio and `Unix` lack (record locks,
  `statvfs`, copy-on-write clones, extents) and builds on POSIX hosts only;
  the rest of the package builds on Windows too.
