# eio-fs

An in-memory filesystem for [Eio](https://github.com/ocaml-multicore/eio),
to test code under filesystem errors and power cuts.

The filesystem is used through the ordinary `Eio.Path` API, so the code
under test does not change. On top of it, `Eio_fs_chaos.run` runs a program
again and again, each time with a different disk error or power cut, and
checks that the program starts again on what the fault left.
`Eio_fs_chaos.device` checks that a filesystem of your own answers as Linux,
macOS or Windows does.

## Installation

<!-- $MDX skip -->
```sh
opam install eio-fs
```

## An in-memory filesystem

`Eio_fs_mem.v ()` is the root of a new, empty filesystem:

```ocaml
# Eio_main.run @@ fun _env ->
  let fs = Eio_fs_mem.v () in
  Eio.Path.save ~create:(`Exclusive 0o644) Eio.Path.(fs / "hello") "world";
  Eio.Path.load Eio.Path.(fs / "hello");;
- : string = "world"
```

It answers as Linux does by default, and as macOS or Windows with `~os`, as
recorded on real kernels. On macOS a name that differs only by case is the
same file, and on Windows a file held open cannot be removed.

## Testing a program

`Eio_fs_chaos.run` takes the program as a function of its environment, and
runs it many times on a fresh in-memory disk:

```ocaml
# Eio_fs_chaos.run @@ fun env ->
  Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(env#fs / "hello") "world";;
- : unit = ()
```

The first run has no fault. Each later run meets one or two, such as a write
that fails or a disk that fills up. Some runs cut the power, and the disk
keeps only part of what was written. After each, the program restarts with no
fault, on what the disk kept. The check fails if a run hangs, if the program
raises something other than an `Eio.Io` error, or if the restart fails.

A restart cannot see lost data, because the program writes it again. So the
program calls `Eio_fs_chaos.ack env file` once it has promised that a file is
safe, and the check makes sure no fault loses it. This save writes a
temporary file, syncs it and renames it over the old one:

```ocaml
# let save fs v =
    let tmp = Eio.Path.(fs / "config.tmp") in
    Eio.Path.with_open_out ~create:(`Or_truncate 0o644) tmp (fun f ->
        Eio.Flow.copy_string v f;
        Eio.File.sync f);
    Eio.Path.rename tmp Eio.Path.(fs / "config");;
val save : [> Eio.Fs.dir_ty ] Eio.Path.t -> string -> unit = <fun>
# match
    Eio_fs_chaos.run (fun env ->
        save env#fs "new";
        Eio_fs_chaos.ack env Eio.Path.(env#fs / "config"))
  with
  | () -> print_endline "passed"
  | exception Failure report -> print_endline report;;
Eio_fs_chaos.run: a power cut lost an acknowledged file: /config held "new" when acknowledged after #4, and now is missing
  cut:      at the end, every unsynced change lost
  faults:   none
  acked:    1 value, after #4
  lost:     #1 create /config.tmp; #4 rename /config.tmp to /config
  trace:
    #1 open-write /config.tmp
    #2 write /config.tmp@0+3
    #3 sync /config.tmp
    #4 rename /config.tmp to /config
  replay:   Eio_fs_chaos.run ~replay:"fs/7a2e6ba7/none|end|drop" app
- : unit = ()
```

The rename is lost because the save does not sync the directory.
`Eio_fs.Sync.save` does both. The report ends with one line that replays the
case, and the [eio-chaos](https://github.com/samoht/ocaml-eio-chaos) README
says more about how faults are chosen and what a pass means.

`Eio_fs_chaos.disk ()` is the same disk as a part of eio-chaos, to test a
program on several simulated devices at once, such as the disk and the
network of [eio-net](https://github.com/samoht/ocaml-eio-net).
[`eio_fs_chaos.mli`](lib/chaos/eio_fs_chaos.mli) documents the rest,
including how to give the first run a disk that is not empty.

## Testing a filesystem

`Eio_fs_chaos.device` checks that an Eio filesystem of your own, such as a
wrapper over another one, answers as the kernel does. It takes a function
that makes a fresh, empty directory, and runs in it about a hundred short
scenarios recorded on real Linux, macOS and Windows machines:

```ocaml
# Eio_fs_chaos.device (fun ~sw:_ _env -> Eio_fs_mem.v ());;
- : unit = ()
```

The scenarios cover the corners where filesystems disagree, such as case and
Unicode forms in names, long paths, trailing slashes, symbolic links and
permissions, and the traps of Windows. The check reports each scenario whose
answer differs from the kernel's. A filesystem built over other devices, such
as one over a network, can also be checked while those devices fail.

## Libraries

- `eio-fs` has `Eio_fs`, helpers for any Eio filesystem: atomic and durable
  writes, tree walks and copies, path strings, and a loader for directory
  trees embedded in an executable.
- `eio-fs.mem` has `Eio_fs_mem`, the in-memory filesystem.
- `eio-fs.chaos` has `Eio_fs_chaos`, the two checks above.
- `eio-fs.unix` has `Eio_fs_unix`, the POSIX calls that Eio lacks, such as
  file locks and extended attributes. It is not built on Windows.

Each library documents itself in its `.mli`, under [`lib/`](lib/), including
how closely the simulation follows each kernel and where it stops.

## Related work

`device` checks a filesystem against recordings of real kernels, as
[SibylFS](https://sibylfs.github.io/) checked real filesystems against a
model of POSIX. The power cut follows the crash models of
[Ferrite](https://github.com/uwplse/ferrite).

## Licence

ISC, see [LICENSE.md](LICENSE.md). The in-memory filesystem started as Patrick
Ferris's [eio_mem](https://git.sirref.org/eio_mem), and `Eio_fs.Path` adapts
its modification of [fpath](https://github.com/dbuenzli/fpath).
