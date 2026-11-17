# eio-fs

An in-memory filesystem for [Eio](https://github.com/ocaml-multicore/eio),
to test code under filesystem errors and power cuts.

Each test gets a fresh filesystem of its own, used through the ordinary
`Eio.Path` API, so the code under test does not change.
`Eio_fs_chaos.run` runs a program again and again, each time with a
different filesystem error or power cut, and restarts it on what the failure
left. `Eio_fs_chaos.device` checks that an Eio filesystem answers as Linux or
macOS does.

## Installation

<!-- $MDX skip -->
```sh
opam install eio-fs
```

## An in-memory filesystem

`Eio_fs.Fs.v ()` is the root of a new, empty filesystem. Two calls share no
file.

```ocaml
# Eio_main.run @@ fun _env ->
  let fs = Eio_fs.Fs.v () in
  Eio.Path.save ~create:(`Exclusive 0o644) Eio.Path.(fs / "hello") "world";
  Eio.Path.load Eio.Path.(fs / "hello");;
- : string = "world"
```

## Testing a program

Your program reads and writes files through `Eio.Path`. What does it do when
the disk fails? `Eio_fs_chaos.run` takes the program as a function of its
environment and runs it many times in memory, unchanged.

```ocaml
# Eio_fs_chaos.run @@ fun env ->
  Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(env#fs / "hello") "world";;
- : unit = ()
```

The first run has no fault. Each later run starts from the same state and
meets one fault:

- an open, a read, a write or a sync fails;
- a write is torn, or the disk becomes full or read-only;
- the power is cut, and the disk keeps only part of what was written.

It tries every fault at every operation of the first run, then random pairs
of faults drawn from `~seed`. After each of those runs, the check starts the
program again with no fault, on what the filesystem then holds, as a machine
restarts a service. After a power cut, that is what the disk kept.

The check fails if a run hangs, if it raises anything other than `Eio.Io`,
or if the restart does not end normally. An `Eio.Io` error that escapes a
run with a fault is not a failure in itself: the disk did fail, so the
restart decides.

A restart cannot see lost data, because the program writes it again. For
that, the program calls `Eio_fs_chaos.ack env file` once it has promised
that a file is safe. After each fault, the check compares every such file
with what it held then.

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
  | () -> "passed"
  | exception Failure report -> List.hd (String.split_on_char '\n' report);;
- : string =
"Eio_fs_chaos.run: a power cut lost an acknowledged file: /config held \"new\" when acknowledged after #4, and now is missing"
```

The save above does not sync its directory, so a power cut loses the
rename. A failure report gives the fault, the operations of the run and one
line to paste, which runs that case alone.
[`eio_fs_chaos.mli`](lib/chaos/eio_fs_chaos.mli) says how the faults are
picked and what a program may and may not do.

## Testing a filesystem

You have written an Eio filesystem: a backend, a wrapper, a cache. Does it
answer as Linux does? `Eio_fs_chaos.device` takes a function that makes a
fresh, empty directory and runs short scenarios in it, each recorded on a
real Linux and a real macOS kernel.

```ocaml
# Eio_fs_chaos.device (fun ~sw:_ _env -> Eio_fs.Fs.v ());;
- : unit = ()
```

The scenarios cover the corners where filesystems disagree:

- names that differ only by case or by Unicode form;
- names and paths at their length limits;
- opens, renames and removals of files, directories and links, with and
  without a trailing slash;
- chains and loops of symbolic links;
- the permission rules that an ordinary user meets.

The check runs every scenario, then reports each one whose answer differs,
as `case lookup: write a x; stat A: expected file 1, got ENOENT`,
with a summary. Pass ``~os:`Macos`` to compare with macOS on its default
APFS volume, where `a` and `A` are the same name. The permission scenarios run
only with `~user`, the user the directory's operations run as, as root
passes every permission check.

## Libraries

- `eio-fs` has the in-memory filesystem. Its `eio-fs-crunch` binary embeds a
  directory tree in an executable.
- `eio-fs.chaos` has `Eio_fs_chaos`, the two checks above and the faults
  they are made of, so a program that uses only the filesystem links none of
  it.
- `eio-fs.sync` writes a file atomically and durably on any Eio filesystem.
- `eio-fs.unix` binds POSIX calls that Eio and `Unix` lack, such as file
  locks and extended attributes.
- `eio-fs.path` works on path strings, without touching a filesystem.

## Limitations

- A file's bytes live in memory, and a file holds at most 4 GiB
  ([`Eio_fs.Fs`](lib/eio_fs.mli)).
- The filesystem answers as Linux does, or as macOS does with ``~os:`Macos``.
  Code that runs on both needs a test under each.
- eio_posix differs from the kernel on a few paths, such as one that ends in
  `..`, and the in-memory filesystem follows the kernel
  ([`Eio_fs.Fs`](lib/eio_fs.mli)).
- A power cut leaves what ext4 may leave, and APFS follows the same rules
  but for its syncs ([`Eio_fs_chaos.Power`](lib/chaos/eio_fs_chaos.mli)).
- `Eio_fs_chaos.run` injects at most two faults in a run, so it misses a bug
  that needs three, and a restart finds lost data only in a file the program
  acknowledged ([`Eio_fs_chaos`](lib/chaos/eio_fs_chaos.mli)).

## Licence

ISC, see [LICENSE.md](LICENSE.md). The in-memory filesystem started as Patrick
Ferris's [eio_mem](https://git.sirref.org/eio_mem), and `eio-fs.path` adapts
its modification of [fpath](https://github.com/dbuenzli/fpath). The scheduler
that runs a check is a copy of
[Eio](https://github.com/ocaml-multicore/eio)'s mock backend, and the tests
port the crash models of [Ferrite](https://github.com/uwplse/ferrite).
