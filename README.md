# eio-fs

An in-memory filesystem for [Eio](https://github.com/ocaml-multicore/eio),
to test code under filesystem errors and power cuts.

Each test gets a fresh filesystem of its own, used through the ordinary
`Eio.Path` API, so the code under test does not change.
`Eio_fs_chaos.run` is chaos engineering on one machine: it runs a program
again and again, each time with a different filesystem error or power cut,
and checks that it starts again on what the fault left
([How it works](#how-it-works)). `Eio_fs_chaos.device` checks that an Eio
filesystem answers as Linux or macOS does.

## Installation

<!-- $MDX skip -->
```sh
opam install eio-fs
```

## An in-memory filesystem

`Eio_fs_mem.v ()` is the root of a new, empty filesystem. Two calls share no
file.

```ocaml
# Eio_main.run @@ fun _env ->
  let fs = Eio_fs_mem.v () in
  Eio.Path.save ~create:(`Exclusive 0o644) Eio.Path.(fs / "hello") "world";
  Eio.Path.load Eio.Path.(fs / "hello");;
- : string = "world"
```

## Testing a program

`Eio_fs_chaos.run` checks how a program that reads and writes files through
`Eio.Path` copes when the disk fails. It takes the program as a function of
its environment and runs it many times in memory:

```ocaml
# Eio_fs_chaos.run @@ fun env ->
  Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(env#fs / "hello") "world";;
- : unit = ()
```

The `env` has the capabilities of `Eio.Stdenv`, each simulated
([How it works](#how-it-works)), so a `main` written against `Eio.Stdenv`
runs unchanged. `Eio_fs_chaos.disk ()` is the same disk as a part of
[eio-chaos](https://github.com/samoht/ocaml-eio-chaos), to test a program on
it and other simulated devices at once. This program saves a file, then
serves it to a client, with a timeout so that a silent peer does not hang
it. Its runs fail the disk, the network of
[eio-net](https://github.com/samoht/ocaml-eio-net), or both:

```ocaml
# let serve (env : Eio_fs_chaos.env) =
    let file = Eio.Path.(env#fs / "log") in
    Eio.Path.save ~create:(`Or_truncate 0o644) file "up";
    Eio.Switch.run @@ fun sw ->
    let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8080) in
    let l = Eio.Net.listen ~reuse_addr:true ~backlog:1 ~sw env#net addr in
    Eio.Fiber.fork_daemon ~sw (fun () ->
        Eio.Net.run_server l ~on_error:ignore (fun flow _ ->
            Eio.Flow.copy_string (Eio.Path.load file) flow));
    let get () = Eio.Flow.read_all (Eio.Net.connect ~sw env#net addr) in
    match Eio.Time.with_timeout env#clock 5.0 (fun () -> Ok (get ())) with
    | Ok _ | Error `Timeout -> ()
    | exception (Eio.Io _ | End_of_file) -> ();;
val serve : Eio_fs_chaos.env -> unit = <fun>
# Eio_chaos.run [ Eio_fs_chaos.disk (); Eio_net_chaos.network () ] serve;;
- : unit = ()
```

The first run has no fault. Each later run starts from the same state and
meets one fault:

- an open, a read, a write or a sync fails;
- a write is torn, or the disk becomes full or read-only;
- the power is cut, and the disk keeps only part of what was written.

It tries each fault at each operation of the first run, except that of the
many reads and writes of one file it takes the first three, the last and one
of each size. Then it tries random pairs of faults drawn from `~seed`. After
each of those runs, the check starts the program again with no fault, on
what the filesystem then holds, as a machine restarts a service. After a
power cut, that is what the disk kept.

The check fails if a run hangs, if it raises anything other than `Eio.Io`,
or if the restart does not end normally. An `Eio.Io` error that escapes a
run with a fault is not a failure in itself: the disk did fail, so the
restart decides. A program that turns every `Eio.Io` into an exception of
its own fails the check at the first fault.

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

The save above does not sync its directory, so a power cut loses the
rename. `Eio_fs_sync.save` writes a file atomically and syncs its directory
too, and `Eio.Path.sync_dir` syncs a directory by hand. A report gives the
fault, the operations of the run, what the program printed, if anything, and
one line to paste that runs that case again; to find it, a replay may run the
program a few times first, and the failing run is the last.

Faults fall only on the operations the first run makes, and that run starts
on an empty filesystem. Code that runs only at a restart, such as the read
of a configuration the first run wrote, meets faults only when `~setup`
makes the first run reach it. This start-up reads its configuration with a
single read, which may return fewer bytes:

```ocaml
# let start env =
    let config = Eio.Path.(env#fs / "config") in
    match
      Eio.Path.with_open_in config (fun f ->
          let buf = Cstruct.create 64 in
          Cstruct.to_string ~len:(Eio.Flow.single_read f buf) buf)
    with
    | "debug" -> ()
    | s -> failwith ("bad config: " ^ s)
    | exception Eio.Io (Eio.Fs.E (Not_found _), _) -> save env#fs "debug";;
val start : < fs : [> Eio.Fs.dir_ty ] Eio.Path.t; .. > -> unit = <fun>
# Eio_fs_chaos.run start;;
- : unit = ()
# match
    Eio_fs_chaos.run ~setup:(fun env -> save env#fs "debug") start
  with
  | () -> "passed"
  | exception Failure report -> List.hd (String.split_on_char '\n' report);;
- : string = "Eio_fs_chaos.run: the program failed under a fault"
```

`~inspect` checks what only the test knows, such as the records a log must
still hold, after each fault and before the restart.
[`eio_fs_chaos.mli`](lib/chaos/eio_fs_chaos.mli) says how the faults are
picked and what a program may and may not do.

## Testing a filesystem

`Eio_fs_chaos.device` checks that an Eio filesystem of your own, such as a
wrapper over another one, answers as Linux or macOS does. It takes a function
that makes a fresh, empty directory from the same simulated `env` and runs
short scenarios in it, each recorded on a real Linux and a real macOS kernel.

```ocaml
# Eio_fs_chaos.device (fun ~sw:_ _env -> Eio_fs_mem.v ());;
- : unit = ()
```

A filesystem built on other capabilities, such as a wrapper over a
directory, takes them from `env`. The scenarios cover the corners where
filesystems disagree:

- names that differ only by case or by Unicode form;
- names and paths at their length limits;
- opens, renames and removals of files, directories and links, with and
  without a trailing slash;
- chains and loops of symbolic links;
- the permission rules that an ordinary user meets.

The check runs every scenario, then reports each one whose answer differs,
as `case lookup: write a x; stat A: expected file 1, got ENOENT`, with a
summary; [`eio_fs_chaos.mli`](lib/chaos/eio_fs_chaos.mli) lists the
operations. Pass ``~os:`Macos`` to compare with macOS on its default APFS
volume, where `a` and `A` are the same name.

The permission scenarios run only with `~user`, as root passes every
permission check. `~user` says which user the directory's operations run as;
it does not change that user. The in-memory filesystem needs
`Eio_fs_mem.v ~user` to match.

A filesystem built over other devices, such as a remote filesystem over a
network, is checked over faulty ones with `~under`:
`Eio_fs_chaos.device ~under:[ Eio_net_chaos.network () ] remote` runs the
scenarios on a healthy network first, then on one that fails, where an
answer may also be an error a disk could give, such as an input/output
error, but never one of the network's, such as `Connection_reset`.

eio_posix itself differs from the kernel on 12 of the 86 scenarios on Linux
and 20 on macOS, as it resolves a trailing slash, `.` and `..` itself. A
filesystem that wraps eio_posix differs there too, and the report marks
those lines with `(eio_posix differs here too)`.

## How it works

### Chaos engineering, on one machine

`Eio_fs_chaos.run` is the check of
[eio-chaos](https://github.com/samoht/ocaml-eio-chaos#chaos-engineering-on-one-machine)
on the disk alone, and its README says how the faults are picked and what a
pass means. On the disk:

- **steady state:** the program returns, and when it is restarted on what a
  fault left, it starts and works again, with every file it acknowledged
  intact;
- **faults:** each fault the real kernel can produce at each call the
  program made, and a power cut;
- **result:** either the hypothesis held for every fault tried, or the first
  counter-example and one line that replays it.

### What is simulated

The program receives each device as an Eio capability, through `env`. The
package replaces each capability with a simulated one; the program is not
changed.

- The filesystem is a page cache over a simulated disk. Writes stay in the
  cache until `fsync` or a directory sync makes them durable. A power cut
  keeps only what ext4 (`data=writeback`) may keep; under ``~os:`Macos``
  only the sync rules of APFS differ (see [Limitations](#limitations)).
- The clocks jump to the next timer, `secure_random` and `Random` are
  seeded, the standard streams are in memory, a domain manager runs its work
  on the run's own scheduler, and no process starts.

A program built with [hermetic](https://github.com/samoht/ocaml-hermetic)
(`open! Hermetic`) cannot escape the check: the compiler rejects direct
`Unix`, standard-library I/O, entropy, domains and processes, so each run
replays exactly, unless the program keeps state in memory from one run to the
next. Without hermetic, a direct call escapes the check, and when it makes
two runs differ, the report names the first difference and its likely
cause. The same holds for a filesystem under `device`.

### What is guaranteed

1. With no fault, the simulated filesystem answers each call as Linux or
   macOS does through Eio, as recorded on real kernels; the Limitations list
   the known differences.
2. Only faults the real kernel can return at that call are injected, and a
   power cut leaves only the states the crash rules allow. So every fault
   reported can happen on a real machine.
3. Completeness is not guaranteed. Faults fall only on the calls the program
   made, at most two per run, and a power cut leaves only the crash states of
   ext4 and APFS. A pass means no counter-example was found there.

`device` checks guarantee 1 for a filesystem of your own: each call's answer
against the recorded kernel answers. It checks nothing about durability
across power cuts, or about on-disk layout.

## Libraries

- `eio-fs` has `Eio_fs`: `Crunch` loads into an in-memory filesystem a
  directory tree its `eio-fs-crunch` binary embeds in an executable, `Tree`
  walks, measures, copies and formats a tree on any Eio backend, and
  `Nofollow` reads a file, bounded, refusing a symbolic link at the final
  path component, and `Path` works on path strings, without touching a
  filesystem.
- `eio-fs.mem` has `Eio_fs_mem`, the in-memory filesystem.
  [`eio_fs_mem.mli`](lib/mem/eio_fs_mem.mli) says how closely it follows
  each kernel.
- `eio-fs.chaos` has `Eio_fs_chaos`, the two checks above and the faults
  they are made of, so a program that uses only the filesystem links none of
  it.
- `eio-fs.sync` writes a file atomically on any Eio filesystem, and
  durably with the barrier it is given: the default is enough on Linux,
  while on macOS only `Eio_fs_unix.barrier` goes through the drive's cache.
- `eio-fs.unix` binds POSIX calls that Eio and `Unix` lack, such as file
  locks and extended attributes.

Each library documents itself in its `.mli`, under [`lib/`](lib/).

## Limitations

- A file's bytes live in memory, and a file holds at most 4 GiB
  ([`Eio_fs_mem`](lib/mem/eio_fs_mem.mli)).
- The filesystem follows one kernel at a time, Linux, or macOS with
  ``~os:`Macos``: code that runs on both needs a check under each
  ([`Eio_fs_mem`](lib/mem/eio_fs_mem.mli)).
- eio_posix differs from the kernel on a few paths, such as one that ends in
  `..`, and the in-memory filesystem follows the kernel
  ([`Eio_fs_mem`](lib/mem/eio_fs_mem.mli)).
- A power cut leaves what ext4 with `data=writeback` may leave, a weaker
  model than ext4's default, `data=ordered`, which writes a file's data
  before the metadata that names it. Under ``~os:`Macos`` only the syncs
  differ: `fsync` stops in the drive's cache and `F_FULLFSYNC` goes through
  it. The rest follows ext4, so a cut may leave a state APFS would not
  ([`Eio_fs_chaos.Power`](lib/chaos/eio_fs_chaos.mli)).

## Related work

`device` checks a filesystem against recordings of real kernels, as
[SibylFS](https://sibylfs.github.io/) checked real filesystems against a
model of POSIX. The litmus tests of the power cut port the crash models of
[Ferrite](https://github.com/uwplse/ferrite).

## Licence

ISC, see [LICENSE.md](LICENSE.md). The in-memory filesystem started as Patrick
Ferris's [eio_mem](https://git.sirref.org/eio_mem), and `Eio_fs.Path` adapts
its modification of [fpath](https://github.com/dbuenzli/fpath).
