# eio-fs

An in-memory filesystem for [Eio](https://github.com/ocaml-multicore/eio),
with fault injection for crash-consistency tests.

Code written against `Eio.Path` runs the same on a disk or in memory. eio-fs
provides the in-memory side, for tests and for unikernels that have no disk.
Each filesystem it creates is separate from the others. Its chaos tests run a
workload under filesystem errors and power cuts, to check how a program copes
with them.

## Installation

<!-- $MDX skip -->
```sh
opam install eio-fs
```

## Usage

`Eio_fs.Fs.v ()` returns the root of a new, empty filesystem in memory. The
code that uses it sees an ordinary `Eio.Path`. The examples below link `eio-fs`
and `eio_main`.

```ocaml
Eio_main.run @@ fun _env ->
let fs = Eio_fs.Fs.v () in
Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(fs / "hello") "world";
assert (Eio.Path.load Eio.Path.(fs / "hello") = "world")
```

`Eio_fs.Fs.load dir` returns a new in-memory filesystem holding a copy of the
tree at `dir`. `Eio_fs.Tree.pp_tree` prints a tree on any backend.

## Chaos tests

### Filesystem errors

`Eio_fs.Chaos.check` runs a workload many times, each time on a fresh
in-memory filesystem and with a different fault. It chooses the faults from
what the workload does:

- an open, a write or a sync fails;
- a write moves one byte, or is torn (only half of its bytes land);
- the filesystem turns read-only or full.

The example below is a cache that must keep working whatever the filesystem
does. An error only costs a recomputation. Each entry ends with `$`, so a
torn entry is not trusted.

```ocaml
# let get dir key ~compute =
    let path = Eio.Path.(dir / key) in
    let soft f = try Some (f ()) with Eio.Io _ -> None in
    match soft (fun () -> Eio.Path.load path) with
    | Some v when String.ends_with ~suffix:"$" v ->
        String.sub v 0 (String.length v - 1)
    | _ ->
        let v = compute key in
        let tmp = Eio.Path.(dir / (key ^ ".tmp")) in
        ignore
          (soft (fun () ->
               Eio.Path.save ~create:(`Or_truncate 0o644) tmp (v ^ "$");
               Eio.Path.rename tmp path));
        v;;
val get :
  [> Eio.Fs.dir_ty ] Eio.Path.t ->
  string -> compute:(string -> string) -> string = <fun>
# Eio_fs.Chaos.check (fun env ->
    for _ = 1 to 2 do
      let v = get env#fs "k" ~compute:String.uppercase_ascii in
      if v <> "K" then Fmt.failwith "got %S" v
    done);;
- : unit = ()
```

### Power cuts

`Eio_fs.Chaos.Crash.check` cuts the power during a workload, and during its
recovery. The workload calls `ack v` once its program has promised that `v` is
durable. After each cut, `recover ~acked` runs on the directory as the disk
holds it, and checks what survived.

The example below saves a configuration file in four steps: it writes a
temporary file, syncs it, renames it, then syncs the directory.

```ocaml
# let save ~sync_dir dir v =
    let tmp = Eio.Path.(dir / "config.tmp") in
    Eio.Path.with_open_out ~create:(`Or_truncate 0o644) tmp (fun f ->
        Eio.Flow.copy_string v f;
        Eio.File.sync f);
    Eio.Path.rename tmp Eio.Path.(dir / "config");
    if sync_dir then Eio.Path.sync_dir dir;;
val save : sync_dir:bool -> [> Eio.Fs.dir_ty ] Eio.Path.t -> string -> unit =
  <fun>
```

`Eio_fs_sync.save` does the same four steps, which the example writes out so
that a test can leave the last one out.

```ocaml
# let test ~sync_dir () =
    Eio_fs.Chaos.Crash.check
      ~setup:(fun env -> save ~sync_dir:true env#fs "old")
      (fun ~ack env ->
        match save ~sync_dir env#fs "new" with
        | () -> ack ()
        | exception Eio.Io _ -> ())
      ~recover:(fun ~acked env ->
        match (Eio.Path.load Eio.Path.(env#fs / "config"), acked) with
        | "new", _ | "old", [] -> ()
        | v, _ -> Fmt.failwith "config is %S" v);;
val test : sync_dir:bool -> unit -> unit = <fun>
# test ~sync_dir:true ();;
- : unit = ()
```

Without the last step, the rename may be lost after the program said that the
configuration was saved:

```ocaml
# let cut f =
    match f () with
    | () -> "passed"
    | exception Failure m -> List.nth (String.split_on_char '\n' m) 1;;
val cut : (unit -> unit) -> string = <fun>
# cut (test ~sync_dir:false);;
- : string = "  cut:      at the end, every unsynced change lost"
```

The full message also gives the operations of the failing run, and a
`~replay:"..."` argument that runs that scenario alone.

### Writing a workload

A workload has a few rules to follow:

- It accepts that any operation fails. A run meets at most two faults, or one
  lasting condition such as a read-only filesystem. A program that tries three
  times gets past two faults, but no retry gets past a lasting condition.
- It lets time pass between its retries. A lasting condition reaches every
  operation from where it starts, the retries included, so a program that
  retries at once fails the check as a hang.
- It catches the errors its program may raise, and checks the results itself.
  Any exception that escapes the workload fails the check, an injected
  `Eio.Io` included.
- It does not catch `Eio.Cancel.Cancelled`. A power cut raises it from the
  operation it falls before, and cancels every fiber once they all wait.
- `recover` reads what the disk holds and what the workload acknowledged. It
  stands for a fresh process, so it must not read state that the workload left
  in memory.

A check blocks the event loop it is called from until it ends. Each run has
its own event loop, whose clock moves only when every fiber waits, so a sleep
takes no time.

A check is deterministic. It draws the scenarios it samples from a fixed seed,
so it meets the same faults each time. It reads no environment variable and
prints nothing. A nightly run can pass another `~seed` to meet other faults,
and the replay of a failure needs no seed.

A test can also inject a fault that a check does not try:
`Eio_fs.Chaos.wrap f dir` asks `f` about each operation, and
`Eio_fs.Chaos.Crash.v` builds a power cut by hand. The documentation of
`Eio_fs.Chaos` has the details.

## Testing as a user

Code that passes its tests as root in CI can fail for an ordinary user, for
instance on a configuration directory that it cannot create, or on a file that
another user owns. An in-memory filesystem runs its operations as a user and
applies the POSIX permission checks to them. The default user is root, which
passes every check, so a test has to name another user to find these bugs.
`Eio_fs.Fs.as_user` runs a directory's operations as another user, so a test
can set a tree up as root first.

The test below saves a configuration file under `/etc`, which root owns. It
works as root and fails as a user.

```ocaml
Eio_main.run @@ fun _env ->
let alice = { Eio_fs.uid = 1000L; gid = 1000L; groups = []; umask = 0o022 } in
let save_config user =
  Eio_fs.run ~user ~cwd:"/home/alice" @@ fun env ->
  let dir = Eio.Path.(env#fs / "/etc/app") in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 dir;
  Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(dir / "conf") "x"
in
save_config Eio_fs.root;
match save_config alice with
| () -> assert false
| exception Eio.Io (Eio.Fs.E (Eio.Fs.Permission_denied _), _) -> ()
```

## Embedding a file tree

The `eio-fs-crunch` binary turns a directory tree into an OCaml module, like
[ocaml-crunch](https://github.com/mirage/ocaml-crunch). With `-o assets.ml` it
writes the module and its interface, `assets.mli`. This dune rule regenerates
them when the `static` directory changes:

```
(rule
 (targets assets.ml assets.mli)
 (deps
  (source_tree static))
 (action
  (run %{bin:eio-fs-crunch} -o assets.ml static)))

(library
 (name assets)
 (modules assets)
 (libraries eio-fs eio))
```

`Assets.load ()` rebuilds the tree in a new in-memory filesystem and returns its
root. The program links the `assets` library. The example below uses the
module built from [examples/readme/static](examples/readme/static).

```ocaml
Eio_main.run @@ fun _env ->
let fs = Assets.load () in
assert (
  Eio.Path.load Eio.Path.(fs / "index.html") = "<h1>Hello from the image</h1>\n")
```

## Libraries

- `eio-fs` holds the in-memory filesystem, `Eio_fs.Chaos` and the support code
  for `eio-fs-crunch`.
- `eio-fs.sync` writes a file atomically and durably on any backend.
- `eio-fs.unix` binds POSIX calls that Eio and `Unix` lack, such as advisory
  locks and extended attributes. It also has `Eio_fs_unix.inline_fs`, a root
  whose stats, opens, link reads and directory listings run on the calling
  thread rather than on a system thread.
- `eio-fs.path` does lexical operations on path strings.

## What the filesystem models

Where POSIX systems differ, the in-memory filesystem answers as Linux does.
With ``~os:`Macos``, it answers as macOS does on its default APFS volume: a name
matches whatever its case and Unicode normalisation, and the errors, modes and
groups are those of macOS.
`Eio_fs.Fs` lists the differences, and the tests check them against a trace of
both kernels, recorded through `Unix` and eio_posix.

Under Linux, `fs.protected_symlinks` is on, as in most distributions. In a
sticky directory that others may write, such as `/tmp`, a link is followed only
by its owner or by the owner of the directory.

A power-cut check models one disk for each system:

- **Linux** is ext4 with `data=writeback`. Unsynced writes may be lost, and
  the changes to a directory reach the disk in program order. A sync of a
  directory makes its own changes durable, a rename into or out of it whole,
  and the earlier changes to each directory they touch. It does not cover the
  other changes that ext4 commits with it, so a program that relies on those
  fails the check, as it would on a filesystem that only follows POSIX.
- **macOS** is APFS on a drive with a cache. `Eio.File.sync` and the portable
  barriers of `Eio_fs_sync` issue `fsync`, which stops in that cache and makes
  nothing durable alone. `Eio_fs_unix.sync_file`, `Eio_fs_unix.sync_dir` and
  `Eio_fs_unix.barrier` issue `F_FULLFSYNC`, which does. Where the sources say
  nothing of APFS, the model keeps the rules of ext4. It may then leave a
  state that APFS would not, but it misses none that APFS would leave.

A program that runs on both systems needs a check under each.
[test/README.md](test/README.md) says where each oracle the tests compare
against comes from, and how to regenerate it.

## Limitations

A check can miss a bug that needs two faults, as some pairs of faults lie past
the default budget of 20,000 runs. A nightly run can raise `~budget` or pass
another `~seed`: `Eio_fs.Chaos` has an example that reads them from
`CHAOS_BUDGET` and `CHAOS_SEED`.

A workload has a size limit. A run that makes 100,000 filesystem operations,
or 10 million scheduler turns, fails as a busy loop or as too large for a
check. A check also fails, and says so, when the scenarios it always runs need
more runs than its budget. The fix is to shrink the workload or to raise
`~budget`.

The in-memory filesystem answers as the kernel does, and eio_posix differs
from the kernel in a few corners. Code that runs on both should not depend on
them:

- A path that ends in a `.` or `..` component, which eio_posix rewrites
  lexically ([eio#976](https://github.com/ocaml-multicore/eio/issues/976)).
- A creating open with a trailing slash, which eio_posix fails with a
  different errno from the kernel's.
- Directories on macOS. eio_posix needs read permission on each directory a
  path walks, and follows up to 40 links to stat a path where the kernel
  stops at 32.

## Licence

ISC, see [LICENSE.md](LICENSE.md). The in-memory backend started as Patrick
Ferris's [eio_mem](https://git.sirref.org/eio_mem), and
`eio-fs.path` adapts its POSIX-only modification of
[fpath](https://github.com/dbuenzli/fpath). The scheduler that runs a check is
a copy of [Eio](https://github.com/ocaml-multicore/eio)'s mock backend, and
the tests port the crash models of
[Ferrite](https://github.com/uwplse/ferrite).
