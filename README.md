# eio-fs

An in-memory filesystem for [Eio](https://github.com/ocaml-multicore/eio),
to test code under filesystem errors and power cuts.

Each test gets a fresh filesystem of its own, used through the ordinary
`Eio.Path` API, so the code under test does not change. The checks of
`Eio_fs.Chaos` run that code again and again, each time with a different
filesystem error or power cut.

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

## Testing an app on a broken filesystem

<!-- TODO: one call that runs an unmodified app under filesystem errors, and
     under power cuts when given a `recover`. -->

## Testing a filesystem implementation

<!-- TODO: one call that checks an Eio filesystem against Linux and macOS. -->

## Libraries

- `eio-fs` has the in-memory filesystem and the checks. Its `eio-fs-crunch`
  binary embeds a directory tree in an executable.
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
- A power cut leaves what ext4 or APFS may leave, and no other filesystem is
  modelled ([`Eio_fs.Chaos.Crash`](lib/chaos.mli)).
- A check injects at most two faults in a run, so it misses a bug that needs
  three ([`Eio_fs.Chaos`](lib/chaos.mli)).

## Licence

ISC, see [LICENSE.md](LICENSE.md). The in-memory filesystem started as Patrick
Ferris's [eio_mem](https://git.sirref.org/eio_mem), and `eio-fs.path` adapts
its modification of [fpath](https://github.com/dbuenzli/fpath). The scheduler
that runs a check is a copy of
[Eio](https://github.com/ocaml-multicore/eio)'s mock backend, and the tests
port the crash models of [Ferrite](https://github.com/uwplse/ferrite).
