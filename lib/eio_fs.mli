(** Filesystem helpers for {!Eio}.

    [Eio_fs] embeds a directory tree into an OCaml module at compile time
    ({!Crunch}), walks over a tree ({!Tree}), and reads a file or opens a path's
    parents without following a symbolic link ({!Nofollow}). {!Nofollow} and
    {!Tree} work on any {!Eio} backend, the in-memory filesystem of [eio-fs.mem]
    included; each call to {!Crunch.load} makes such a filesystem of its own.
    {!Path} works on path strings, without touching a filesystem, and {!Sync}
    writes a file atomically on any Eio filesystem, durably with the barrier it
    is given. *)

module Crunch = Crunch
(** Compile-time file embedding for the in-memory filesystem. *)

module Tree = Tree
(** Trees and directories on any {!Eio} backend. *)

module Nofollow = Nofollow
(** Reads and walks that follow no symbolic link. *)

module Path = Eio_fs_path
(** Lexical arithmetic on Eio path strings. *)

module Sync = Eio_fs_sync
(** Barriers and the atomic publications built on them. *)
