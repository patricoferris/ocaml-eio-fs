(** Filesystem helpers for {!Eio}.

    [Eio_fs] embeds a directory tree into an OCaml module at compile time
    ({!Crunch}), walks over a tree ({!Tree}) and reads a file without following
    a symbolic link ({!Nofollow}). {!Nofollow} and {!Tree} work on any {!Eio}
    backend, the in-memory filesystem of [eio-fs.mem] included; each call to
    {!Crunch.load} makes such a filesystem of its own. *)

module Crunch = Crunch
(** Compile-time file embedding for the in-memory filesystem. *)

module Tree = Tree
(** Trees and directories on any {!Eio} backend. *)

module Nofollow = Nofollow
(** Bounded reads that refuse a symbolic link at the final path component. *)
