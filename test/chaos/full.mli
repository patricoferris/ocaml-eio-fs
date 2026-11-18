(** F_FULLFSYNC on a filesystem that registers its barriers, as
    [Eio_fs_unix.sync_file] and [Eio_fs_unix.sync_dir] issue it there. *)

val file : _ Eio.Resource.t -> unit
(** [file f] makes [f]'s data durable through the drive's cache.

    @raise Invalid_argument if [f] registers no barrier. *)

val dir : _ Eio.Path.t -> unit
(** [dir d] makes [d]'s entries durable through the drive's cache.

    @raise Invalid_argument if [d] registers no barrier. *)
