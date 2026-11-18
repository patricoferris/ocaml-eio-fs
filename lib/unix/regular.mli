(** A regular file's bytes and mode, read without waiting on a FIFO, which
    {!Eio_fs_unix} documents as {!Eio_fs_unix.read_regular}. *)

type t = { contents : string; perm : Eio.File.Unix_perm.t }
(** See {!Eio_fs_unix.regular}. *)

val read :
  max_bytes:int -> _ Eio.Path.t -> (t option, Eio_fs.Nofollow.error) result
(** See {!Eio_fs_unix.read_regular}. *)
