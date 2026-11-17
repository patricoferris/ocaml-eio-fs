(** Filesystem calls made on the calling thread. *)

val fs : Eio.Fs.dir_ty Eio.Path.t -> Eio.Fs.dir_ty Eio.Path.t
(** See {!Eio_fs_unix.inline_fs}. *)
