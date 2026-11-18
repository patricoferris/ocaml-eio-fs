(** System calls on descriptors and beneath directory descriptors, each the one
    call its name says, with the runtime released around it. They block, so a
    fiber makes them from a system thread; they perform no effect, so any Eio
    backend runs them. {!Eio_fs_unix.read_regular} makes one per system-thread
    hop, {!Eio_fs_unix.Batch} many. *)

type raw_stat = {
  dev : int64;
  ino : int64;
  mode : int64;
  nlink : int64;
  uid : int64;
  gid : int64;
  rdev : int64;
  size : int64;
  blksize : int64;
  blocks : int64;
  atime : float;
  mtime : float;
  ctime : float;
}
(** The type for a stat as the system gives it, before {!stat_of_raw}. *)

external open_dir : Unix.file_descr -> string -> Unix.file_descr
  = "caml_eio_fs_batch_open_dir"
(** [open_dir dir name] is [openat(O_RDONLY | O_DIRECTORY | O_NOFOLLOW)]. *)

external open_leaf : Unix.file_descr -> string -> Unix.file_descr
  = "caml_eio_fs_open_leaf"
(** [open_leaf dir name] is [openat(O_RDONLY | O_NOFOLLOW | O_NONBLOCK)]. *)

external mkdir : Unix.file_descr -> string -> int -> unit
  = "caml_eio_fs_batch_mkdir"
(** [mkdir dir name perm] is [mkdirat]. *)

external symlink : string -> Unix.file_descr -> string -> unit
  = "caml_eio_fs_batch_symlink"
(** [symlink target dir name] is [symlinkat]. *)

external read_link : Unix.file_descr -> string -> string
  = "caml_eio_fs_readlinkat"
(** [read_link dir name] is [readlinkat]. *)

external lstat : Unix.file_descr -> string -> raw_stat
  = "caml_eio_fs_batch_lstat"
(** [lstat dir name] is [fstatat(AT_SYMLINK_NOFOLLOW)]. *)

external fstat : Unix.file_descr -> raw_stat = "caml_eio_fs_fstat"
(** [fstat fd] is [fstat], its times to the nanosecond as Eio's own stat has
    them. *)

val kind_of_mode : int -> Eio.File.Stat.kind
(** [kind_of_mode mode] is the kind of file [mode]'s type bits name. *)

val stat_of_raw : raw_stat -> Eio.File.Stat.t
(** [stat_of_raw s] is [s] as an {!Eio.File.Stat.t}. *)
