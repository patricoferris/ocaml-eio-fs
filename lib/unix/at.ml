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

(* [openat(O_RDONLY | O_DIRECTORY | O_NOFOLLOW)]. *)
external open_dir : Unix.file_descr -> string -> Unix.file_descr
  = "caml_eio_fs_batch_open_dir"

(* [openat(O_RDONLY | O_NOFOLLOW | O_NONBLOCK)]. *)
external open_leaf : Unix.file_descr -> string -> Unix.file_descr
  = "caml_eio_fs_open_leaf"

external mkdir : Unix.file_descr -> string -> int -> unit
  = "caml_eio_fs_batch_mkdir"

external symlink : string -> Unix.file_descr -> string -> unit
  = "caml_eio_fs_batch_symlink"

external read_link : Unix.file_descr -> string -> string
  = "caml_eio_fs_readlinkat"

(* [fstatat(AT_SYMLINK_NOFOLLOW)]. *)
external lstat : Unix.file_descr -> string -> raw_stat
  = "caml_eio_fs_batch_lstat"

(* [fstat], its times to the nanosecond as Eio's own stat has them. *)
external fstat : Unix.file_descr -> raw_stat = "caml_eio_fs_fstat"

let kind_of_mode mode =
  match mode land 0o170000 with
  | 0o040000 -> `Directory
  | 0o100000 -> `Regular_file
  | 0o120000 -> `Symbolic_link
  | 0o010000 -> `Fifo
  | 0o020000 -> `Character_special
  | 0o060000 -> `Block_device
  | 0o140000 -> `Socket
  | _ -> `Unknown

let stat_of_raw (r : raw_stat) : Eio.File.Stat.t =
  let mode = Int64.to_int r.mode in
  {
    dev = r.dev;
    ino = r.ino;
    kind = kind_of_mode mode;
    perm = mode land 0o7777;
    nlink = r.nlink;
    uid = r.uid;
    gid = r.gid;
    rdev = r.rdev;
    size = Optint.Int63.of_int64 r.size;
    blksize = r.blksize;
    blocks = r.blocks;
    atime = r.atime;
    mtime = r.mtime;
    ctime = r.ctime;
  }
