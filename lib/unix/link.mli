(** Symbolic links published beneath a parent directory held open, which
    {!Eio_fs_unix} documents as {!Eio_fs_unix.Link}. *)

type t
(** See {!Eio_fs_unix.Link.t}. *)

(** See {!Eio_fs_unix.Link.publication}. *)
type publication = Created | Present

val with_parent : ?create:Lock.create -> _ Eio.Path.t -> (t -> 'a) -> 'a
(** See {!Eio_fs_unix.Link.with_parent}. *)

val publish :
  t ->
  target:string ->
  (publication, [ `Changed of string option | `Invalid of string ]) result
(** See {!Eio_fs_unix.Link.publish}. *)

type regular = { contents : string; perm : Eio.File.Unix_perm.t }
(** See {!Eio_fs_unix.Link.regular}. *)

val read_regular :
  max_bytes:int -> t -> (regular option, Eio_fs.Nofollow.error) result
(** See {!Eio_fs_unix.Link.read_regular}. *)
