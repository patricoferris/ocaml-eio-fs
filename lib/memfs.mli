(** The in-memory filesystem's directories, behind [Eio_fs.Fs]. *)

val fs :
  ?user:Low_level.User.t ->
  Low_level.t ->
  [ `Close | Eio.Fs.dir_ty ] Eio.Resource.t
(** [fs ~user t] is full access to the filesystem [t], as [user] (defaults to
    root). *)

val cwd :
  ?user:Low_level.User.t ->
  Low_level.t ->
  [ `Close | Eio.Fs.dir_ty ] Eio.Resource.t
(** [cwd ~user t] is access to [t] confined to its current directory. *)

val v :
  ?clock:_ Eio.Time.clock ->
  ?user:Low_level.User.t ->
  ?os:[< `Linux | `Macos ] ->
  unit ->
  [ `Close | Eio.Fs.dir_ty ] Eio.Path.t
(** [v ~clock ~user ~os ()] is the root of a fresh, empty filesystem whose times
    [clock] gives, that answers as [os] does. *)

val as_user :
  Low_level.User.t -> 'a Eio.Path.t -> [ `Close | Eio.Fs.dir_ty ] Eio.Path.t
(** [as_user user path] is [path], an in-memory directory, as [user]. A wrapper
    that binds [Eio_fs_internal.Rebase] is rebuilt over its base as [user].

    @raise Invalid_argument
      if [path] is neither an in-memory directory nor such a wrapper. *)

val held : _ Eio.Path.t -> Extents.t option
(** [held path] is the byte ranges writes covered in the regular file [path] of
    an in-memory filesystem, looked up as root: the rest of it is a hole. It is
    [None] if [path] is not in one, or names no regular file. *)

val descriptors : _ Eio.Path.t -> int option
(** [descriptors path] is how many descriptors are open on the in-memory
    filesystem of [path], or [None] if [path] is not in one. *)

val set_times :
  _ Eio.Path.t -> atime:float -> mtime:float -> ctime:float -> unit
(** [set_times path ~atime ~mtime ~ctime] sets the times of [path], not
    followed, as root, if it is in an in-memory filesystem, and does nothing
    otherwise. *)

val load :
  ?clock:_ Eio.Time.clock ->
  ?filter:('a Eio.Path.t -> bool) ->
  ?os:[< `Linux | `Macos ] ->
  'a Eio.Path.t ->
  [ `Close | Eio.Fs.dir_ty ] Eio.Path.t
(** [load ~clock ~filter ~os path] is a new filesystem holding a copy of the
    tree at [path], that answers as [os] does. *)
