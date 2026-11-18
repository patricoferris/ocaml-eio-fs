(** What a wrapper of this package was built over, for this package's own
    libraries and tests only.

    A wrapper binds its own interfaces and nothing of its base's, so a caller
    holding it cannot step around it. Two of this package's libraries still need
    the base: [Eio_fs_chaos] itself, to hand a rename target that is a wrapper
    to the base backend, and [eio-fs.unix], to reach the host descriptor a lock
    or a native batch is taken on. They reach it through the interfaces below,
    which no library outside the package can name. *)

type (_, _, _) Eio.Resource.pi +=
  | Base_dir :
      ('t, 't -> Eio.Fs.dir_ty Eio.Resource.t, [> `Base_dir ]) Eio.Resource.pi
        (** The directory a directory wrapper wraps. *)
  | Base_file :
      ('t, 't -> Eio.File.ro_ty Eio.Resource.t, [> `Base_file ]) Eio.Resource.pi
        (** The file a file wrapper wraps. *)
  | Admin :
      ('t, 't -> Eio.Fs.dir_ty Eio.Resource.t, [> `Admin ]) Eio.Resource.pi
        (** The same directory with root's rights, which the power cut of
            [Eio_fs_chaos] reads and rebuilds a tree through. *)
  | Sync_failed :
      ('t, 't -> evict:bool -> unit, [> `Sync_failed ]) Eio.Resource.pi
        (** A sync a wrapper failed on purpose, told to the file beneath it, so
            a power-loss model there loses what that sync would have written;
            [evict] says the failure dropped the file's cached pages too. *)
  | Rebase :
      ( 't,
        't ->
        (Eio.Fs.dir_ty Eio.Resource.t ->
        [ `Close | Eio.Fs.dir_ty ] Eio.Resource.t) ->
        [ `Close | Eio.Fs.dir_ty ] Eio.Resource.t,
        [> `Rebase ] )
      Eio.Resource.pi
        (** The same directory wrapper over [f base], which [Eio_fs_mem.as_user]
            builds a wrapper as another user with. *)

val base_dir : _ Eio.Resource.t -> Eio.Fs.dir_ty Eio.Resource.t option
(** [base_dir r] is the directory [r] wraps, or [None] when [r] binds no
    {!Base_dir}. *)

val base_file : _ Eio.Resource.t -> Eio.File.ro_ty Eio.Resource.t option
(** [base_file r] is the file [r] wraps, or [None] when [r] binds no
    {!Base_file}. *)

val admin : _ Eio.Resource.t -> Eio.Fs.dir_ty Eio.Resource.t option
(** [admin r] is [r] with root's rights, or [None] when [r] binds no {!Admin}.
*)

val rebase :
  _ Eio.Resource.t ->
  ((Eio.Fs.dir_ty Eio.Resource.t -> [ `Close | Eio.Fs.dir_ty ] Eio.Resource.t) ->
  [ `Close | Eio.Fs.dir_ty ] Eio.Resource.t)
  option
(** [rebase r] rebuilds the wrapper [r] over another base, or is [None] when [r]
    binds no {!Rebase}. *)

val bounded_read :
  max_bytes:int ->
  validate:(Eio.File.Stat.t -> (unit, string) result) ->
  stat:Eio.File.Stat.t ->
  read:(Cstruct.t -> int) ->
  (Cstruct.t, [> `Invalid of string | `Too_large ]) result
(** [bounded_read ~max_bytes ~validate ~stat ~read] is the bounded read of
    [Eio_fs.Nofollow.read_file] over a file the caller has already opened, with
    [max_bytes] between [0] and [max_int - 1]. It validates [stat] before asking
    [read] for bytes and reads through end-of-file with one byte of lookahead
    past [max_bytes]. The buffer offered to [read] starts at [stat]'s size plus
    one byte and doubles toward [max_bytes + 1] only while the file keeps
    answering, so a read costs the file and not the bound. A rejected or
    partially read buffer, and each one outgrown, is overwritten before the
    function returns or raises. [read] answering [0] or raising [End_of_file]
    ends the file, and what [validate] or [read] raises otherwise is propagated.
*)

val last_component : string -> string
(** [last_component path] is the last component of [path], or the whole of
    [path] when it has none: ["/"] is ["/"], [".."] is [".."] and the empty path
    is ["."]. *)
