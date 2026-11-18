(** A directory tree as values, read from any filesystem and loaded into a new
    in-memory one: [Eio_fs.Crunch]'s entries and [Eio_fs_mem.load]'s copy. *)

(** The type for nodes of a tree, each at its absolute path. *)
type entry =
  | Dir of { path : string; perm : int }
  | File of { path : string; perm : int; contents : string }
  | Symlink of { path : string; target : string }

val scan :
  ?filter:('a Eio.Path.t -> bool) -> at:string -> 'a Eio.Path.t -> entry list
(** [scan ~filter ~at root] is the tree at [root], followed if it is a symbolic
    link, recorded at [at] (made absolute): [root] is [Dir { path = at; _ }] and
    an entry [n] below a recorded [p] is at [p/n]. Entries come in depth-first
    order, names sorted, so every directory precedes its children. A symbolic
    link below [root] is recorded and not followed, and an entry that is neither
    a regular file, a directory nor a symbolic link is not recorded; it is
    traced with [Eio.traceln]. [filter] is {!Walk.iter}'s: a path it refuses is
    not recorded, nor anything under it. A regular file is read through an open
    that does not follow a link, and must be the file its stat found.

    @raise Eio.Io
      if [root] is not a directory, if it or an entry under it cannot be read,
      or if a regular file is replaced between its stat and its open. *)

val load :
  ?os:[< `Linux | `Macos ] ->
  ?clock:_ Eio.Time.clock ->
  entry list ->
  [ `Close | Eio.Fs.dir_ty ] Eio.Path.t
(** [load ~os ~clock entries] is the root of a new in-memory filesystem that
    answers as [os] does, its times read from [clock], holding [entries], as
    root, with their permissions. A {!Dir} creates its missing ancestors; a
    {!File} or {!Symlink} needs its parent directory to exist already. A file's
    contents alias the entry's string until it is first written.

    @raise Eio.Io
      with {!Eio.Fs.Not_found} if the parent of a {!File} or {!Symlink} is
      missing, and with {!Eio.Fs.Already_exists} if its path is already taken.
*)
