(** In-memory {!Eio.Fs} filesystem.

    For hermetic tests: code reads and writes files through {!Eio.Path} without
    touching the host's disk. Each call to {!v}, {!load} or {!run} makes a
    filesystem of its own: two of them share no file. The library [eio-fs.chaos]
    tests programs and filesystems with it.

    Where Linux and macOS differ, the filesystem follows one, chosen by [?os]:
    [`Linux], the default, or [`Macos]. The choice comes from the argument,
    never from the host, so a test sees the same filesystem on every machine.
    {!section-macos} lists what [`Macos] changes.

    An entry's stat carries what Linux gives on tmpfs. Its times come from the
    clock given to {!v}, and stay at [0.] without one; under [Eio_fs_chaos.run]
    they come from the run's clock. A write, a truncation and an open that
    truncates move a file's [mtime] and [ctime]. A chmod, a chown and a rename
    move the entry's [ctime]. Making, removing or renaming an entry moves its
    directory's [mtime] and [ctime]. [atime] follows relatime, Linux's default:
    a read, a listing or a link read moves it when it is not past [mtime] or
    [ctime], or a day after it last moved. [dev] is the filesystem's own, shared
    with no other filesystem and no host device. [nlink] is [2] plus the number
    of subdirectories for a directory, [1] for anything else, and [0] once its
    last name is gone. [blocks] counts the 512-byte blocks of the 4096-byte
    pages writes put bytes in: a hole, a directory and a link hold none.

    A file's bytes live in memory, holes included, and a file holds at most 4
    GiB: a write or a truncation past that raises {!Eio.Fs.File_too_large}.

    Where POSIX systems differ, the filesystem answers under [`Linux] as Linux
    does: {!Eio.Path.unlink} of a directory raises the error EISDIR names, not
    [Permission_denied], a positioned write to a file opened to append lands at
    its end, [Eio.Path.chmod ~follow:false] on a symbolic link raises the error
    EOPNOTSUPP names, and a symbolic link to the empty path is refused with
    [Not_found]. Under [`Macos] it answers as macOS does: see {!section-macos}.
    A write of no bytes returns [0] under either, where eio_posix on macOS
    raises the error EINVAL names from writev(2). Its paths have no native
    spelling ({!Eio.Path.native} is [None]). The root {!v} returns stands for
    [Eio.Stdenv.fs]: an absolute path or link target resolves from it, as
    eio_posix resolves one from [/] through [fs]. One filesystem may be used
    from several domains at once.

    A path resolves one component at a time, as path_resolution(7) describes: a
    [".."] climbs from the directory the walk has reached, a link's target
    included, never from a lexical rewrite of the path. A directory opened from
    the filesystem is confined as eio_posix confines one: an absolute path, an
    absolute link target and a [".."] above that directory are refused with
    [Eio.Fs.Permission_denied], whose message names the path asked for and not
    where the directory sits.

    {b eio_posix.} The filesystem answers as the kernel does, and eio_posix
    differs from the kernel on a few paths. Code that runs on both should not
    depend on them:

    - a path that ends in a ["."] or [".."] component, which eio_posix rewrites
      lexically ({{:https://github.com/ocaml-multicore/eio/issues/976}eio#976});
    - a creating open of a name with a trailing slash, which eio_posix fails
      with another error than the kernel's;
    - on macOS, a path through a directory with no read bit, which eio_posix
      refuses, and a stat through more than 32 symbolic links, which it follows
      up to 40.

    {b Permissions.} Each operation runs as a {!type-user} and meets the POSIX
    rules:

    - search ([x]) on every directory a path walks; write and search on the
      directory where an entry is made, removed or renamed, with the sticky bit
      keeping removal and renaming to the entry's or the directory's owner; read
      on a directory to list it; read, or read and write, on a file to open it.
    - In a sticky directory that others may write, a link is followed as the
      last component of a path only by its owner, or if the directory's owner
      owns it, root included: Linux does so with [fs.protected_symlinks] set, as
      most distributions set it.
    - A new file or directory loses the user's umask bits and belongs to the
      user's uid and gid, or to the group of a setgid directory it is made in.
    - A new directory keeps only the permission and sticky bits of its mode, and
      is setgid iff its parent is.
    - A file made in a setgid directory by a user outside that group loses
      setgid, as Linux strips it at create.
    - Only a file's owner changes its mode, and a user outside the file's group
      cannot set its setgid bit.
    - Only root gives a file away; its owner may move it to one of their own
      groups.
    - A chown of a regular file, or a write to one by a user other than root,
      clears its setuid bit, and its setgid bit when its group may execute it.
    - Root needs no bit to read, write or search, as on Linux, so a test that
      runs as {!root}, the default, misses the bugs a user meets: a directory it
      cannot create in, a file it cannot open. *)

(** {1:errors Errors}

    An error Eio classifies raises the {!Eio.Fs} error of its class:
    [Not_found], [Already_exists], [Permission_denied], [File_too_large], and
    [Symlink] for a path through too many links. Every other error is an
    {!Eio.Io} error of no class, as eio_posix raises it. Each carries {!Eio_fs},
    but [Symlink], which carries nothing, and the [Permission_denied] of a
    confined directory, whose code names the path asked for. Below, "the error
    EISDIR names" is the one whose {!Eio_fs} names EISDIR's error, "is a
    directory", and likewise for each errno. *)

type Eio.Exn.Backend.t +=
  | Eio_fs of string
        (** The backend code of the filesystem's errors: the operation, the
            error and the path, as ["rmdir: directory not empty: /d"]. *)

(** {1:users Users} *)

type user = {
  uid : int64;  (** The user id, as {!Eio.File.Stat.t} carries it. *)
  gid : int64;  (** The primary group id. *)
  groups : int64 list;  (** The supplementary group ids. *)
  umask : int;  (** The mode bits a new file or directory loses. *)
}
(** The type for the user an in-memory filesystem's operations run as. *)

val root : user
(** [root] is uid [0], gid [0], no supplementary group and umask [0o022]. *)

(** {1:filesystems Filesystems} *)

val v :
  ?clock:_ Eio.Time.clock ->
  ?user:user ->
  ?os:[< `Linux | `Macos ] ->
  unit ->
  [ `Close | Eio.Fs.dir_ty ] Eio.Path.t
(** [v ~clock ~user ~os ()] is the root of a fresh, empty in-memory filesystem
    whose operations run as [user] (defaults to {!root}), who owns its root
    directory, and answer as [os] (defaults to [`Linux]) does. Its times are
    read from [clock]; without one, they stay at [0.]. Closing this capability
    prevents further operations through its aliases. Directories already opened
    from it keep their own lifetimes. *)

val as_user : user -> 'a Eio.Path.t -> [ `Close | Eio.Fs.dir_ty ] Eio.Path.t
(** [as_user user path] is [path], an in-memory directory, with its operations
    run as [user]. A test sets a tree up as {!root} and runs the code under test
    as a user. A directory opened from the result shares its descriptor with
    [path]'s. A wrapper of [Eio_fs_chaos] over an in-memory directory, such as a
    run's [env#fs], stays the same wrapper, over the directory as [user].

    @raise Invalid_argument
      if [path] is neither an in-memory directory nor such a wrapper. *)

val load :
  ?clock:_ Eio.Time.clock ->
  ?filter:('a Eio.Path.t -> bool) ->
  ?os:[< `Linux | `Macos ] ->
  'a Eio.Path.t ->
  [ `Close | Eio.Fs.dir_ty ] Eio.Path.t
(** [load ~clock ~filter ~os path] is the root of a new in-memory filesystem
    that answers as [os] (defaults to [`Linux]) does, holding a copy of the
    directory tree at [path], at the same path from its root:
    [load (dir / "a/b")] holds [/a/b]. [path] is followed if it is a symbolic
    link; the entries below it are copied as they are, a link kept a link.
    [filter] (defaults to [fun _ -> true]) is called on [path], then on every
    path below it, before anything of it is copied: a path for which it is
    [false] is not copied, nor anything under it, so a refused [path] is an
    empty filesystem. [path] can be on any filesystem, an in-memory one
    included. Its times are read from [clock], as {!v} reads them: a copy is a
    new file, made now.

    An entry that is neither a regular file, a directory nor a symbolic link is
    not copied; it is traced with [Eio.traceln].

    @raise Eio.Io
      if [path] is not a directory, if it or an entry under it cannot be read,
      or if a regular file is replaced between the stat that found it and the
      open that copies it: that open does not follow a link. *)

type stdenv = < fs : Eio.Fs.dir_ty Eio.Path.t ; cwd : Eio.Fs.dir_ty Eio.Path.t >
(** The type for the standard environment {!run} passes on: an in-memory
    filesystem and current directory. *)

val run :
  ?clock:_ Eio.Time.clock ->
  ?cwd:string ->
  ?user:user ->
  ?os:[< `Linux | `Macos ] ->
  (stdenv -> 'a) ->
  'a
(** [run ~clock ~cwd ~user ~os fn] calls [fn] with a new in-memory filesystem in
    place of the host's, that answers as [os] (defaults to [`Linux]) does, whose
    times are read from [clock], as {!v} reads them. Unlike [Eio_main.run], it
    starts no event loop and installs no effect handler: call it inside one,
    such as [Eio_main.run] or [Eio_mock.Backend.run]. [fn]'s argument holds the
    root and the current directory of the new filesystem. The current directory
    is the directory at the absolute path [cwd] (defaults to ["/"]), made with
    its missing ancestors, mode [0o755], before [fn] runs. Like a process's, it
    follows that directory through a rename. Both run as [user] (defaults to
    {!root}). Root owns the tree and gives [user] the current directory.

    @raise Invalid_argument if [cwd] is not an absolute path. *)

(** {1:macos macOS}

    Under [`Macos] the filesystem answers as macOS does on its default APFS
    volume, where it differs from Linux:

    - A name matches every name with the same canonical caseless form: case and
      Unicode normalisation do not count. An entry keeps the spelling it was
      made with. A rename to another spelling of its own name respells it; one
      over another entry keeps that entry's spelling. A name that is not UTF-8
      is refused with the error EILSEQ names, and one longer than 255 UTF-16
      code units with ENAMETOOLONG's.
    - {!Eio.Path.unlink} of a directory raises [Permission_denied].
    - A last component in directory syntax that is a symbolic link is followed,
      even to make, remove or move an entry: {!Eio.Path.mkdir},
      {!Eio.Path.rmdir} and {!Eio.Path.rename} of ["l/"] act on the link's
      target. A creating open of a name in directory syntax raises [Not_found]
      if it is missing, and the error ENOTDIR or EISDIR names otherwise. A
      rename of a file to a missing name in directory syntax raises [Not_found].
    - {!Eio.Path.rmdir} of a last [".."], and {!Eio.Path.rename} of a last ["."]
      or [".."], raise the error EINVAL names.
    - A positioned write to a file opened to append lands at its offset.
    - A symbolic link may point to the empty path, and
      [Eio.Path.chmod ~follow:false] changes a link's mode.
    - A path follows at most 32 symbolic links.
    - A new entry takes its directory's group, and a new directory keeps only
      the permission bits of its mode.
    - Opening a directory, {!Eio.Path.open_dir} included, needs its read bit.
    - No [fs.protected_symlinks]: any link is followed. *)
