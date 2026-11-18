(** Filesystem helpers for {!Eio}.

    [Eio_fs] embeds a directory tree into an OCaml module at compile time
    ({!Crunch}), walks over a tree ({!Tree}) and reads a file without following
    a symbolic link ({!Nofollow}). {!Nofollow} and {!Tree} work on any {!Eio}
    backend, the in-memory filesystem of [eio-fs.mem] included; each call to
    {!Crunch.load} makes such a filesystem of its own. *)

(** Compile-time file embedding for the in-memory filesystem, in the style of
    ocaml-crunch. The [eio-fs-crunch] binary serialises a directory tree into an
    OCaml module of {!Crunch.entry} values; at runtime the generated module's
    [load ()] rebuilds the tree in a new in-memory filesystem.

    With [-o assets.ml] the binary writes the module and its interface,
    [assets.mli]. This dune rule regenerates them when the [static] directory
    changes:
    {v
    (rule
     (targets assets.ml assets.mli)
     (deps
      (source_tree static))
     (action
      (run %{bin:eio-fs-crunch} -o assets.ml static)))
    v} *)
module Crunch : sig
  (** The type for nodes of an embedded file tree. Paths are absolute within the
      image and use ["/"] as separator. *)
  type entry =
    | Dir of { path : string; perm : int }
        (** A directory at [path] with mode [perm]. *)
    | File of { path : string; perm : int; contents : string }
        (** A regular file at [path] with mode [perm], holding [contents]. *)
    | Symlink of { path : string; target : string }
        (** A symbolic link at [path] holding [target], never resolved. *)

  type t = entry list
  (** The type for embedded file trees. Every directory precedes its children.
  *)

  val scan : _ Eio.Path.t -> t
  (** [scan dir] serialises the directory tree rooted at [dir]. The root is
      recorded as [Dir { path = "/"; _ }], entries follow in depth-first sorted
      order, and symbolic links are recorded without being followed. An entry
      that is neither a regular file, a directory nor a symbolic link is not
      recorded; it is traced with [Eio.traceln].

      @raise Eio.Io
        if [dir] is not a directory, or if it or an entry under it cannot be
        read. *)

  val find : string -> t -> string option
  (** [find path entries] is [Some contents] for the first {!File} at [path] in
      [entries], and [None] if there is none. [path] is absolute within the
      image, as {!scan} records it, and compared as a string; a symbolic link is
      not followed. A program that embeds fixtures reads them with it and needs
      no filesystem. *)

  val load :
    ?clock:_ Eio.Time.clock ->
    ?os:[< `Linux | `Macos ] ->
    t ->
    [ `Close | Eio.Fs.dir_ty ] Eio.Path.t
  (** [load ~clock ~os entries] is the root of a new in-memory filesystem that
      answers as [os] (defaults to [`Linux]) does, holding [entries], with their
      permissions and symbolic links. Its times are read from [clock], as
      [Eio_fs_mem.v] reads them. A {!Dir} creates its missing ancestors; a
      {!File} or {!Symlink} needs its parent directory to exist already, as the
      root or from an earlier entry.

      @raise Eio.Io
        with {!Eio.Fs.Not_found} if the parent of a {!File} or {!Symlink} is
        missing, and with {!Eio.Fs.Already_exists} if its path is already taken.
  *)

  val source : t Fmt.t
  (** [source] formats a standalone OCaml module embedding the entries. The
      generated module exposes [entries : Eio_fs.Crunch.t] and
      [load ?clock ?os ()], which is {!load} of [entries] with the same optional
      arguments. *)

  val interface : string
  (** [interface] is the [.mli] file matching what {!source} prints: it declares
      the generated module's entry list and its loading function. *)
end

(** Trees and directories on any {!Eio} backend:

    - walks over the tree under a path, which measure it ({!val-usage}), visit
      it ({!iter}), read its links ({!links}), change its modes ({!grant_owner},
      {!revoke_writes}), copy its files with other links apart ({!unshare}) or
      format it ({!pp_tree});
    - the names a directory holds ({!entries}), and the removal of a chain of
      empty directories ({!remove_empty});
    - what a path is called outside the program ({!name}), and whether two paths
      name one file ({!same_file}). *)
module Tree : sig
  type usage = {
    allocated_bytes : int64;
        (** The bytes every entry's allocated 512-byte blocks hold, whatever its
            apparent length. *)
    apparent_bytes : int64;  (** The lengths of the regular files. *)
    files : int;  (** The number of regular files. *)
    latest_mtime : float;
        (** The latest modification time of any entry, in seconds since the
            epoch. *)
  }
  (** The type for what a tree costs. Both byte counts saturate at
      [Int64.max_int]. *)

  val usage : ?max_fibers:int -> _ Eio.Path.t -> usage
  (** [usage ~max_fibers path] measures [path] and all descendants in one walk,
      without following symbolic links. The walk is breadth-first, at most
      [max_fibers] workers per level (defaults to [32]).

      @raise Invalid_argument if [max_fibers] is not positive.
      @raise Eio.Io as a stat or a listing of the walk raises. *)

  val iter :
    ?max_fibers:int ->
    ('a Eio.Path.t -> Eio.File.Stat.t -> unit) ->
    'a Eio.Path.t ->
    unit
  (** [iter ~max_fibers f path] is [f p stat] for [path] and each of its
      descendants [p], with [stat] its one [Eio.Path.stat ~follow:false]:
      symbolic links are not followed. A directory's [f] runs before its entries
      are read. The walk is {!val-usage}'s: breadth-first, at most [max_fibers]
      workers per level (defaults to [32]).

      Filesystem errors and those [f] raises are propagated, the walk's other
      workers cancelled.

      @raise Invalid_argument if [max_fibers] is not positive. *)

  val grant_owner : _ Eio.Path.t -> unit
  (** [grant_owner p] grants its owner read, write and search on the directory
      [p] and on each directory under it, so that every entry of the tree can be
      unlinked: a tree that a program made read-only, then left when it was
      killed, refuses any removal. Symbolic links are not followed, so the walk
      stays in [p]'s tree. A directory that cannot be granted or read is left as
      it is, for the removal to name; a [p] that is missing or not a directory
      is left alone. It costs a stat per entry and a listing per directory. *)

  val revoke_writes : _ Eio.Path.t -> unit
  (** [revoke_writes p] removes every write bit from [p] and from each entry
      under it but symbolic links, which are neither followed nor changed. An
      entry that has none is left as it is, since a chmod moves its change time
      even to the same mode. It costs a stat per entry and a listing per
      directory.

      @raise Eio.Io when an entry cannot be read or changed. *)

  val entries : _ Eio.Path.t -> string list
  (** [entries dir] is the names in the directory [dir], and [[]] when nothing
      stands at [dir].

      @raise Eio.Io when [dir] cannot be listed for another reason. *)

  val remove_empty : 'a Eio.Path.t list -> (unit, 'a Eio.Path.t * exn) result
  (** [remove_empty dirs] removes each directory of [dirs] in turn while it is
      empty, as Git removes the directories a deleted ref leaves: the first that
      holds anything, or is gone, ends it with [Ok ()], and stays with those
      after it. It is [Error (d, e)] for the first [d] whose removal failed for
      another reason, [e] the {!Eio.Io} it raised. *)

  val links : _ Eio.Path.t -> string -> (string * string) list option
  (** [links dir rel] is each symbolic link at or below [rel] in [dir] with its
      target, read and not followed, when [rel] is a link, or a directory whose
      entries are each a link or such a directory. A link is named by its path
      from [dir]: an entry [n] of a directory [rel] is [rel/n], a trailing slash
      of [rel] dropped. It is [None] when [rel], or an entry below it, is
      anything else: a file, an empty directory, or nothing. The walk stops at
      the first such entry.

      @raise Eio.Io when a directory or a link of the walk cannot be read. *)

  val name : _ Eio.Path.t -> string
  (** [name path] is what [path] is called outside the program: its native
      spelling when the capability has one, and the rendering {!Eio.Path.pp}
      gives it when not. *)

  val same_file : _ Eio.Path.t -> _ Eio.Path.t -> bool
  (** [same_file a b] is [true] iff [a] and [b] both name a file and their stats
      give the same device and inode: the same file, by any of its names.
      Symbolic links are followed, and a path that names nothing matches no
      path, itself included.

      @raise Eio.Io as a stat fails for another reason than a missing file. *)

  val unshare : spare:'a Eio.Path.t -> 'a Eio.Path.t -> (unit, string) result
  (** [unshare ~spare path] replaces each regular file at or under [path] that
      has more than one link by a copy of itself, so a write to it reaches no
      other link. A copy is made beside [spare], named [spare] with a count
      appended, with the file's permission bits, and renamed over the file.
      Symbolic links are not followed, as {!iter} does not follow them. It is
      [Error why] for the first file that cannot be copied apart, [why] naming
      it and the reason; the files copied before it stay apart.

      @raise Eio.Io as a stat or a listing of the walk raises. *)

  val pp_tree : depth:int -> _ Eio.Path.t Fmt.t
  (** [pp_tree ~depth] formats a path and what is under it [depth] levels deep,
      as tree(1) lists it: each entry by its last component, a symbolic link as
      [name -> target], not followed. [depth = 1] is the path alone, and
      [depth <= 0] prints nothing. A missing path prints nothing, and an entry
      that is neither a regular file, a directory nor a symbolic link (a FIFO, a
      socket, a device) is left out with what is under it.

      @raise Eio.Io
        if a directory cannot be listed or a link read; what was formatted
        before stays formatted. *)
end

(** Bounded reads that refuse a symbolic link at the final path component. *)
module Nofollow : sig
  type error =
    [ `Invalid of string
      (** The [validate] function refused the file's stat, for the reason given.
      *)
    | `Too_large  (** The file holds more than [max_bytes] bytes. *) ]
  (** The type for read refusals. *)

  val read_file :
    max_bytes:int ->
    validate:(Eio.File.Stat.t -> (unit, string) result) ->
    _ Eio.Path.t ->
    (Cstruct.t, error) result
  (** [read_file ~max_bytes ~validate path] opens [path] as
      [Eio.Path.open_in ~follow:false] does, validates the opened file's
      {!Eio.File.stat} with [validate], and reads through end-of-file with one
      byte of lookahead past [max_bytes]. Validation happens before any byte is
      read. The result is [Ok data] with the file's bytes, [Error (`Invalid r)]
      if [validate] answered [Error r], and [Error `Too_large] if the file holds
      more than [max_bytes] bytes. The returned [Cstruct.t] belongs to the
      caller and can be overwritten after use.

      Prefix components keep ordinary {!Eio.Path} resolution and confinement;
      only the final component is refused when it is a symbolic link. Every
      backend that honours [~follow:false] gets the same refusal, and a wrapper
      of [Eio_fs_chaos] observes the one open.

      @raise Invalid_argument if [max_bytes] is negative or [max_int].
      @raise Eio.Io
        when the open or read fails, including when the final component is a
        symbolic link. *)

  val with_regular :
    _ Eio.Path.t ->
    (Eio.File.ro_ty Eio.Resource.t -> Eio.File.Stat.t -> 'a) ->
    ('a, [ `Invalid of string ]) result
  (** [with_regular path f] is [Ok (f file stat)], [file] the file at [path]
      opened as [Eio.Path.open_in ~follow:false] opens it and [stat] its stat
      once opened, when [path] is a regular file of one link. It is
      [Error (`Invalid why)] otherwise, [why] saying what [path] is, such as
      ["a symbolic link, not a regular file"] or ["a file of 2 links"], and [f]
      is not called. [path] is stated before the open, so a FIFO, which an open
      or a read waits on, and a device, which a read never ends, are refused
      unopened. The opened file is checked again: a rename over [path] once it
      is opened leaves it with no name, and it is read as it was opened.

      @raise Eio.Io when the stat or the open fails, or as [f] raises. *)
end
