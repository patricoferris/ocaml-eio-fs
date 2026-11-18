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
(** The type for embedded file trees. Every directory precedes its children. *)

val scan : _ Eio.Path.t -> t
(** [scan dir] serialises the directory tree rooted at [dir]. The root is
    recorded as [Dir { path = "/"; _ }], entries follow in depth-first sorted
    order, and symbolic links are recorded without being followed. An entry that
    is neither a regular file, a directory nor a symbolic link is not recorded;
    it is traced with [Eio.traceln].

    @raise Eio.Io
      if [dir] is not a directory, or if it or an entry under it cannot be read.
*)

val find : string -> t -> string option
(** [find path entries] is [Some contents] for the first {!File} at [path] in
    [entries], and [None] if there is none. [path] is absolute within the image,
    as {!scan} records it, and compared as a string; a symbolic link is not
    followed. A program that embeds fixtures reads them with it and needs no
    filesystem. *)

val load :
  ?clock:_ Eio.Time.clock ->
  ?os:[< `Linux | `Macos ] ->
  t ->
  [ `Close | Eio.Fs.dir_ty ] Eio.Path.t
(** [load ~clock ~os entries] is the root of a new in-memory filesystem that
    answers as [os] (defaults to [`Linux]) does, holding [entries], with their
    permissions and symbolic links. Its times are read from [clock], as
    [Eio_fs_mem.v] reads them. A {!Dir} creates its missing ancestors; a {!File}
    or {!Symlink} needs its parent directory to exist already, as the root or
    from an earlier entry.

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
