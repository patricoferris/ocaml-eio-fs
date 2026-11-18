(** Bounded reads that refuse a symbolic link at the final path component. *)

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
    read. The result is [Ok data] with the file's bytes, [Error (`Invalid r)] if
    [validate] answered [Error r], and [Error `Too_large] if the file holds more
    than [max_bytes] bytes. The returned [Cstruct.t] belongs to the caller and
    can be overwritten after use.

    Prefix components keep ordinary {!Eio.Path} resolution and confinement; only
    the final component is refused when it is a symbolic link. Every backend
    that honours [~follow:false] gets the same refusal, and a wrapper of
    [Eio_fs_chaos] observes the one open.

    @raise Invalid_argument if [max_bytes] is negative or [max_int].
    @raise Eio.Io
      when the open or read fails, including when the final component is a
      symbolic link. *)

val with_regular :
  _ Eio.Path.t ->
  (Eio.File.ro_ty Eio.Resource.t -> Eio.File.Stat.t -> 'a) ->
  ('a, [ `Invalid of string ]) result
(** [with_regular path f] is [Ok (f file stat)], [file] the file at [path]
    opened as [Eio.Path.open_in ~follow:false] opens it and [stat] its stat once
    opened, when [path] is a regular file of one link. It is
    [Error (`Invalid why)] otherwise, [why] saying what [path] is, such as
    ["a symbolic link, not a regular file"] or ["a file of 2 links"], and [f] is
    not called. [path] is stated before the open, so a FIFO, which an open or a
    read waits on, and a device, which a read never ends, are refused unopened.
    The opened file is checked again: a rename over [path] once it is opened
    leaves it with no name, and it is read as it was opened.

    @raise Eio.Io when the stat or the open fails, or as [f] raises. *)
