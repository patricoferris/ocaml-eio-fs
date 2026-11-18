(** Sequences of filesystem operations and their outcomes, run through
    {!Eio.Path} on any backend: eio_posix or eio_windows to record a kernel, any
    directory to compare with the recording. [test/interop/kernel/traces/fs.csv]
    holds what Linux, macOS and Windows answered. *)

type ids = { gid : int; other_gid : int }
(** The type for the groups an outcome names: the user's primary group, and a
    supplementary group the user is in. *)

val scenarios : (string * string list) list
(** [scenarios] is each scenario's name and operations. The last ones are
    Windows' traps, what it refuses or resolves otherwise than Linux and macOS:
    reserved names, trailing dots and spaces, a file or directory held open
    while it is moved or removed, [".."] and links in a directory opened as a
    sandbox, links to directories, renames over what exists, and what Eio's
    fixes and reports for Windows name. *)

val run :
  ?root:string ->
  ids ->
  [> Eio.Fs.dir_ty ] Eio.Path.t ->
  string list ->
  string list
(** [run ?root ids dir ops] runs [ops] in the empty directory [dir], and is each
    operation's outcome. An exception an operation raises is its outcome, but
    for [Eio.Cancel.Cancelled]. [root] is the absolute path of [dir], which a
    word [@p] names: [root] and the components of [p], joined with
    {!Filename.dir_sep}. A file or directory an operation holds open stays open
    until [run] returns.

    @raise Bad_op if an operation has a word [@p] and [root] is not given. *)

val key : string list -> string
(** [key ops] is how a trace spells [ops]: escaped, separated by [";"]. *)

(** {1:recorder For the recorder}

    What the recorder needs to make the same operations through [Unix]. *)

exception Bad_op of string
(** The exception for an operation no scenario spells so. *)

val words : ?root:string -> string -> string list
(** [words ?root op] is the words of [op], each expanded: [p{s}n] is [p]
    followed by [s] repeated [n] times, [-] is the empty string, [\NNN] is the
    byte of decimal code [NNN], a space included, and [@p] is [p] under [root],
    as {!run} says. *)

val perm : string -> int
(** [perm s] is the octal mode [s]. *)

val names : string list -> string
(** [names l] is the outcome of a listing of [l]. *)

val errno_of_message : string -> string option
(** [errno_of_message s] is the errno whose strerror text [s] contains, if any.
*)
