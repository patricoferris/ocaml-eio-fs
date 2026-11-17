(** Sequences of filesystem operations and their outcomes, run through
    {!Eio.Path} on any backend: eio_posix to record a kernel, any directory to
    compare with the recording. [fs.csv] beside this module holds what Linux and
    macOS answered. *)

type ids = { gid : int; other_gid : int }
(** The type for the groups an outcome names: the user's primary group, and a
    supplementary group the user is in. *)

val scenarios : (string * string list) list
(** [scenarios] is each scenario's name and operations. *)

val run : ids -> [> Eio.Fs.dir_ty ] Eio.Path.t -> string list -> string list
(** [run ids dir ops] runs [ops] in the empty directory [dir], and is each
    operation's outcome. An exception an operation raises is its outcome, but
    for [Eio.Cancel.Cancelled]. *)

val key : string list -> string
(** [key ops] is how a trace spells [ops]: escaped, separated by [";"]. *)

(** {1:recorder For the recorder}

    What the recorder needs to make the same operations through [Unix]. *)

exception Bad_op of string
(** The exception for an operation no scenario spells so. *)

val words : string -> string list
(** [words op] is the words of [op], each expanded: [{s}n] is [s] repeated [n]
    times and [-] is the empty string. *)

val perm : string -> int
(** [perm s] is the octal mode [s]. *)

val names : string list -> string
(** [names l] is the outcome of a listing of [l]. *)

val errno_of_message : string -> string option
(** [errno_of_message s] is the errno whose strerror text [s] contains, if any.
*)
