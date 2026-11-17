(** Lexical arithmetic on the paths an {!Eio} filesystem is addressed by: Eio
    paths, whose separator is always ["/"] even on Windows, so the host's own
    path modules do not apply. Everything here is lexical; nothing opens a file
    or asks the filesystem anything. {!normalize} rewrites a [".."], sound only
    where the component before it is not a link; {!canonical} refuses one
    instead. *)

val is_abs : string -> bool
(** [is_abs path] is [true] iff [path] starts at the root, that is iff it opens
    with a separator. *)

val segs : string -> string list
(** [segs path] is [path] cut at every separator, empty segments kept: an
    absolute path opens with one, a directory path ends with one, and a doubled
    separator is one of its own. It is the inverse of joining with ["/"], so
    nothing about the spelling is lost: [segs ""] is [[""]]. *)

val normalize : string -> string
(** [normalize path] is [path] with its empty and ["."] segments dropped and its
    [".."] segments resolved against the segment before them, so the spellings
    of one path (["/a//b"], ["/a/./b"], ["/a/x/../b"]) share a name. A [".."]
    that would climb above an absolute root is dropped; one at the head of a
    relative path is kept, there being nothing to resolve it against. Directory
    syntax survives: a [path] ending in a separator, a ["."] or a [".."] is
    answered ending in a separator. A path left with no segment is ["/"] when
    absolute and ["."] otherwise, so [normalize ""] is ["."] and [normalize "."]
    is ["./"]. *)

val append : string -> string -> string
(** [append base path] is [path] resolved against [base]: [path] itself when it
    is absolute, and otherwise [base] and [path] joined by a separator, added
    only when [base] is not empty and does not already end in one. *)

val basename : string -> string
(** [basename path] is [path]'s last component, ignoring trailing separators,
    and [""] for a path that has none: the empty path, a root, ["."] or [".."].
*)

(** The type for the reasons {!canonical} refuses a path. *)
type refusal =
  | Not_a_path  (** Empty, or holding a NUL byte. *)
  | Relative  (** Does not start at the root. *)
  | Root  (** Is the root, which has no components. *)
  | Not_canonical  (** Names something, by a spelling that is not its own. *)

val canonical : string -> (string list, refusal) result
(** [canonical path] is [Ok cs], [cs] the components of [path], when [path] is
    absolute and already canonical, and otherwise [Error r] with [r] the first
    of these that holds:
    - {!Not_a_path} if [path] is empty or holds a NUL byte.
    - {!Relative} if [path] does not start with a separator.
    - {!Root} if [path] has no segment but empty ones (["/"], ["//"]).
    - {!Not_canonical} if a segment is ["."] or [".."], or if an empty segment
      follows the leading one (a trailing or doubled separator).

    It refuses rather than rewrites: rewriting [a/b/..] to [a] names a different
    directory wherever [b] is a symbolic link, so a caller resolving components
    under [O_NOFOLLOW] needs the refusal. *)
