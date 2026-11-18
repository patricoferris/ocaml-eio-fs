(** How a kernel compares and bounds names. Linux compares bytes. APFS's default
    volume on macOS compares a name by its key: decomposed (NFD), case-folded,
    decomposed again, Unicode's canonical caseless match; it keeps the spelling
    a name was made with. A name that is not UTF-8, which APFS never stores, is
    its own key. NTFS compares each character's simple uppercase. *)

val key : [< `Linux | `Macos | `Windows ] -> string -> string
(** [key os name] is the key [os] compares [name] by. *)

val too_long : [< `Linux | `Macos | `Windows ] -> string -> bool
(** [too_long os name] is [true] iff [name] is longer than NAME_MAX: 255 bytes
    on Linux, 255 UTF-16 code units on APFS and NTFS. *)
