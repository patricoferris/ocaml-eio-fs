(** What the host's own filesystem does that a test relies on. *)

val posix_only : string -> (unit -> unit) -> unit -> unit
(** [posix_only why f ()] is [f ()], and skips the test on Windows, saying
    [why]: what Windows, or eio_windows, does not do that [f] needs. *)
