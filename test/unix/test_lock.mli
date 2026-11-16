(** Tests for the locks of {!Eio_fs_unix} over a rendezvous file. *)

val suite : string * unit Alcotest.test_case list
(** [suite] tests [with_lock], probes, held locks, waits, the lock file's checks
    and sections. *)
