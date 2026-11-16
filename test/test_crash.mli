(** Tests for the crash model of {!Eio_fs.Chaos.Private}. *)

val suite : string * unit Alcotest.test_case list
(** [suite] tests what a power cut leaves of files and directories, the barriers
    that make them durable, and the authority the wrapper keeps. *)
