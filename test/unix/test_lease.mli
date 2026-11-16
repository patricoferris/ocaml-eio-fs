(** Tests for {!Eio_fs_unix.Lease}. *)

val suite : string * unit Alcotest.test_case list
(** [suite] tests leases taken for a callback, for a switch and after readers.
*)

val lease_suite : string * unit Alcotest.test_case list
(** [lease_suite] is the inherited-descriptor cases, which the test executable
    also runs alone. *)
