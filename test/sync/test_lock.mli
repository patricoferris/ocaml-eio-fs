(** Tests for {!Eio_fs.Sync.Lock}: the critical section a caller supplies. *)

val suite : string * unit Alcotest.test_case list
(** [suite] is the lock test suite. *)
