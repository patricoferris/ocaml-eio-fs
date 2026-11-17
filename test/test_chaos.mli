(** Fault-injection tests for {!Eio_fs.Chaos.wrap}. *)

val suite : string * unit Alcotest.test_case list
(** [suite] tests the faults the wrapper injects. *)
