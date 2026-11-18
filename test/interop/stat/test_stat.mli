(** The in-memory filesystem's stat answers against Linux's. *)

val suite : string * unit Alcotest.test_case list
(** [suite] replays each workload of [traces/linux.csv]. *)
