(** Tests for the crash model's writeback. *)

val suite : string * unit Alcotest.test_case list
(** [suite] tests which blocks, sectors and lengths a power cut keeps of the
    writes no sync made durable. *)
