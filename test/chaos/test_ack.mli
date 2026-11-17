(** Tests of the files a program acknowledges, and of [inspect]. *)

val suite : string * unit Alcotest.test_case list
(** [suite] checks that a file acknowledged and lost by a power cut fails
    [Eio_fs_chaos.run], and that [inspect] sees what was acknowledged. *)
