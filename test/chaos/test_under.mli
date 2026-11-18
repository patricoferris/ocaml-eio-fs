(** Tests of a program on several parts, and of a filesystem over a faulty
    network. *)

val suite : string * unit Alcotest.test_case list
(** [suite] checks a program on the disk and the network, its pairs of failures
    across both, and [Eio_fs_chaos.device ~under]. *)
