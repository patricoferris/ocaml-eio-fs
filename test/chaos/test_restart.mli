(** Tests of how [Eio_fs.Chaos.check] treats a run that ends with [Eio.Io]. *)

val suite : string * unit Alcotest.test_case list
(** [suite] checks that a run may end with [Eio.Io] if the workload, run again
    on what it left, returns, and that it fails otherwise. *)
