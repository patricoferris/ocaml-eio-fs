(** Tests for the generation publish under the crash model. *)

val suite : string * unit Alcotest.test_case list
(** [suite] tests {!Eio_fs.Sync.publish_generation}. *)
