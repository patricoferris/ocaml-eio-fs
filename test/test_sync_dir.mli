(** Tests for [Eio.Path.sync_dir] as the crash model's directory barrier. *)

val suite : string * unit Alcotest.test_case list
(** [suite] tests what the barrier commits, by name and through a subtree. *)
