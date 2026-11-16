(** A differential of {!Eio_fs.Chaos.Private.Crash} against a reference crash
    model that answers which images the documented envelope permits, under every
    policy, with directed cases. *)

val suite : string * unit Alcotest.test_case list
(** [suite] runs random workloads and the directed cases against the reference.
*)
