(** The window a directory operation with no barrier of its own leaves. *)

val suite : string * unit Alcotest.test_case list
(** [suite] tests what a cut right after such an operation, and after the
    barrier that follows it, leaves. *)
