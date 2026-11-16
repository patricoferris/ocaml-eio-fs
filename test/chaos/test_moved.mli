(** Power cuts after entries moved, renamed directories included. *)

val suite : string * unit Alcotest.test_case list
(** [suite] checks what a cut keeps of moved entries under directory barriers.
*)
