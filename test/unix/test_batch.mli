(** Confined batched directory operations. *)

val suite : string * unit Alcotest.test_case list
(** [suite] tests confinement, per-operation errors, the hop count and held
    parents. *)
