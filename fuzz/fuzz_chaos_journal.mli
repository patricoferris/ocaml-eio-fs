(** Fuzz tests for [Journal]: the indexes of a metadata journal and what its
    barriers commit. *)

val suite : string * Alcobar.test_case list
(** [suite] is the fuzz test suite. *)
