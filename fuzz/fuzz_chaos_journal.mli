(** Fuzz tests for [Chaos_journal]: the indexes of a metadata journal and what
    its barriers commit. *)

val suite : string * Alcobar.test_case list
(** [suite] is the fuzz test suite. *)
