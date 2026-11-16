(** Fuzz tests for [Chaos_blocks]: the images a cut leaves of a file's pending
    changes. *)

val suite : string * Alcobar.test_case list
(** [suite] is the fuzz test suite. *)
