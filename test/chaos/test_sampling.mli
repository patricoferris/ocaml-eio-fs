(** The sampled tier, on candidates with a period. *)

val suite : string * unit Alcotest.test_case list
(** [suite] checks that a period of the candidates hides none from the sampled
    tier. *)
