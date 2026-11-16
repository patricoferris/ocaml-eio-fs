(** [Extents], the byte ranges a write covered in a crash-tracked file, on
    chosen edges. *)

val suite : string * unit Alcotest.test_case list
(** [suite] checks merging, cutting, overlaps, page counts and folds. *)
