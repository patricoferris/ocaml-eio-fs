(** Tests for the POSIX durability barrier, positioned calls, the sparse map,
    tree copies and removals, and path calls. *)

val suite : string * unit Alcotest.test_case list
(** [suite] tests {!Eio_fs_unix} outside its locks. *)
