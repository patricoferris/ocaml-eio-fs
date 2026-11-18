(** A volume's space. *)

val suite : string * unit Alcotest.test_case list
(** [suite] tests {!Eio_fs_unix.volume_used} against {!Eio_fs_unix.statvfs}. *)
