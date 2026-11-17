(** The law of [Eio_fs_chaos.wrap]: what it does with each operation. *)

val suite : string * unit Alcotest.test_case list
(** [suite] checks that every interface a wrapped directory or file binds has an
    entry, and that a wrapper with no fault does what the bare in-memory
    filesystem does over generated programs that close, close twice, use after
    close, release a switch with resources open and cancel a fiber in the middle
    of an operation. *)
