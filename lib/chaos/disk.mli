(** The disk of {!Eio_fs_chaos.run}, as a part of {!Eio_chaos.run}: an in-memory
    filesystem, the faults proposed at its operations, and its power cuts. *)

val name : string
(** [name] is the part's name, ["fs"]. *)

val part : unit -> Eio_chaos.part
(** [part ()] is a fresh disk, as a part. *)

val acks : string -> Eio_chaos.env -> Ack.t
(** [acks name env] is the files acknowledged in the run of [env].

    @raise Invalid_argument
      naming [name] if [env] is not that of a run of {!Eio_fs_chaos.run}. *)

val acknowledge : Eio_chaos.env -> bool
(** [acknowledge env] records a value acknowledged in the run of [env], which a
    cut must keep, and is [false] if [env] is that of no disk or its power is
    cut. *)
