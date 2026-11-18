(** A regular file of the in-memory tree: its stat, its bytes and the ranges a
    write covered. *)

type contents =
  | Literal of string
      (** An immutable string (an embedded asset), turned into an owned
          {!constructor-Buffer} by the first write, so embedding a file costs no
          copy until someone mutates it. *)
  | Buffer of Cstruct.t
      (** At least as long as the file, whose length is the stat's size, and
          grown by doubling, so a run of writes costs linear time. *)

type t = {
  stat : Eio.File.Stat.t;
  contents : contents;
  held : Extents.t;
      (** The byte ranges a write covered: the rest of the file is a hole. *)
}

val v : stat:Eio.File.Stat.t -> t
(** [v ~stat] is an empty file. *)

val literal : stat:Eio.File.Stat.t -> string -> t
(** [literal ~stat s] is a file holding [s], without a copy. *)

val size : t -> int
(** [size f] is [f]'s size in bytes. *)

val blocks : t -> int64
(** [blocks f] is the 512-byte blocks of the 4096-byte pages a write put bytes
    in, as tmpfs counts them: a hole holds none. *)

val blit :
  contents -> src_off:int -> Cstruct.t -> dst_off:int -> len:int -> unit
(** [blit c ~src_off dst ~dst_off ~len] copies [len] bytes of [c] from [src_off]
    into [dst] at [dst_off]. *)

val write : t -> off:int -> Cstruct.t -> t
(** [write f ~off src] is [f] with [src] written at [off].

    @raise Eio.Io [File_too_large] past 4 GiB. *)

val truncate : t -> int -> t
(** [truncate f n] is [f] cut or zero-extended to [n] bytes.

    @raise Eio.Io [File_too_large] past 4 GiB. *)
