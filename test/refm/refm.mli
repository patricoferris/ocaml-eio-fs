(** An ext4-like reference for one file's data under failed syncs, apart from
    [Private.Crash].

    Each block keeps sets of possible contents: in the page cache, on disk
    (durable), possibly written by a partly failed writeback, and the versions
    dirtied since the last writeback. It over-approximates what Linux may leave;
    the model under test must stay inside it. *)

module IS : Set.S with type elt = int

(** The type for changes to a file's data. *)
type chg =
  | CW of int * string  (** A write of bytes at an offset. *)
  | CR of int  (** A resize to a length. *)

type t
(** The type for references of one file. *)

val v : string -> t
(** [v s] is a file whose content [s] is durable. *)

val copy : t -> t
(** [copy r] is a reference that changes apart from [r]. *)

val length : t -> int
(** [length r] is the file's length in the page cache. *)

val pending : t -> chg list
(** [pending r] is the changes since the last sync that succeeded, a failed
    one's lost writes left out, in program order. *)

val write : t -> int -> string -> unit
(** [write r off data] writes [data] at [off]. *)

val resize : t -> int -> unit
(** [resize r m] truncates or extends the file to [m] bytes. *)

val sync_ok : t -> unit
(** [sync_ok r] is a sync that succeeds. *)

val sync_fail : reorder:bool -> t -> unit
(** [sync_fail ~reorder r] is a sync that fails. With [reorder], the writeback
    may have written part of the dirty blocks, and the page cache may evict
    them. *)

(** The type for what a crash keeps of the unsynced changes. *)
type policy =
  | Drop  (** None of them. *)
  | Survive_all  (** All of them. *)
  | Reorder  (** Any of them. *)

val lengths : policy:policy -> t -> IS.t
(** [lengths ~policy r] is the lengths the file may have after a crash. *)

val check : policy:policy -> t -> string -> string option
(** [check ~policy r img] is [None] if [img] is a content the file may hold
    after a crash, or the reason it may not. *)
