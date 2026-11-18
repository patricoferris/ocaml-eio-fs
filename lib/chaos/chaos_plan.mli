(** The operations of the fault wrapper, and the faults the disk proposes at
    each, which [Eio_chaos] chooses from. *)

(** The operations a wrapper sees. *)
module Op : sig
  type kind =
    [ `Open_read
    | `Open_write
    | `Stat
    | `Read
    | `Read_dir
    | `Write
    | `Truncate
    | `Sync
    | `Change ]

  type t = {
    index : int;
    kind : kind;
    path : string;
    nth : int;
    rank : int;
    offset : int;
    length : int;
    dest : string option;
  }

  val kind_name : kind -> string
  (** [kind_name k] names [k] in reports and keys. *)

  val pp : t Fmt.t
  (** [pp] formats an operation. *)
end

(** The faults a wrapper injects. *)
module Fault : sig
  type code = [ `Enoent | `Eacces | `Eperm | `Erofs | `Enospc | `Eio ]

  type t =
    | Fail of code
    | Evict of code
    | Short of int
    | Torn of int
    | Swap_symlink of string

  val pp : t Fmt.t
  (** [pp] formats a fault. *)

  val key : t -> string
  (** [key f] names [f] in a replay key: ["eio"], ["torn3"]. *)
end

(** The type for the conditions that last from an operation on. *)
type lasting = Read_only | Full | Denied

(** The type for what the disk proposes at an operation. *)
type choice =
  | Fault of Fault.t
  | Lasting of lasting * Fault.code
      (** A condition, and the error it gives the operation. *)

val name : choice -> string
(** [name c] names [c] in a replay key: ["eio"], ["torn3"], ["read-only"]. *)

val pp_choice : choice Fmt.t
(** [pp_choice] says what a choice does, or what holds under a condition:
    ["fails with EIO"], ["the filesystem is full"]. *)

val act : choice -> Fault.t
(** [act c] is the fault the operation meets. *)

val once : space:bool -> Op.t -> choice list
(** [once ~space op] is each fault a run tries alone at [op], with each error
    Linux gives there, [ENOSPC] only if [space], where [op] takes room. *)

val lasting : space:bool -> Op.t -> choice list
(** [lasting ~space op] is each condition under which [op] fails: read-only,
    full and denying every access, each where Linux answers it. *)

val durable : size:int -> Op.t -> choice list
(** [durable ~size op] is each fault a run tries at [op] before a power cut: a
    failed sync, and a write torn where a disk may tear it, given the file's
    [size] before it: at the 512-byte sectors of the bytes it overwrites, and
    anywhere in those it adds. *)

val alone : Op.t -> choice list
(** [alone op] is a failed sync that evicts its pages, which the linear tier
    alone tries. *)
