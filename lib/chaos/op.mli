(** The operations a wrapper sees. {!Eio_fs_chaos.Op} documents them. *)

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
