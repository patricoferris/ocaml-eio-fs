open! Hermetic

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

let kind_name : kind -> string = function
  | `Open_read -> "open"
  | `Open_write -> "open-write"
  | `Stat -> "stat"
  | `Read -> "read"
  | `Read_dir -> "read-dir"
  | `Write -> "write"
  | `Truncate -> "truncate"
  | `Sync -> "sync"
  | `Change -> "change"

let pp ppf t =
  match t.kind with
  | `Read | `Write ->
      Fmt.pf ppf "#%d %s %s@%d+%d" t.index (kind_name t.kind) t.path t.offset
        t.length
  | `Truncate -> Fmt.pf ppf "#%d truncate %s to %d" t.index t.path t.offset
  | `Change when Option.is_some t.dest ->
      Fmt.pf ppf "#%d rename %s to %s" t.index t.path (Option.get t.dest)
  | k -> Fmt.pf ppf "#%d %s %s" t.index (kind_name k) t.path
