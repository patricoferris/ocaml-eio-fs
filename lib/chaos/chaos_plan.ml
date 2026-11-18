open! Hermetic

module Op = struct
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
end

module Fault = struct
  type code = [ `Enoent | `Eacces | `Eperm | `Erofs | `Enospc | `Eio ]

  type t =
    | Fail of code
    | Evict of code
    | Short of int
    | Torn of int
    | Swap_symlink of string

  let code_name : code -> string = function
    | `Enoent -> "ENOENT"
    | `Eacces -> "EACCES"
    | `Eperm -> "EPERM"
    | `Erofs -> "EROFS"
    | `Enospc -> "ENOSPC"
    | `Eio -> "EIO"

  let pp ppf = function
    | Fail c -> Fmt.pf ppf "fails with %s" (code_name c)
    | Evict c -> Fmt.pf ppf "fails with %s, its pages evicted" (code_name c)
    | Short n ->
        let n = max 1 n in
        Fmt.pf ppf "moves %d byte%s" n (if n = 1 then "" else "s")
    | Torn n ->
        Fmt.pf ppf "is torn after %d byte%s" n (if n = 1 then "" else "s")
    | Swap_symlink t -> Fmt.pf ppf "is swapped for a link to %s" t

  let key = function
    | Fail c -> String.lowercase_ascii (code_name c)
    | Evict c -> "evict-" ^ String.lowercase_ascii (code_name c)
    | Short n -> Fmt.str "short%d" n
    | Torn n -> Fmt.str "torn%d" n
    | Swap_symlink t -> "swap:" ^ t
end

type lasting = Read_only | Full | Denied
type choice = Fault of Fault.t | Lasting of lasting * Fault.code

let lasting_name = function
  | Read_only -> "read-only"
  | Full -> "full"
  | Denied -> "denying"

let lasting_text = function
  | Read_only -> "the filesystem is read-only"
  | Full -> "the filesystem is full"
  | Denied -> "the filesystem is denying every access"

let name = function Fault f -> Fault.key f | Lasting (l, _) -> lasting_name l

let pp_choice ppf = function
  | Fault f -> Fault.pp ppf f
  | Lasting (l, _) -> Fmt.string ppf (lasting_text l)

let act = function Fault f -> f | Lasting (_, c) -> Fault.Fail c

(* The lists are constants, but for a write's tear, so a run allocates
   none of them. An [ENOSPC] needs an operation that takes room. *)
let once ~space (op : Op.t) : choice list =
  match op.kind with
  | `Read -> [ Fault (Short 1); Fault (Fail `Eio) ]
  | `Read_dir -> [ Fault (Fail `Eio) ]
  | `Write ->
      [
        Fault (Short 1);
        Fault (Torn (op.length / 2));
        Fault (Fail `Enospc);
        Fault (Fail `Eio);
      ]
  | `Truncate | `Sync -> [ Fault (Fail `Eio) ]
  | `Open_read | `Stat -> [ Fault (Fail `Eacces); Fault (Fail `Eio) ]
  | `Open_write | `Change ->
      if space then
        [ Fault (Fail `Eacces); Fault (Fail `Erofs); Fault (Fail `Enospc) ]
      else [ Fault (Fail `Eacces); Fault (Fail `Erofs) ]

let lasting ~space (op : Op.t) : choice list =
  match op.kind with
  | `Write -> [ Lasting (Read_only, `Erofs); Lasting (Full, `Enospc) ]
  | `Truncate -> [ Lasting (Read_only, `Erofs) ]
  | `Open_write | `Change ->
      if space then
        [
          Lasting (Read_only, `Erofs);
          Lasting (Full, `Enospc);
          Lasting (Denied, `Eacces);
        ]
      else [ Lasting (Read_only, `Erofs); Lasting (Denied, `Eacces) ]
  | `Open_read | `Read_dir | `Stat -> [ Lasting (Denied, `Eacces) ]
  | `Read | `Sync -> []

(* [n] elements of [l], evenly spread: the tear points of a write, which the
   linear tier tries whole and a replay finds again. *)
let spread n l =
  let len = List.length l in
  if len <= n then l
  else
    let a = Array.of_list l in
    List.init n (fun i -> a.(i * len / n))

(* The bytes a write overwrites tear only at the 512-byte sectors a disk
   writes whole. Bytes that extend a file tear anywhere. *)
let tears ~size (op : Op.t) =
  let len = op.length in
  let over = max 0 (min len (size - op.offset)) in
  let sector =
    List.filter
      (fun k -> k > 0 && k < over)
      (List.init ((over / 512) + 2) (fun j -> (512 * j) - (op.offset mod 512)))
  in
  let extend =
    if over < len then
      List.filter
        (fun k -> k > over && k < len)
        [ over + 1; (over + len) / 2; len - 1 ]
    else []
  in
  spread 4 (List.sort_uniq compare (sector @ extend))

let durable ~size (op : Op.t) : choice list =
  match op.kind with
  | `Sync -> [ Fault (Fail `Eio) ]
  | `Write -> List.map (fun n -> Fault (Torn n)) (tears ~size op)
  | _ -> []

let alone (op : Op.t) : choice list =
  match op.kind with `Sync -> [ Fault (Evict `Eio) ] | _ -> []
