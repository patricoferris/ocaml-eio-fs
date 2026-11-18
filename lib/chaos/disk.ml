open! Hermetic

(* {1 Faults} *)

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

let choice_name = function
  | Fault f -> Fault.key f
  | Lasting (l, _) -> lasting_name l

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

(* How long an operation that meets no fault takes on the run's clock: a
   sync waits for the disk, another operation for the CPU. Other fibers run
   meanwhile. *)
let sync_time = Mtime.Span.(1 * ms)
let op_time = Mtime.Span.(1 * us)

(* {1 The part} *)

type t = {
  mutable os : Eio_chaos.os;
  mutable chaos : Eio_chaos.t;
  mutable sw : Eio.Switch.t option;
  mutable base : Eio.Fs.dir_ty Eio.Path.t option; (* the run's filesystem *)
  mutable ctl : Crash.t option; (* what a cut keeps, in this phase *)
  mutable power : bool; (* the phase may meet a cut *)
  mutable keep : Eio_chaos.keep option;
  mutable ack : Ack.t option;
  (* The last sync or open, and the controller's stamp before its effect,
     which follows with no other operation in between. *)
  mutable sync : (int * int) option;
  mutable syncing : int option; (* the sync whose time the controller takes *)
  mutable seqs : (int * int) list;
      (* for each operation, latest first, the last change before it *)
  mutable acked_at : int list; (* latest first *)
}

let v () =
  {
    os = `Linux;
    chaos = Eio_chaos.none;
    sw = None;
    base = None;
    ctl = None;
    power = false;
    keep = None;
    ack = None;
    sync = None;
    syncing = None;
    seqs = [];
    acked_at = [];
  }

let name = "fs"
let noun = "filesystem operation"
let indeterminate _ = false

let pp_op ppf (op : Eio_chaos.Op.t) =
  match op.kind with
  | "read" | "write" ->
      Fmt.pf ppf "%s %s@%d+%d" op.kind op.target op.offset op.length
  | "truncate" -> Fmt.pf ppf "truncate %s to %d" op.target op.offset
  | "change" when op.detail <> "" ->
      Fmt.pf ppf "rename %s to %s" op.target op.detail
  | k -> Fmt.pf ppf "%s %s" k op.target

let generic (op : Op.t) : Eio_chaos.Op.t =
  {
    device = name;
    kind = Op.kind_name op.kind;
    target = op.path;
    instance = "";
    offset = op.offset;
    length = op.length;
    detail = Option.value ~default:"" op.dest;
    structure =
      (match op.kind with
      | `Read | `Write -> Thin
      | `Sync -> After "write"
      | _ -> Every);
    changes =
      (match op.kind with
      | `Write | `Truncate | `Change -> Always
      | `Open_write | `Sync -> Maybe
      | _ -> Never);
  }

let base t = Option.get t.base
let crashed t = match t.ctl with Some c -> Crash.is_crashed c | None -> false

(* The size of the file [op] writes, before it. *)
let size t (op : Op.t) =
  let rel =
    String.concat "/"
      (List.filter (( <> ) "") (String.split_on_char '/' op.path))
  in
  match Eio.Path.stat ~follow:true Eio.Path.(base t / rel) with
  | s -> Optint.Int63.to_int s.size
  | exception Eio.Io _ -> 0

(* A sync or an open that changed what a cut leaves. *)
let synced t =
  match (t.sync, t.ctl) with
  | Some (i, s), Some ctl ->
      t.sync <- None;
      if Crash.stamp ctl <> s then Eio_chaos.changed t.chaos i
  | _ -> t.sync <- None

(* The operation numbered [i] about to have its effect. *)
let landing t i ~tracked =
  match t.ctl with
  | Some ctl when t.power ->
      synced t;
      if tracked then t.sync <- Some (i, Crash.stamp ctl);
      t.seqs <-
        (match t.seqs with
        | (s, _) :: rest when s = Crash.seq ctl -> (s, i) :: rest
        | l -> (Crash.seq ctl, i) :: l)
  | _ -> ()

let hook t ~space (op : Op.t) =
  if t.power && not (crashed t) then synced t;
  let gop = generic op in
  let once, lasting, durable =
    if t.power then ([], [], durable ~size:(size t op) op)
    else (once ~space op, lasting ~space op, [])
  in
  let f =
    Eio_chaos.decide t.chaos gop ~name:choice_name ~pp:pp_choice ~lasting
      ~durable ~alone:(alone op) once
  in
  let i = Eio_chaos.index t.chaos in
  Option.iter (fun a -> Ack.touch a op) t.ack;
  let tracked = op.kind = `Sync || op.kind = `Open_write in
  match f with
  | Some c ->
      landing t i ~tracked;
      Some (act c)
  | None ->
      if t.power && op.kind = `Sync then begin
        t.syncing <- Some i;
        landing t i ~tracked:false
      end
      else begin
        Eio_chaos.spend t.chaos (if op.kind = `Sync then sync_time else op_time);
        landing t i ~tracked
      end;
      None

(* The sync whose time the controller takes, in the middle of it. *)
let pause t () =
  let i = t.syncing in
  t.syncing <- None;
  Eio_chaos.spend t.chaos sync_time;
  Option.iter (fun i -> landing t i ~tracked:true) i

(* The directory [d] opened as eio_posix opens a process's cwd: confined to
   itself, so an absolute path or link and a [..] above it are refused. It
   is opened beneath the fault wrapper, so the open is no operation of the
   run. *)
let confined t (d : _ Eio.Path.t) =
  (Eio.Path.open_dir ~sw:(Option.get t.sw) d :> Eio.Fs.dir_ty Eio.Path.t)

(* [fs] and [cwd] share one numbering of the operations. *)
let capability t (fs : _ Eio.Path.t) =
  let sched = Wrapper.schedule (hook t) in
  let w d = (Wrapper.wrap sched d :> Eio.Fs.dir_ty Eio.Path.t) in
  Eio_chaos.Fs (w fs, w (confined t fs))

let start t ~os ~sw ~clock ~mono_clock:_ chaos =
  let base = (Memfs.v ~clock ~os () :> Eio.Fs.dir_ty Eio.Path.t) in
  t.os <- os;
  t.chaos <- chaos;
  t.sw <- Some sw;
  t.base <- Some base;
  t.ctl <- None;
  t.power <- false;
  t.keep <- None;
  t.ack <- None;
  t.sync <- None;
  t.syncing <- None;
  t.seqs <- [];
  t.acked_at <- [];
  Eio_chaos.Fs (base, confined t base)

let policy : Eio_chaos.keep -> Crash.policy = function
  | Lose_unsynced -> Drop_unsynced
  | Keep_unsynced -> Survive (fun _ -> true)
  | Choose f -> Choose f

(* A phase on a controller with [keep]'s policy, whose syncs take their time
   in the middle. *)
let tracked t keep =
  let ctl =
    Crash.v ~os:t.os ~policy:(policy keep) ~block_size:512 ~pause:(pause t) ()
  in
  t.ctl <- Some ctl;
  t.power <- true;
  t.sync <- None;
  t.syncing <- None;
  t.seqs <- [];
  (Crash.wrap ctl (base t) :> Eio.Fs.dir_ty Eio.Path.t)

let alive t () = not (crashed t)

let run t ~cut ~faults =
  let base = base t in
  let ack = Ack.v ~at:(fun () -> Eio_chaos.index t.chaos) base in
  t.ack <- Some ack;
  t.keep <- cut;
  let fs =
    match cut with
    | Some keep ->
        let fs = tracked t keep in
        ack.alive <- alive t;
        (* A file's ack changes what a cut must keep, as a value's does. *)
        ack.on_ack <-
          (fun () ->
            let i = Eio_chaos.index t.chaos in
            Eio_chaos.changed t.chaos i;
            t.acked_at <- i :: t.acked_at);
        fs
    | None ->
        (* Pages a failed sync evicts read as the disk holds them, which
           the model of the disk knows. *)
        if List.exists (String.starts_with ~prefix:"evict-") faults then begin
          let ctl = Crash.v ~os:t.os () in
          t.ctl <- Some ctl;
          (Crash.wrap ctl base :> Eio.Fs.dir_ty Eio.Path.t)
        end
        else base
  in
  capability t fs

(* A change, numbered by the operation that made it, as the trace shows
   it. *)
let change_text seqs (u : Crash.unsynced) =
  let n =
    match List.find_opt (fun (s, _) -> s < u.seq) seqs with
    | Some (_, i) -> Fmt.str "#%d " i
    | None -> ""
  in
  match u.kind with
  | Data -> Fmt.str "%swrite %s@%d+%d" n u.path u.file_offset u.length
  | Truncate -> Fmt.str "%struncate %s to %d" n u.path u.file_offset
  | Create -> Fmt.str "%screate %s" n u.path
  | Mkdir -> Fmt.str "%smkdir %s" n u.path
  | Unlink -> Fmt.str "%sunlink %s" n u.path
  | Rmdir -> Fmt.str "%srmdir %s" n u.path
  | Symlink { target } -> Fmt.str "%ssymlink %s -> %s" n u.path target
  | Rename { dest } -> Fmt.str "%srename %s to %s" n u.path dest
  | Chmod { perm } -> Fmt.str "%schmod %s %o" n u.path perm
  | Chown { uid; gid } -> Fmt.str "%schown %s %Ld:%Ld" n u.path uid gid

(* The values or files acknowledged, by the operation each came after. *)
let acked_text = function
  | [] -> "none"
  | ats ->
      let n = List.length ats in
      let shown = List.take 8 ats in
      Fmt.str "%d value%s, after %s%s" n
        (if n = 1 then "" else "s")
        (String.concat ", "
           (List.map (function 0 -> "the start" | i -> Fmt.str "#%d" i) shown))
        (if n > 8 then Fmt.str " and %d more" (n - 8) else "")

let cut t =
  match t.ctl with
  | Some ctl when t.power && not (Crash.is_crashed ctl) ->
      let lost = Crash.unsynced ctl in
      let note =
        if Crash.cached ctl then
          "under macOS an fsync stops in the drive's cache; F_FULLFSYNC \
           (Eio_fs_unix.sync_file, Eio_fs_unix.barrier) goes through it"
        else ""
      in
      Crash.cut ctl;
      let left = Crash.left ctl in
      let changes = String.concat "; " (List.map (change_text t.seqs) lost) in
      let lost =
        match t.keep with
        | Some Lose_unsynced | None -> ("lost", changes)
        | Some Keep_unsynced -> ("lost", "")
        | Some (Choose _) -> ("unsynced", changes)
      in
      let left =
        match t.keep with
        | Some (Lose_unsynced | Keep_unsynced) | None -> ""
        | Some (Choose _) when left = [] ->
            "no file written since its last sync"
        | Some (Choose _) ->
            let n = List.length left in
            (List.take 8 left
            @ if n > 8 then [ Fmt.str "and %d more files" (n - 8) ] else [])
            |> String.concat "\n            "
      in
      [
        ("acked", acked_text (List.rev t.acked_at));
        lost;
        ("left", left);
        ("note", note);
      ]
  | _ -> []

let restart t ~power ~cut =
  Option.iter
    (fun a ->
      Option.iter
        (fun l -> Eio_chaos.lost t.chaos ("an acknowledged file: " ^ l))
        (Ack.lost a))
    t.ack;
  let fs =
    if power then begin
      Option.iter
        (fun (a : Ack.t) ->
          a.on_ack <- ignore;
          a.alive <- alive t)
        t.ack;
      tracked t (match cut with Some k -> k | None -> Eio_chaos.Lose_unsynced)
    end
    else begin
      t.ctl <- None;
      t.power <- false;
      base t
    end
  in
  t.keep <- cut;
  capability t fs

module Part = Eio_chaos.Part (struct
  type nonrec t = t

  let name = name
  let noun = noun
  let start = start
  let run = run
  let cut = cut
  let restart = restart
  let pp_op = pp_op
  let indeterminate = indeterminate
end)

let part () = Part.v (v ())

let acks name (env : Eio_chaos.env) =
  match Part.find env#chaos with
  | Some ({ ack = Some a; _ } : t) -> a
  | Some _ | None ->
      Fmt.invalid_arg "%s: env is not that of a run of Eio_fs_chaos.run" name

(* An acknowledged value changes what a cut must keep, until the cut. *)
let acknowledge (env : Eio_chaos.env) =
  match Part.find env#chaos with
  | Some d when not (crashed d) ->
      let i = Eio_chaos.index d.chaos in
      Eio_chaos.changed d.chaos i;
      d.acked_at <- i :: d.acked_at;
      true
  | Some _ | None -> false
