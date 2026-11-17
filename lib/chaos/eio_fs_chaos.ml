open! Hermetic
module M = Chaos_model
module Plan = Chaos_plan

type env = Eio_chaos.env

module Op = Plan.Op
module Fault = Plan.Fault

(* {1 Wrapper} *)

let wrap f base =
  let hook ~space:_ op = f op in
  (M.wrap (M.schedule hook) base :> Eio.Fs.dir_ty Eio.Path.t)

(* How long an operation that meets no fault takes on the run's clock: a
   sync waits for the disk, another operation for the CPU. Other fibers run
   meanwhile. *)
let sync_time = Mtime.Span.(1 * ms)
let op_time = Mtime.Span.(1 * us)

(* {1 Acknowledged files} *)

(* The files a run acknowledged, each with what it held then, read beneath
   the fault wrapper. A file the run writes, truncates, renames or removes
   since is no longer compared. *)
module Ack = struct
  type entry = { path : string; held : string; at : int }

  type t = {
    base : Eio.Fs.dir_ty Eio.Path.t;
    at : unit -> int; (* the last operation's index *)
    mutable alive : unit -> bool; (* false once the power is cut *)
    mutable on_ack : unit -> unit;
    mutable live : entry list; (* latest first *)
    mutable paths : string list; (* latest first, each once *)
  }

  let v ~at base =
    {
      base :> Eio.Fs.dir_ty Eio.Path.t;
      at;
      alive = (fun () -> true);
      on_ack = ignore;
      live = [];
      paths = [];
    }

  (* A path as the root names it, with no [""], ["."] or [".."]. *)
  let norm path =
    let rec go acc = function
      | [] -> List.rev acc
      | ("" | ".") :: l -> go acc l
      | ".." :: l -> go (match acc with [] -> [] | _ :: a -> a) l
      | c :: l -> go (c :: acc) l
    in
    String.concat "/" (go [] (String.split_on_char '/' path))

  let read t p =
    let f = Eio.Path.(t.base / p) in
    match Eio.Path.kind ~follow:false f with
    | `Regular_file -> Some (Eio.Path.load f)
    | _ -> None
    | exception Eio.Io _ -> None

  let ack name t ((_, path) : _ Eio.Path.t) =
    if t.alive () then
      let p = norm path in
      match read t p with
      | None ->
          Fmt.invalid_arg "%s: %S is not a regular file of the run's filesystem"
            name path
      | Some held ->
          t.live <-
            { path = p; held; at = t.at () }
            :: List.filter (fun e -> e.path <> p) t.live;
          if not (List.mem p t.paths) then t.paths <- p :: t.paths;
          t.on_ack ()

  let acked t = List.rev_map (fun p -> "/" ^ p) t.paths

  let under ~dir p =
    dir = "" || p = dir || String.starts_with ~prefix:(dir ^ "/") p

  let touches (op : Op.t) p =
    match op.kind with
    | `Open_write | `Write | `Truncate -> norm op.path = p
    | `Change ->
        under ~dir:(norm op.path) p
        || Option.fold ~none:false
             ~some:(fun d -> under ~dir:(norm d) p)
             op.dest
    | `Open_read | `Stat | `Read | `Read_dir | `Sync -> false

  let touch t op =
    if t.live <> [] then
      t.live <- List.filter (fun e -> not (touches op e.path)) t.live

  let quote s =
    let n = String.length s in
    if n <= 40 then Fmt.str "%S" s
    else Fmt.str "%S... (%d bytes)" (String.sub s 0 32) n

  (* The first file acknowledged and untouched since that no longer holds
     what it held then. *)
  let lost t =
    List.find_map
      (fun e ->
        match read t e.path with
        | Some s when s = e.held -> None
        | now ->
            Fmt.kstr Option.some "/%s held %s when acknowledged %s, and now %s"
              e.path (quote e.held)
              (if e.at = 0 then "at the start" else Fmt.str "after #%d" e.at)
              (match now with
              | None -> "is missing"
              | Some s -> "holds " ^ quote s))
      (List.rev t.live)
end

(* {1 Power} *)

module Power = struct
  type change = M.Crash.unsynced

  let pp_change = M.Crash.pp_unsynced

  type policy = Lose_unsynced | Keep of (change -> bool)
  type t = M.Crash.t

  (* A controller built by hand may wrap a directory other processes write,
     so its cut keeps their changes. *)
  let v ?os = function
    | Lose_unsynced -> M.Crash.v ?os ~policy:Drop_unsynced ~outside:Kept ()
    | Keep p -> M.Crash.v ?os ~policy:(Survive p) ~outside:Kept ()

  let wrap t base = (M.Crash.wrap t base :> Eio.Fs.dir_ty Eio.Path.t)
  let unsynced = M.Crash.unsynced
  let cut = M.Crash.post_crash
end

(* {1 The disk} *)

module Disk = struct
  type t = {
    mutable os : Eio_chaos.os;
    mutable chaos : Eio_chaos.t;
    mutable sw : Eio.Switch.t option;
    mutable base : Eio.Fs.dir_ty Eio.Path.t option; (* the run's filesystem *)
    mutable ctl : M.Crash.t option; (* what a cut keeps, in this phase *)
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
      offset = op.file_offset;
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

  let crashed t =
    match t.ctl with Some c -> M.Crash.is_crashed c | None -> false

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
        if M.Crash.stamp ctl <> s then Eio_chaos.changed t.chaos i
    | _ -> t.sync <- None

  (* The operation numbered [i] about to have its effect. *)
  let landing t i ~tracked =
    match t.ctl with
    | Some ctl when t.power ->
        synced t;
        if tracked then t.sync <- Some (i, M.Crash.stamp ctl);
        t.seqs <-
          (match t.seqs with
          | (s, _) :: rest when s = M.Crash.seq ctl -> (s, i) :: rest
          | l -> (M.Crash.seq ctl, i) :: l)
    | _ -> ()

  let hook t ~space (op : Op.t) =
    if t.power && not (crashed t) then synced t;
    let gop = generic op in
    let once, lasting, durable =
      if t.power then ([], [], Plan.durable ~size:(size t op) op)
      else (Plan.once ~space op, Plan.lasting ~space op, [])
    in
    let f =
      Eio_chaos.decide t.chaos gop ~name:Plan.name ~pp:Plan.pp_choice ~lasting
        ~durable ~alone:(Plan.alone op) once
    in
    let i = Eio_chaos.index t.chaos in
    Option.iter (fun a -> Ack.touch a op) t.ack;
    let tracked = op.kind = `Sync || op.kind = `Open_write in
    match f with
    | Some c ->
        landing t i ~tracked;
        Some (Plan.act c)
    | None ->
        if t.power && op.kind = `Sync then begin
          t.syncing <- Some i;
          landing t i ~tracked:false
        end
        else begin
          Eio_chaos.spend t.chaos
            (if op.kind = `Sync then sync_time else op_time);
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
    let sched = M.schedule (hook t) in
    let w d = (M.wrap sched d :> Eio.Fs.dir_ty Eio.Path.t) in
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

  let policy : Eio_chaos.keep -> M.Crash.policy = function
    | Lose_unsynced -> Drop_unsynced
    | Keep_unsynced -> Survive (fun _ -> true)
    | Choose f -> Choose f

  (* A phase on a controller with [keep]'s policy, whose syncs take their time
     in the middle. *)
  let tracked t keep =
    let ctl =
      M.Crash.v ~os:t.os ~policy:(policy keep) ~block_size:512 ~pause:(pause t)
        ()
    in
    t.ctl <- Some ctl;
    t.power <- true;
    t.sync <- None;
    t.syncing <- None;
    t.seqs <- [];
    (M.Crash.wrap ctl (base t) :> Eio.Fs.dir_ty Eio.Path.t)

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
            let ctl = M.Crash.v ~os:t.os () in
            t.ctl <- Some ctl;
            (M.Crash.wrap ctl base :> Eio.Fs.dir_ty Eio.Path.t)
          end
          else base
    in
    capability t fs

  (* A change, numbered by the operation that made it, as the trace shows
     it. *)
  let change_text seqs (u : M.Crash.unsynced) =
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
             (List.map
                (function 0 -> "the start" | i -> Fmt.str "#%d" i)
                shown))
          (if n > 8 then Fmt.str " and %d more" (n - 8) else "")

  let cut t =
    match t.ctl with
    | Some ctl when t.power && not (M.Crash.is_crashed ctl) ->
        let lost = M.Crash.unsynced ctl in
        M.Crash.crash ctl;
        let left = M.Crash.left ctl in
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
        [ ("acked", acked_text (List.rev t.acked_at)); lost; ("left", left) ]
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
        tracked t
          (match cut with Some k -> k | None -> Eio_chaos.Lose_unsynced)
      end
      else begin
        t.ctl <- None;
        t.power <- false;
        base t
      end
    in
    t.keep <- cut;
    capability t fs
end

module Disk_part = Eio_chaos.Part (Disk)

let disk () = Disk_part.v (Disk.v ())

(* {1 The check} *)

let name = "Eio_fs_chaos.run"

let run ?os ?budget ?seed ?replay ?setup ?inspect app =
  Eio_chaos.Private.run ~name ?os ?budget ?seed ?replay ?setup ?inspect
    [ disk () ]
    app

let get name (env : env) =
  match Disk_part.find env#chaos with
  | Some ({ ack = Some a; _ } : Disk.t) -> a
  | Some _ | None ->
      Fmt.invalid_arg "%s: env is not that of a run of a check" name

let ack env file =
  let name = "Eio_fs_chaos.ack" in
  Ack.ack name (get name env) file

let acked env = Ack.acked (get "Eio_fs_chaos.acked" env)

let device ?os ?user dir =
  let run f =
    let d = Disk.v () in
    Eio_chaos.Private.within ?os [ Eio_chaos.quiet (Disk_part.v d) ] f
  in
  Device.check ?os ?user ~run dir

module Private = struct
  let check_faults ?budget ?seed ?replay ?os ?setup run =
    Eio_chaos.Private.run ~name ~passes:[ Errors ] ?budget ?seed ?replay ?os
      ?setup
      [ disk () ]
      run

  (* An acknowledged value changes what a cut must keep, until the cut. *)
  let acknowledge (env : env) =
    match Disk_part.find env#chaos with
    | Some d when not (Disk.crashed d) ->
        let i = Eio_chaos.index d.chaos in
        Eio_chaos.changed d.chaos i;
        d.acked_at <- i :: d.acked_at;
        true
    | Some _ | None -> false

  let check_cuts ?budget ?seed ?replay ?os ?setup run ~recover =
    let acked = ref [] in
    let app env =
      if Eio_chaos.Private.restarted env then
        recover ~acked:(List.rev !acked) env
      else begin
        acked := [];
        run ~ack:(fun v -> if acknowledge env then acked := v :: !acked) env
      end
    in
    Eio_chaos.Private.run ~name ~passes:[ Cuts ] ?budget ?seed ?replay ?os
      ?setup
      [ disk () ]
      app

  let scenarios = Device.scenarios
  let eio_posix_differs = Device.eio_posix_differs

  module Crash = M.Crash
  module Law = M.Law
  module Internal = M.Internal
end
