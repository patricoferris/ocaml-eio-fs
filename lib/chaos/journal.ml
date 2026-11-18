(* The metadata journal of a power-cut controller. *)

module Ids = Map.Make (Int)

type 'a entry = {
  seq : int;
  dirs : int64 list;
  mutable barred : int64 list;
  attrs_ino : int64 option;
  nodes : int64 list;
  parents : int64 list;
  ancestors : int64 list;
  moved : int64 list;
  removed : int64 list;
  mutable synced : bool;
  clear : bool;
  mutable committed : bool;
  op : 'a;
}

let entry ~seq ?(dirs = []) ?attrs_ino ?(nodes = []) ?(parents = [])
    ?(ancestors = []) ?(moved = []) ?(removed = []) ?(clear = false) op =
  {
    seq;
    dirs;
    barred = [];
    attrs_ino;
    nodes;
    parents;
    ancestors;
    moved;
    removed;
    synced = false;
    clear;
    committed = false;
    op;
  }

(* Every entry since the last whole snapshot, committed or not, by seq; and
   the indexes of the pending ones. *)
type 'a t = {
  mutable entries : 'a entry Ids.t;
  mutable pool : 'a entry Ids.t;
      (* The pending entries a barrier or a sync has reached. *)
  by_dir : (int64, 'a entry Ids.t) Hashtbl.t;
      (* By the directories whose barrier commits them. *)
  made : (int64, 'a entry) Hashtbl.t;
      (* The entry that made each node, committed or not, until the snapshot
         holds the node. *)
  by_parent : (int64, 'a entry Ids.t) Hashtbl.t;
  by_moved : (int64, 'a entry Ids.t) Hashtbl.t;
  mutable clears : 'a entry Ids.t;
  by_attrs : (int64, 'a entry Ids.t) Hashtbl.t;
  mutable carried : int;
      (* The last place a commit reached, the entries it took out of
         [entries] included. *)
}

let v () =
  {
    entries = Ids.empty;
    pool = Ids.empty;
    by_dir = Hashtbl.create 16;
    made = Hashtbl.create 16;
    by_parent = Hashtbl.create 16;
    by_moved = Hashtbl.create 16;
    clears = Ids.empty;
    by_attrs = Hashtbl.create 16;
    carried = min_int;
  }

let reset ~upto t =
  t.carried <- max t.carried upto;
  t.entries <- Ids.empty;
  t.pool <- Ids.empty;
  Hashtbl.reset t.by_dir;
  Hashtbl.reset t.made;
  Hashtbl.reset t.by_parent;
  Hashtbl.reset t.by_moved;
  t.clears <- Ids.empty;
  Hashtbl.reset t.by_attrs

let add_to tbl key e =
  let m = Option.value ~default:Ids.empty (Hashtbl.find_opt tbl key) in
  Hashtbl.replace tbl key (Ids.add e.seq e m)

let remove_from tbl key e =
  match Hashtbl.find_opt tbl key with
  | None -> ()
  | Some m ->
      let m = Ids.remove e.seq m in
      if Ids.is_empty m then Hashtbl.remove tbl key
      else Hashtbl.replace tbl key m

let add t e =
  t.entries <- Ids.add e.seq e t.entries;
  List.iter (fun d -> add_to t.by_dir d e) e.dirs;
  List.iter (fun n -> Hashtbl.replace t.made n e) e.nodes;
  List.iter (fun p -> add_to t.by_parent p e) e.parents;
  List.iter (fun n -> add_to t.by_moved n e) e.moved;
  Option.iter (fun a -> add_to t.by_attrs a e) e.attrs_ino;
  if e.clear then t.clears <- Ids.add e.seq e t.clears

let fold f t acc = Ids.fold (fun _ e acc -> f e acc) t.entries acc
let made t ino = Hashtbl.find_opt t.made ino
let forget t ino = Hashtbl.remove t.made ino

let pop_committed t =
  match Ids.min_binding_opt t.entries with
  | Some (s, e) when e.committed ->
      t.entries <- Ids.remove s t.entries;
      Some e
  | Some _ | None -> None

let candidate e =
  (not e.committed)
  && (e.barred <> [] || (e.synced && Option.is_some e.attrs_ino))

let refresh t e =
  t.pool <-
    (if candidate e then Ids.add e.seq e t.pool else Ids.remove e.seq t.pool)

(* A committed entry leaves the indexes of pending ones. *)
let mark_committed t e =
  e.committed <- true;
  t.carried <- max t.carried e.seq;
  List.iter (fun d -> remove_from t.by_dir d e) e.dirs;
  List.iter (fun p -> remove_from t.by_parent p e) e.parents;
  List.iter (fun n -> remove_from t.by_moved n e) e.moved;
  Option.iter (fun a -> remove_from t.by_attrs a e) e.attrs_ino;
  if e.clear then t.clears <- Ids.remove e.seq t.clears;
  t.pool <- Ids.remove e.seq t.pool

(* A directory renamed before [e] changed it is no dependency: its inode is
   durable under one name or the other. *)
let deps t e =
  match e.attrs_ino with
  | Some _ -> []
  | None ->
      let earlier tbl key =
        match Hashtbl.find_opt tbl key with
        | None -> []
        | Some m ->
            Ids.fold (fun s e' acc -> if s < e.seq then e' :: acc else acc) m []
      in
      List.concat_map (earlier t.by_parent) e.parents
      @ List.concat_map (earlier t.by_moved) e.ancestors
      @ List.concat_map (earlier t.by_parent) e.removed

let closure t e =
  let seen = Hashtbl.create 8 in
  let rec go acc e =
    if Hashtbl.mem seen e.seq then acc
    else begin
      Hashtbl.replace seen e.seq ();
      List.fold_left go (e :: acc) (deps t e)
    end
  in
  go [] e

let clears_before t seq =
  Ids.fold (fun s e acc -> if s < seq then e :: acc else acc) t.clears []

(* A clear counts from the write that made it, logged just before it. *)
let first_lost t ~kept =
  Ids.fold
    (fun s e acc ->
      if e.committed || kept e then acc
      else min acc (if e.clear then s - 1 else s))
    t.entries max_int

let last_kept t ~kept =
  Ids.fold
    (fun s e acc -> if e.committed || kept e then max acc s else acc)
    t.entries t.carried

let bar ?(upto = max_int) t ino =
  match Hashtbl.find_opt t.by_dir ino with
  | None -> 0
  | Some m ->
      Ids.fold
        (fun _ e n ->
          if e.seq <= upto && not (List.exists (Int64.equal ino) e.barred) then begin
            e.barred <- ino :: e.barred;
            refresh t e;
            n + 1
          end
          else n)
        m 0

let has_attrs t = Hashtbl.length t.by_attrs > 0

let sync_attrs ~upto t ino =
  match Hashtbl.find_opt t.by_attrs ino with
  | None -> 0
  | Some m ->
      Ids.fold
        (fun _ e n ->
          if (not e.synced) && e.seq <= upto then begin
            e.synced <- true;
            refresh t e;
            n + 1
          end
          else n)
        m 0

(* A commit carries the setuid clears before it, which ext4 journals in the
   running transaction. *)
let rec settle t ~reached ~commit =
  match
    Ids.fold (fun _ e acc -> if reached e then e :: acc else acc) t.pool []
  with
  | [] -> ()
  | ready ->
      let newly = List.concat_map (closure t) ready in
      let last = List.fold_left (fun m e -> max m e.seq) 0 newly in
      let newly =
        List.sort_uniq
          (fun a b -> Int.compare a.seq b.seq)
          (clears_before t last @ newly)
      in
      List.iter (mark_committed t) newly;
      commit newly;
      settle t ~reached ~commit
