open Alcobar
open Chaos_journal

(* Workloads over six inodes, so entries share directories. Inodes [0] and
   [1] are on disk before the journal starts; the others exist once an entry
   that makes them commits. *)
module Shape = struct
  type t = {
    dirs : int list;
    parents : int list;
    nodes : int list;
    ancestors : int list;
    moved : int list;
    removed : int list;
    attrs : int option;
    clear : bool;
  }
end

type op =
  | Add of Shape.t
  | Bar of int * int option
  | Sync of int * int
  | Settle

(* The inodes of the low six bits of [b]. *)
let mask b = List.filter (fun i -> b land (1 lsl i) <> 0) [ 0; 1; 2; 3; 4; 5 ]

(* An inode for three bits [b], or none for two of the eight. *)
let one b = if b land 7 < 6 then [ b land 7 ] else []

let attrs ?(clear = false) i =
  {
    Shape.dirs = (if clear then [] else [ i ]);
    parents = [];
    nodes = [];
    ancestors = [];
    moved = [];
    removed = [];
    attrs = Some i;
    clear;
  }

(* Four bytes of a tape: a change of directory entries, a chmod, a setuid
   clear, a barrier, a sync or a settle. Places in program order go up to
   [32], so barriers and syncs often stop short of the last entry. *)
let op_of b0 b1 b2 b3 =
  match b0 mod 8 with
  | 0 | 1 | 2 ->
      let parents = mask b1 in
      Add
        {
          Shape.dirs = parents;
          parents;
          nodes = one b2;
          removed = one (b2 lsr 3);
          moved = one b3;
          ancestors = mask (b3 lsr 3);
          attrs = None;
          clear = false;
        }
  | 3 -> Add (attrs (b1 mod 6))
  | 4 -> Add (attrs ~clear:true (b1 mod 6))
  | 5 -> Bar (b1 mod 6, if b3 land 1 = 0 then None else Some (b2 mod 32))
  | 6 -> Sync (b1 mod 6, b2 mod 32)
  | _ -> Settle

let ops tape =
  List.init
    (String.length tape / 4)
    (fun i ->
      let b j = Char.code tape.[(i * 4) + j] in
      op_of (b 0) (b 1) (b 2) (b 3))

let i64 = List.map Int64.of_int
let has i l = List.exists (Int64.equal i) l
let meets a b = List.exists (fun x -> has x b) a
let seqs l = List.sort_uniq Int.compare (List.map (fun e -> e.seq) l)
let pp_seqs = Fmt.(Dump.list int)
let pending (all : unit entry list) = List.filter (fun e -> not e.committed) all

(* The dependencies of [e] by their definition, over every entry. *)
let deps_of all (e : unit entry) =
  match e.attrs_ino with
  | Some _ -> []
  | None ->
      List.filter
        (fun e' ->
          e'.seq < e.seq
          && (meets e'.parents e.parents || meets e'.moved e.ancestors
            || meets e'.parents e.removed))
        (pending all)

let candidate (e : unit entry) =
  (not e.committed)
  && (e.barred <> [] || (e.synced && Option.is_some e.attrs_ino))

let check_indexes t all =
  check_eq ~pp:pp_seqs (seqs (fold List.cons t [])) (seqs all);
  check_eq ~pp:Fmt.bool (has_attrs t)
    (List.exists (fun e -> Option.is_some e.attrs_ino) (pending all));
  List.iter
    (fun e ->
      check_eq ~pp:pp_seqs (seqs (deps t e)) (seqs (deps_of all e));
      (* The closure holds [e], is closed under its dependencies, and holds
         nothing else. *)
      let c = closure t e in
      if not (List.memq e c) then failf "closure of %d misses it" e.seq;
      List.iter
        (fun x ->
          if x.seq > e.seq then failf "closure of %d holds %d" e.seq x.seq;
          List.iter
            (fun d ->
              if not (List.memq d c) then
                failf "closure of %d holds %d, not its dependency %d" e.seq
                  x.seq d.seq)
            (deps t x);
          if x != e && not (List.exists (fun y -> List.memq x (deps t y)) c)
          then failf "closure of %d holds %d, needed by none" e.seq x.seq)
        c;
      check_eq ~pp:pp_seqs
        (seqs (clears_before t e.seq))
        (seqs (List.filter (fun c -> c.clear && c.seq < e.seq) (pending all))))
    all;
  (* The committed entries are closed under dependencies, and carry every
     setuid clear before them. *)
  let committed = List.filter (fun e -> e.committed) all in
  List.iter
    (fun e ->
      (match deps_of all e with
      | [] -> ()
      | d :: _ -> failf "%d is committed, its dependency %d is not" e.seq d.seq);
      List.iter
        (fun c ->
          if c.clear && c.seq < e.seq then
            failf "%d is committed, the clear %d before it is not" e.seq c.seq)
        (pending all))
    committed

let test_invariants a b =
  let ops = ops (a ^ b) in
  let t = v () in
  let all = ref [] in
  let held = Hashtbl.create 8 in
  Hashtbl.replace held 0L ();
  Hashtbl.replace held 1L ();
  let is_held i = Hashtbl.mem held i in
  let reached e =
    (match e.attrs_ino with Some i -> is_held i | None -> true)
    && ((e.barred <> [] && List.exists is_held e.dirs)
       || (e.synced && Option.is_some e.attrs_ino))
  in
  let commit newly =
    let s = List.map (fun e -> e.seq) newly in
    check_eq ~pp:pp_seqs s (List.sort_uniq Int.compare s);
    (match List.rev newly with
    | [] -> fail "an empty commit"
    | last :: _ ->
        if not ((last.barred <> [] || last.synced) && reached last) then
          failf "commit up to %d, which no barrier or sync reached" last.seq);
    List.iter
      (fun e -> List.iter (fun n -> Hashtbl.replace held n ()) e.nodes)
      newly
  in
  List.iteri
    (fun seq op ->
      let was = List.filter (fun e -> e.committed) !all in
      (match op with
      | Add s ->
          let e =
            entry ~seq ~dirs:(i64 s.Shape.dirs)
              ?attrs_ino:(Option.map Int64.of_int s.attrs)
              ~nodes:(i64 s.nodes) ~parents:(i64 s.parents)
              ~ancestors:(i64 s.ancestors) ~moved:(i64 s.moved)
              ~removed:(i64 s.removed) ~clear:s.clear ()
          in
          add t e;
          all := !all @ [ e ];
          List.iter
            (fun n ->
              match made t n with
              | Some m when m == e -> ()
              | _ -> failf "%d made %Ld" seq n)
            e.nodes
      | Bar (i, upto) ->
          let i = Int64.of_int i in
          let bound = Option.value ~default:max_int upto in
          let due e =
            (not e.committed) && has i e.dirs && e.seq <= bound
            && not (has i e.barred)
          in
          let expected = List.length (List.filter due !all) in
          check_eq ~pp:Fmt.int (bar ?upto t i) expected;
          List.iter
            (fun e -> if due e then failf "%d not barred on %Ld" e.seq i)
            !all
      | Sync (i, upto) ->
          let i = Int64.of_int i in
          let due e =
            (not e.committed) && e.attrs_ino = Some i && e.seq <= upto
            && not e.synced
          in
          let expected = List.length (List.filter due !all) in
          check_eq ~pp:Fmt.int (sync_attrs ~upto t i) expected;
          List.iter (fun e -> if due e then failf "%d not synced" e.seq) !all
      | Settle ->
          settle t ~reached ~commit;
          List.iter
            (fun e ->
              if candidate e && reached e then
                failf "%d is reached and left pending" e.seq)
            !all);
      List.iter
        (fun e -> if not e.committed then failf "%d uncommitted" e.seq)
        was;
      check_indexes t !all)
    ops

let suite =
  ( "chaos_journal",
    [
      test_case "the journal's indexes and commits hold their invariants"
        [ bytes; bytes ] test_invariants;
    ] )
