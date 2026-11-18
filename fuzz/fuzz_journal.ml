open Alcobar
open Journal

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
  | Pop
  | Reset

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
   clear, a barrier, a sync, a settle, popping the committed entries or a
   reset. Places in program order go up to
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
  | _ -> ( match b1 mod 8 with 0 -> Pop | 1 -> Reset | _ -> Settle)

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

(* A place of program order no entry holds stands for a write. A length it
   sets survives a cut iff the cut keeps every entry before it, and the
   setuid clear it made, logged at the next place: ext4 journals the length
   as it writes the pages back, after them. *)
(* A truncation there survives iff the cut keeps an entry after it, or a
   commit [floor] stands for reached it: ext4 journals it in the syscall's
   own handle, which a later entry's transaction carries. *)
let check_lost t all ~floor ~kept =
  let lost = first_lost t ~kept in
  let upto = last_kept t ~kept in
  let last = List.fold_left (fun m e -> max m e.seq) floor all in
  for w = 0 to last + 1 do
    if not (List.exists (fun e -> e.seq = w) all) then begin
      let carried =
        w <= floor
        || List.exists (fun e -> (e.committed || kept e) && e.seq > w) all
      in
      if w <= upto <> carried then
        failf "a truncation at %d %s, the last kept place %d" w
          (if carried then "is lost" else "survives")
          upto;
      let needs =
        List.for_all
          (fun e ->
            e.committed || kept e
            || not (e.seq < w || (e.clear && e.seq = w + 1)))
          all
      in
      if w < lost <> needs then
        failf "a length set at %d %s, the first lost at %d" w
          (if needs then "is lost" else "survives")
          lost
    end
  done

(* Cuts that keep a prefix of the pending entries, as a drawn cut does, and
   one that keeps every other entry. *)
let check_cuts t all ~floor =
  let p = pending all in
  List.iteri
    (fun n _ ->
      let kept e =
        List.exists (fun x -> x == e) (List.filteri (fun i _ -> i < n) p)
      in
      check_lost t all ~floor ~kept)
    (() :: List.map ignore p);
  check_lost t all ~floor ~kept:(fun e -> e.seq mod 2 = 0)

(* Whether a barrier or a sync reached [e], given the nodes [held]. *)
let reached held e =
  let is_held i = Hashtbl.mem held i in
  (match e.attrs_ino with Some i -> is_held i | None -> true)
  && ((e.barred <> [] && List.exists is_held e.dirs)
     || (e.synced && Option.is_some e.attrs_ino))

let commit held newly =
  let s = List.map (fun e -> e.seq) newly in
  check_eq ~pp:pp_seqs s (List.sort_uniq Int.compare s);
  (match List.rev newly with
  | [] -> fail "an empty commit"
  | last :: _ ->
      if not ((last.barred <> [] || last.synced) && reached held last) then
        failf "commit up to %d, which no barrier or sync reached" last.seq);
  List.iter
    (fun e -> List.iter (fun n -> Hashtbl.replace held n ()) e.nodes)
    newly

let check_add t all seq (s : Shape.t) =
  let e =
    entry ~seq ~dirs:(i64 s.dirs)
      ?attrs_ino:(Option.map Int64.of_int s.attrs)
      ~nodes:(i64 s.nodes) ~parents:(i64 s.parents) ~ancestors:(i64 s.ancestors)
      ~moved:(i64 s.moved) ~removed:(i64 s.removed) ~clear:s.clear ()
  in
  add t e;
  all := !all @ [ e ];
  List.iter
    (fun n ->
      match made t n with
      | Some m when m == e -> ()
      | _ -> failf "%d made %Ld" seq n)
    e.nodes

let check_bar t all i upto =
  let i = Int64.of_int i in
  let bound = Option.value ~default:max_int upto in
  let due e =
    (not e.committed) && has i e.dirs && e.seq <= bound && not (has i e.barred)
  in
  let expected = List.length (List.filter due !all) in
  check_eq ~pp:Fmt.int (bar ?upto t i) expected;
  List.iter (fun e -> if due e then failf "%d not barred on %Ld" e.seq i) !all

let check_sync t all i upto =
  let i = Int64.of_int i in
  let due e =
    (not e.committed) && e.attrs_ino = Some i && e.seq <= upto && not e.synced
  in
  let expected = List.length (List.filter due !all) in
  check_eq ~pp:Fmt.int (sync_attrs ~upto t i) expected;
  List.iter (fun e -> if due e then failf "%d not synced" e.seq) !all

let check_settle t all held =
  settle t ~reached:(reached held) ~commit:(commit held);
  List.iter
    (fun e ->
      if candidate e && reached held e then
        failf "%d is reached and left pending" e.seq)
    !all

let check_pop t all floor =
  let rec go () =
    match pop_committed t with
    | None -> ()
    | Some e ->
        (match !all with
        | e' :: rest when e' == e && e.committed ->
            all := rest;
            floor := max !floor e.seq
        | _ -> failf "popped %d, not the first committed entry" e.seq);
        go ()
  in
  go ();
  match !all with
  | e :: _ when e.committed -> failf "%d committed, not popped" e.seq
  | _ -> ()

let check_reset t all held floor seq =
  reset ~upto:seq t;
  List.iter
    (fun e -> List.iter (fun n -> Hashtbl.replace held n ()) e.nodes)
    !all;
  all := [];
  floor := max !floor seq

let test_invariants a b =
  let ops = ops (a ^ b) in
  let t = v () in
  let all = ref [] in
  (* The last place a commit of entries no longer in [t] reached. *)
  let floor = ref min_int in
  let held = Hashtbl.create 8 in
  Hashtbl.replace held 0L ();
  Hashtbl.replace held 1L ();
  List.iteri
    (fun seq op ->
      let was = List.filter (fun e -> e.committed) !all in
      (match op with
      | Add s -> check_add t all seq s
      | Bar (i, upto) -> check_bar t all i upto
      | Sync (i, upto) -> check_sync t all i upto
      | Settle -> check_settle t all held
      | Pop -> check_pop t all floor
      | Reset -> check_reset t all held floor seq);
      List.iter
        (fun e -> if not e.committed then failf "%d uncommitted" e.seq)
        was;
      check_indexes t !all;
      check_cuts t !all ~floor:!floor)
    ops

let suite =
  ( "journal",
    [
      test_case
        "the journal's indexes, commits, lost and kept places hold their \
         invariants"
        [ bytes; bytes ] test_invariants;
    ] )
