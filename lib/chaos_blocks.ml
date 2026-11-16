(* A file's bytes under writeback, block by block. *)

let copy_cs c =
  let n = Cstruct.length c in
  let b = Cstruct.create n in
  Cstruct.blit c 0 b 0 n;
  b

(* [Write] carries a copy of the committed bytes so a crash can replay the
   survivors onto the durable baseline. *)
type change = Write of { offset : int; data : Cstruct.t } | Resize of int

(* A file's bytes while changes replay over them: the first [len] bytes of
   [buf], which grows by doubling, so a run of appends costs linear time. *)
type image = { mutable buf : Cstruct.t; mutable len : int }

(* The bytes replays have copied into images, which a test bounds. *)
let copied = Atomic.make 0

let image_of c =
  ignore (Atomic.fetch_and_add copied (Cstruct.length c) : int);
  { buf = copy_cs c; len = Cstruct.length c }

let bytes_of img = Cstruct.sub img.buf 0 img.len

(* Room for [n] bytes, those past [len] zero. *)
let extend img n =
  if n > Cstruct.length img.buf then begin
    let b = Cstruct.create (max n (2 * Cstruct.length img.buf)) in
    ignore (Atomic.fetch_and_add copied img.len : int);
    Cstruct.blit img.buf 0 b 0 img.len;
    img.buf <- b
  end
  else if n > img.len then
    Cstruct.memset (Cstruct.sub img.buf img.len (n - img.len)) 0

let apply img = function
  | Write { offset; data } ->
      let n = offset + Cstruct.length data in
      extend img n;
      Cstruct.blit data 0 img.buf offset (Cstruct.length data);
      img.len <- max img.len n
  | Resize n ->
      extend img n;
      img.len <- n

(* [durable] with [changes] replayed over it, in order. *)
let replay_changes durable changes =
  let img = image_of durable in
  List.iter (apply img) changes;
  bytes_of img

(* The draws a crash state is built from: a seeded generator, or a function
   that enumerates them. *)
type draw = { int : int -> int; bool : unit -> bool }

let draw_of_state rng =
  { int = Random.State.int rng; bool = (fun () -> Random.State.bool rng) }

let draw_of_fun f = { int = f; bool = (fun () -> f 2 = 1) }

(* ext4's block, which it allocates whole, and the disk's sector, which
   it writes whole. *)
let page = 4096
let sector = 512

(* [held] with [change] made durable: a write adds what it covers, a
   shrink frees the pages past it. *)
let hold held = function
  | Write { offset; data } ->
      Extents.add offset (offset + Cstruct.length data) held
  | Resize m -> Extents.cut ((m + page - 1) / page * page) held

(* Whether the disk holds the page of the byte range [\[a, b)]. *)
let held_at held a b =
  Extents.overlaps (a / page * page) ((b + page - 1) / page * page) held

(* The bytes [change] dirties in a file of [prev] bytes: a write those it
   covers and, past the end, the hole it leaves; a shrink the block it cuts
   mid-way, and with [zero_page] the rest of the page it cuts, which ext4
   zeroes; a growth the bytes it adds. *)
let dirtied ?(zero_page = false) ~bs prev = function
  | Write { offset; data } -> (min prev offset, offset + Cstruct.length data)
  | Resize m when m < prev ->
      let block = if m mod bs = 0 then m else m + 1 in
      if zero_page && m mod page <> 0 then
        (m, max block (min prev (((m / page) + 1) * page)))
      else (m, block)
  | Resize m -> (prev, m)

(* [durable] with [changes] replayed over it, where a block they dirty in
   the page cache, [cache] with the same changes, is written whole from
   it: the page cache still holds the bytes a failed sync lost. And the page
   cache after them. *)
let replay_cached ~bs durable ~cache changes =
  match cache with
  | None ->
      let c = replay_changes durable (List.map fst changes) in
      (c, c)
  | Some cache ->
      let plain = copy_cs (replay_changes durable (List.map fst changes)) in
      let live =
        replay_changes cache
          (List.filter_map
             (fun (c, cached) -> if cached then None else Some c)
             changes)
      in
      let len = Cstruct.length plain and live_len = Cstruct.length live in
      let size prev = function
        | Write { offset; data } -> max prev (offset + Cstruct.length data)
        | Resize m -> m
      in
      let _ =
        List.fold_left
          (fun (pprev, cprev) (change, cached) ->
            (* A kept truncation carries its length only. *)
            let lo, hi = if cached then (0, 0) else dirtied ~bs cprev change in
            if hi > lo then
              for b = lo / bs to (hi - 1) / bs do
                let off = b * bs in
                let n = max 0 (min bs (min len live_len - off)) in
                if n > 0 then Cstruct.blit live off plain off n
              done;
            (size pprev change, if cached then cprev else size cprev change))
          (Cstruct.length durable, Cstruct.length cache)
          changes
      in
      (plain, live)

(* The blocks [changes] dirty, or only their writes when [writes], replayed
   from a file of [len] bytes. *)
let dirty_blocks ?(writes = false) ~bs len changes =
  let _, acc =
    List.fold_left
      (fun (prev, acc) change ->
        let acc =
          match change with
          | Resize _ when writes -> acc
          | Write _ | Resize _ ->
              let lo, hi = dirtied ~bs prev change in
              if hi > lo then
                List.init
                  (((hi - 1) / bs) - (lo / bs) + 1)
                  (fun i -> (lo / bs) + i)
                @ acc
              else acc
        in
        ( (match change with
          | Write { offset; data } -> max prev (offset + Cstruct.length data)
          | Resize m -> m),
          acc ))
      (len, []) changes
  in
  List.sort_uniq Int.compare acc

(* The byte that stands for stale data where the disk held nothing the
   model knows of: a digit, so that a reader that trusts what it finds
   parses a number. *)
let stale = '7'

(* The most distinct freed pages a stale sector draws from: past it, the
   draws would rarely try the sector's other versions. *)
let max_stale = 8

(* Writeback in blocks of [bs] bytes. For each block a pending change
   dirtied, a crash keeps the block as it was durable or as it stood after
   any one of those changes, drawn from [rng]; the length is one a change
   left, or a block boundary a write grew the file across, as writeback
   lands a long append block by block. A write past the end of the file
   dirties the hole it leaves as well. A truncation changes the length in
   one step and dirties only the block it cuts, so a block kept from before
   it keeps its old bytes. A block is never torn, and a later change never
   lands in a block without the earlier ones.

   With [bytes], blocks are sectors and the length is one a change left
   or the end of a page a write put bytes in, as ext4 extends its on-disk
   size as writeback lands. A page the disk did not hold that a write put
   bytes in is written whole, and each of its sectors may also hold stale
   data: ext4 may commit the page's allocation before its bytes land.
   [old p] is what the freed blocks the page may have been given held, at
   page [p] of its file, and where each was; one of them is drawn. [describe] is told the length and, run by run, what each
   block holds. *)
let writeback ?(over = []) ?(len_upto = max_int) ?(bytes = false)
    ?(held = Extents.empty) ?(old = fun _ -> []) ?(describe = fun _ _ -> ())
    ~rng ~bs ~cache durable changes =
  let cached = Array.of_list (List.map snd changes) in
  let changes = Array.of_list (List.map fst changes) in
  let n = Array.length changes in
  (* Dirt is counted in the page cache, which a failed sync may have left
     apart from the durable bytes. *)
  let cprev = ref (Cstruct.length (Option.value ~default:durable cache)) in
  let sizes = Array.make (n + 1) (Cstruct.length durable) in
  let versions = Hashtbl.create 16 in
  let boundaries = ref [] in
  (* The pages a pending write put bytes in. *)
  let filled = Hashtbl.create 8 in
  for k = 0 to n - 1 do
    let prev = sizes.(k) in
    (match changes.(k) with
    | Write { offset; data } ->
        let size = max prev (offset + Cstruct.length data) in
        sizes.(k + 1) <- size;
        if k < len_upto then begin
          let step = if bytes then page else bs in
          (* ext4 extends its on-disk size, up to the file's, as it writes
             back a page a write put bytes in, this one or an earlier one,
             not one of the hole before it. *)
          for b = (prev / step) + 1 to (size - 1) / step do
            if
              (not bytes) || b - 1 >= offset / step || Hashtbl.mem filled (b - 1)
            then boundaries := (b * step, k) :: !boundaries
          done
        end;
        if bytes && Cstruct.length data > 0 then
          for p = offset / page to (offset + Cstruct.length data - 1) / page do
            Hashtbl.replace filled p ()
          done
    | Resize m -> sizes.(k + 1) <- m);
    let lo, hi =
      if cached.(k) then (0, 0)
      else dirtied ~zero_page:bytes ~bs !cprev changes.(k)
    in
    (if not cached.(k) then
       cprev :=
         match changes.(k) with
         | Write { offset; data } -> max !cprev (offset + Cstruct.length data)
         | Resize m -> m);
    (* ext4 writes a page it allocates whole: with [bytes], a write into
       a page the disk does not hold dirties the rest of the page within
       the length too. *)
    let pages =
      match changes.(k) with
      | Write { offset; data } when bytes && not cached.(k) ->
          let last = (offset + Cstruct.length data - 1) / page in
          List.filter_map
            (fun pg ->
              let a = pg * page in
              let b = min (a + page) sizes.(k + 1) in
              if b > a && not (held_at held a b) then Some (a, b) else None)
            (List.init
               (max 0 (last - (offset / page) + 1))
               (fun i -> (offset / page) + i))
      | _ -> []
    in
    let dirty = Hashtbl.create 8 in
    List.iter
      (fun (lo, hi) ->
        if hi > lo then
          for b = lo / bs to (hi - 1) / bs do
            Hashtbl.replace dirty b ()
          done)
      ((lo, hi) :: pages);
    Hashtbl.iter
      (fun b () ->
        let vs = Option.value ~default:[] (Hashtbl.find_opt versions b) in
        Hashtbl.replace versions b ((k + 1) :: vs))
      dirty
  done;
  (* Each dirty block's version, in block order: [0] is the durable one, [k]
     the one after change [k]. *)
  let blocks =
    Hashtbl.fold (fun b _ acc -> b :: acc) versions [] |> List.sort Int.compare
  in
  (* Whether a write put bytes in block [b], in a page the disk did not
     hold: the page is allocated by the crash, and may hold stale data. *)
  let may_be_stale b vs =
    bytes
    && (not (held_at held (b * bs) ((b + 1) * bs)))
    && List.exists
         (fun v ->
           match changes.(v - 1) with
           | Write { offset; data } when not cached.(v - 1) ->
               let lo = offset / page * page
               and hi =
                 (offset + Cstruct.length data + page - 1) / page * page
               in
               b * bs >= lo && b * bs < hi
           | _ -> false)
         vs
  in
  (* The length, and how many changes its metadata holds: a length a
     change left holds those before it, a page boundary those up to the
     write that grew the file across it. *)
  let size, committed =
    let boundaries = List.sort_uniq compare !boundaries in
    if boundaries = [] || rng.bool () then (
      (* Two counts that leave the same length and the same shrinks
         pending leave the same state: draw one of each. *)
      let shrinks = Array.make (n + 1) 0 in
      for k = n - 1 downto 0 do
        shrinks.(k) <-
          (shrinks.(k + 1) + match changes.(k) with Resize _ -> 1 | _ -> 0)
      done;
      let js =
        List.init (min n len_upto + 1) Fun.id
        |> List.fold_left
             (fun acc j ->
               if
                 List.exists
                   (fun i -> sizes.(i) = sizes.(j) && shrinks.(i) = shrinks.(j))
                   acc
               then acc
               else j :: acc)
             []
        |> List.rev
      in
      let j = List.nth js (rng.int (List.length js)) in
      (sizes.(j), j))
    else
      let b, k = List.nth boundaries (rng.int (List.length boundaries)) in
      (b, k + 1)
  in
  (* A shrink the length does not hold has freed no block yet: the pages
     past it keep their blocks, which no later change reaches. *)
  let upto b =
    let at = b * bs in
    let limit = ref max_int in
    for k = n - 1 downto committed do
      match changes.(k) with
      | Resize m
        when m < sizes.(k)
             && at >= (m + page - 1) / page * page
             && at < sizes.(k) ->
          limit := k
      | _ -> ()
    done;
    !limit
  in
  let chosen = Hashtbl.create 16 and stale_blocks = Hashtbl.create 4 in
  List.iter
    (fun b ->
      let limit = upto b in
      let ks =
        List.filter (fun v -> v <= limit) (List.rev (Hashtbl.find versions b))
      in
      let stale =
        if may_be_stale b ks then
          List.init
            (max 1 (List.length (old (b * bs / page))))
            (fun i -> -1 - i)
        else []
      in
      let vs = (0 :: ks) @ stale in
      let v = List.nth vs (rng.int (List.length vs)) in
      if v > 0 then Hashtbl.replace chosen b v
      else if v < 0 then Hashtbl.replace stale_blocks b (-1 - v))
    blocks;
  (* Replay once, taking each chosen block as it stood after its change. *)
  let at = Hashtbl.create 16 in
  Hashtbl.iter
    (fun b v ->
      Hashtbl.replace at v
        (b :: Option.value ~default:[] (Hashtbl.find_opt at v)))
    chosen;
  let taken = Hashtbl.create 16 in
  let block_of buf b =
    let off = b * bs in
    let len = max 0 (min bs (Cstruct.length buf - off)) in
    if len = 0 then Cstruct.empty else copy_cs (Cstruct.sub buf off len)
  in
  (* The page cache: what a failed sync lost is still there. *)
  let live = image_of (Option.value ~default:durable cache) in
  Array.iteri
    (fun k change ->
      if not cached.(k) then apply live change;
      List.iter
        (fun b -> Hashtbl.replace taken b (block_of (bytes_of live) b))
        (Option.value ~default:[] (Hashtbl.find_opt at (k + 1))))
    changes;
  let image = Cstruct.create size in
  if size > 0 then
    for b = 0 to (size - 1) / bs do
      let src =
        match Hashtbl.find_opt taken b with
        | Some blk -> blk
        | None -> (
            match List.assoc_opt b over with
            | Some blk -> blk
            | None -> block_of durable b)
      in
      let len = min (Cstruct.length src) (size - (b * bs)) in
      if len > 0 then Cstruct.blit src 0 image (b * bs) len
    done;
  Hashtbl.iter
    (fun b i ->
      let len = min bs (size - (b * bs)) in
      if len > 0 then
        let dst = Cstruct.sub image (b * bs) len in
        match List.nth_opt (old (b * bs / page)) i with
        | None -> Cstruct.memset dst (Char.code stale)
        | Some (pg, _) ->
            (* Past the old file's end its block held zeros. *)
            Cstruct.memset dst 0;
            let off = b * bs mod page in
            let n = max 0 (min len (Cstruct.length pg - off)) in
            if n > 0 then Cstruct.blit pg off dst 0 n)
    stale_blocks;
  (let source b =
     match Hashtbl.find_opt stale_blocks b with
     | Some i -> `Stale (Option.map snd (List.nth_opt (old (b * bs / page)) i))
     | None -> (
         match Hashtbl.find_opt chosen b with
         | Some v -> `Change (v - 1)
         | None when List.mem_assoc b over -> `Over
         | None -> `Durable)
   in
   let runs = ref [] in
   if size > 0 then
     for b = (size - 1) / bs downto 0 do
       let len = min bs (size - (b * bs)) in
       let src = source b in
       match !runs with
       | (off, n, s) :: rest when s = src && off = (b + 1) * bs ->
           runs := (b * bs, n + len, s) :: rest
       | _ -> runs := (b * bs, len, src) :: !runs
     done;
   describe size !runs);
  image

(* A disk writes a 512-byte sector whole, so the overwrites of one sector
   land together: where a surviving write overwrote bytes the file held, its
   sector holds what the page cache held after it, the earlier overwrites
   of that sector included. *)
let sectors_together ~cache durable mine image =
  let base = Option.value ~default:durable cache in
  let entries = Array.of_list mine in
  (* The sectors each write overwrites, by its place in [mine]. *)
  let _, over =
    Array.fold_left
      (fun (size, acc) (change, cached, _) ->
        match change with
        | Write { offset; data } ->
            let hi = min size (offset + Cstruct.length data) in
            let ss =
              if hi > offset then
                List.init
                  (((hi - 1) / sector) - (offset / sector) + 1)
                  (fun i -> (offset / sector) + i)
              else []
            in
            (max size (offset + Cstruct.length data), ss :: acc)
        | Resize m -> ((if cached then size else m), [] :: acc))
      (Cstruct.length base, [])
      entries
  in
  let over = Array.of_list (List.rev over) in
  let kept_at = Array.map (fun (_, _, kept) -> kept) entries in
  let patch = Hashtbl.create 4 in
  Array.iteri
    (fun k ss ->
      if kept_at.(k) then
        List.iter
          (fun s ->
            let earlier_lost = ref false in
            for j = 0 to k - 1 do
              if (not kept_at.(j)) && List.mem s over.(j) then
                earlier_lost := true
            done;
            if !earlier_lost then Hashtbl.replace patch s k)
          ss)
    over;
  if Hashtbl.length patch = 0 then image
  else begin
    let image = copy_cs image in
    Hashtbl.iter
      (fun s k ->
        let live =
          replay_changes base
            (List.filteri (fun j _ -> j <= k) mine
            |> List.filter_map (fun (change, cached, _) ->
                if cached then None else Some change))
        in
        let off = s * sector in
        let n =
          max 0
            (min sector
               (min (Cstruct.length image) (Cstruct.length live) - off))
        in
        if n > 0 then Cstruct.blit live off image off n)
      patch;
    image
  end
