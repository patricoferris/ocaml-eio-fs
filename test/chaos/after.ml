(* The design's tests, written with the API they propose. *)

(* (a) An atomic save of a config. *)
let test_a ?replay ~save () =
  Eio_fs.Chaos.Crash.check ?replay
    ~setup:(fun env -> save env#fs "old")
    (fun ~ack env ->
      match save env#fs "new" with () -> ack () | exception Eio.Io _ -> ())
    ~recover:(fun ~acked env ->
      match (App.load_config env#fs, acked) with
      | "new", _ | "old", [] -> ()
      | v, _ -> Fmt.failwith "config is %S" v)

(* A recover that ignores [acked]: it checks that the config is whole, not
   that it is durable, and the check cannot tell it from one whose
   acknowledged values hold after setup. *)
let test_a_weak ~save () =
  Eio_fs.Chaos.Crash.check
    ~setup:(fun env -> save env#fs "old")
    (fun ~ack env ->
      match save env#fs "new" with () -> ack () | exception Eio.Io _ -> ())
    ~recover:(fun ~acked:_ env ->
      match App.load_config env#fs with
      | "new" | "old" -> ()
      | v -> Fmt.failwith "config is %S" v)

(* (b) An append-only log of checksummed records, each longer than two
   sectors, whose sectors a cut may land apart. *)
let records =
  List.init 6 (fun i -> Fmt.str "record-%d:%s" i (String.make 1100 'r'))

let rec is_prefix p l =
  match (p, l) with
  | [], _ -> true
  | x :: p, y :: l -> x = y && is_prefix p l
  | _ -> false

(* Appends [records] to the log in [dir], acking each, until a fault. *)
let log_records ?(append = fun f r -> App.Log.append f r) ~ack dir =
  Eio.Switch.run @@ fun sw ->
  let log = App.Log.open_ ~sw dir in
  List.iter
    (fun r ->
      append log r;
      ack r)
    records

let names l = List.map (fun r -> String.sub r 0 (String.index r ':')) l

let test_b ?budget ?replay ~append ~recover () =
  Eio_fs.Chaos.Crash.check ?budget ?replay
    (fun ~ack env -> try log_records ~append ~ack env#fs with Eio.Io _ -> ())
    ~recover:(fun ~acked env ->
      let found = recover env#fs in
      if not (is_prefix acked found && is_prefix found records) then
        Fmt.failwith "acked %a, recovered %a"
          Fmt.Dump.(list string)
          acked
          Fmt.Dump.(list string)
          found)

(* A failed sync loses writes and moves none: the log read back holds the
   records appended, in order, each once. *)
let read_back dir =
  match App.Log.recover dir with
  | exception Eio.Io _ -> ()
  | found ->
      if not (is_prefix found records) then
        Fmt.failwith "read back %a" Fmt.Dump.(list string) (names found)

(* The log, read back by the program once a fault stops it. *)
let test_b_read_back () =
  Eio_fs.Chaos.check (fun env ->
      try log_records ~ack:ignore env#fs with Eio.Io _ -> read_back env#fs)

(* The log on a controller built by hand that cuts by [Drop_unsynced]: the
   second sync of the log meets [fault], then the cut. It is the records
   acked and those the cut left. *)
let cut_after_sync_fault fault =
  let module Chaos = Eio_fs.Chaos in
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env ->
  let t = Chaos.Crash.v Drop_unsynced in
  let hook (op : Chaos.Op.t) =
    if op.kind = `Sync && op.nth = 2 && Filename.basename op.path = "log" then
      Some fault
    else None
  in
  let fs = Chaos.wrap hook (Chaos.Crash.wrap t env#fs) in
  let acked = ref [] in
  (try log_records ~ack:(fun r -> acked := r :: !acked) fs with Eio.Io _ -> ());
  (List.rev !acked, App.Log.recover (Chaos.Crash.cut t))

let cut_failure acked found =
  Fmt.failwith "acked %a, recovered %a"
    Fmt.Dump.(list string)
    (names acked)
    Fmt.Dump.(list string)
    (names found)

(* The sync evicts its pages: the cut leaves each record once. *)
let test_b_cut_after_evict () =
  let acked, found = cut_after_sync_fault (Eio_fs.Chaos.Fault.Evict `Eio) in
  if not (is_prefix acked found && is_prefix found records) then
    cut_failure acked found

(* The sync fails and the page cache keeps its pages: the cut loses the
   write the sync did not make durable, as every unsynced write. *)
let test_b_cut_after_failure () =
  let acked, found = cut_after_sync_fault (Eio_fs.Chaos.Fault.Fail `Eio) in
  if found <> acked then cut_failure acked found

(* (c) A cache that survives any filesystem error. *)
let compute k = "value of " ^ k

let test_c ?replay ~get () =
  Eio_fs.Chaos.check ?replay (fun env ->
      for _ = 1 to 2 do
        let v = get env#fs "k" ~compute in
        if v <> compute "k" then Fmt.failwith "got %S" v
      done)

(* A key-value store with a write-ahead log and compaction. *)
let puts = List.init 12 (fun i -> (Fmt.str "k%d" (i mod 5), Fmt.str "v%d" i))
let keys = List.sort_uniq compare (List.map fst puts)

(* Every acked put is visible, or the next put of its key. *)
let recover_kv ?bug ~puts ~acked dir =
  Eio.Switch.run @@ fun sw ->
  let db = Kv.open_ ?bug ~sw dir in
  let next = List.nth_opt puts (List.length acked) in
  List.iter
    (fun k ->
      let ok =
        List.assoc_opt k (List.rev acked)
        :: (match next with Some (k', v) when k' = k -> [ Some v ] | _ -> [])
      in
      if not (List.mem (Kv.find db k) ok) then Fmt.failwith "%s lost" k)
    keys

let test_kv ?bug () =
  Eio_fs.Chaos.Crash.check
    (fun ~ack env ->
      try
        Eio.Switch.run @@ fun sw ->
        let db = Kv.open_ ?bug ~sw env#fs in
        List.iter
          (fun (k, v) ->
            Kv.put db k v;
            ack (k, v))
          puts
      with Eio.Io _ -> ())
    ~recover:(fun ~acked env -> recover_kv ?bug ~puts ~acked env#fs)

(* A save that retries a failed write once, in place. *)
let value = "a value of some length"

let test_retry ?bug () =
  Eio_fs.Chaos.check (fun env ->
      match Retry.save ?bug env#fs value with
      | exception Eio.Io _ -> ()
      | () -> (
          match Retry.load env#fs with
          | v -> if v <> value then Fmt.failwith "saved %S" v
          | exception Eio.Io _ -> ()))
