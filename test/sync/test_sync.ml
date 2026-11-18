(* Tests for the portable barrier: dispatch to the backend's registered
   operation, the POSIX route for a backend that registers nothing, the loud
   failure when the barrier cannot be issued, and the durable-save protocol.
   The full-strength POSIX facade on top is covered by test/unix. *)

let ( / ) = Eio.Path.( / )

let contains ~affix s =
  let n = String.length affix and m = String.length s in
  let rec at i = i + n <= m && (String.sub s i n = affix || at (i + 1)) in
  at 0

(* A sparse publication takes the bounded publisher's options and its ceiling,
   and its extents as offsets with their bytes. *)
let _ :
    ?barrier:Eio_fs.Sync.Barrier.t ->
    ?perm:Eio.File.Unix_perm.t ->
    ?exclusive:Eio_fs.Sync.Rename.t ->
    ?tmp:'a Eio.Path.t ->
    max_bytes:int64 ->
    size_bytes:int64 ->
    'a Eio.Path.t ->
    (int64 * Cstruct.t) list ->
    unit =
  Eio_fs.Sync.publish_sparse

(* The in-memory backend registers its barrier (a no-op: volatile storage has
   nothing to sync), so the portable entry points work unchanged over it. *)
let test_registered_backend () =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs_mem.run @@ fun env ->
  Eio.Path.sync_dir env#fs;
  Eio_fs.Sync.publish ~max_bytes:3L (env#fs / "cfg") (fun f ->
      Eio.Flow.copy_string "one" f);
  Alcotest.(check string) "published" "one" (Eio.Path.load (env#fs / "cfg"));
  Eio_fs.Sync.publish ~max_bytes:3L (env#fs / "cfg") (fun f ->
      Eio.Flow.copy_string "two" f);
  Alcotest.(check string) "replaced" "two" (Eio.Path.load (env#fs / "cfg"));
  Alcotest.(check bool)
    "temporary cleaned" true
    (Eio.Path.kind ~follow:false (env#fs / "cfg.tmp") = `Not_found)

(* A plain POSIX directory needs no registration and no wrapper: the
   portable barrier is the backend's own [Eio.Path.sync_dir]. *)
let test_posix_needs_no_registration () =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let dir = Filename.temp_file "eio-fs-sync-posix" "" in
  Sys.remove dir;
  Eio.Path.mkdir ~perm:0o755 (fs / dir);
  Fun.protect ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true (fs / dir))
  @@ fun () ->
  Eio.Path.sync_dir (fs / dir);
  Eio_fs.Sync.publish ~max_bytes:1L
    (fs / dir / "cfg")
    (fun f -> Eio.Flow.copy_string "x" f);
  Alcotest.(check string) "published" "x" (Eio.Path.load (fs / dir / "cfg"))

(* The published file carries exactly [perm]. A create's mode is
   narrowed by the process umask (open(2): "the mode of the created file is
   (mode & ~umask)"), so a mode the caller named survives only if publish
   restates it on the temporary before the rename. The umask here clears the
   group and other read bits a 0o644 names, so a publish that leaves the
   create's mode alone reads 0o600 back. *)
let test_publish_honours_perm_under_umask () =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let dir = Filename.temp_file "eio-fs-sync-perm" "" in
  Sys.remove dir;
  Eio.Path.mkdir ~perm:0o755 (fs / dir);
  let previous = Unix.umask 0o077 in
  Fun.protect ~finally:(fun () ->
      ignore (Unix.umask previous : int);
      Eio.Path.rmtree ~missing_ok:true (fs / dir))
  @@ fun () ->
  Eio_fs.Sync.publish ~perm:0o644 ~max_bytes:1L
    (fs / dir / "cfg")
    (fun f -> Eio.Flow.copy_string "x" f);
  let stat = Eio.Path.stat ~follow:false (fs / dir / "cfg") in
  Alcotest.(check int) "published mode is perm" 0o644 stat.perm

(* A barrier on a missing directory raises, never a silent no-op. *)
let test_unissuable_barrier_raises () =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  match Eio.Path.sync_dir (fs / "/no-such-dir-for-eio-fs-sync") with
  | () -> Alcotest.fail "sync_dir accepted a missing directory"
  | exception Eio.Io (Eio.Fs.E (Not_found _), _) -> ()

(* A failed write cleans its temporary up and the failure propagates. *)
let test_failed_save_cleans_temporary () =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs_mem.run @@ fun env ->
  (match
     Eio_fs.Sync.publish ~max_bytes:1L
       (env#fs / "missing-dir" / "f")
       (fun f -> Eio.Flow.copy_string "x" f)
   with
  | () -> Alcotest.fail "publish wrote into a missing directory"
  | exception Eio.Io _ -> ());
  Alcotest.(check bool)
    "no temporary left behind" true
    (Eio.Path.kind ~follow:false (env#fs / "missing-dir") = `Not_found)

(* publish takes a caller-chosen temporary, possibly in another directory on
   the same filesystem, so a watched target directory never sees temporary
   churn. The temporary is consumed by the rename. *)
let test_publish_with_remote_tmp () =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs_mem.run @@ fun env ->
  Eio.Path.mkdir ~perm:0o755 (env#fs / "tmp");
  Eio.Path.mkdir ~perm:0o755 (env#fs / "live");
  Eio_fs.Sync.publish ~max_bytes:7L
    ~tmp:(env#fs / "tmp" / "stage-1")
    (env#fs / "live" / "cfg")
    (fun f -> Eio.Flow.copy_string "payload" f);
  Alcotest.(check string)
    "published" "payload"
    (Eio.Path.load (env#fs / "live" / "cfg"));
  Alcotest.(check bool)
    "temporary consumed" true
    (Eio.Path.kind ~follow:false (env#fs / "tmp" / "stage-1") = `Not_found)

let test_publish_with_stream () =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs_mem.run @@ fun env ->
  let result =
    Eio_fs.Sync.publish ~max_bytes:7L ~tmp:(env#fs / "stream.tmp")
      (env#fs / "stream") (fun file ->
        Eio.Flow.copy_string "one" file;
        Eio.Flow.copy_string "-two" file;
        42)
  in
  Alcotest.(check int) "callback result" 42 result;
  Alcotest.(check string)
    "streamed payload" "one-two"
    (Eio.Path.load (env#fs / "stream"));
  Alcotest.(check bool)
    "temporary consumed" true
    (Eio.Path.kind ~follow:false (env#fs / "stream.tmp") = `Not_found)

let test_publish_with_failure_preserves_target () =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs_mem.run @@ fun env ->
  Eio.Path.save ~create:(`Or_truncate 0o644) (env#fs / "live") "old";
  (match
     Eio_fs.Sync.publish ~max_bytes:7L ~tmp:(env#fs / "live.tmp")
       (env#fs / "live") (fun file ->
         Eio.Flow.copy_string "partial" file;
         raise Exit)
   with
  | _ -> Alcotest.fail "failing producer unexpectedly published"
  | exception Exit -> ());
  Alcotest.(check string)
    "old target preserved" "old"
    (Eio.Path.load (env#fs / "live"));
  Alcotest.(check bool)
    "failed temporary removed" true
    (Eio.Path.kind ~follow:false (env#fs / "live.tmp") = `Not_found)

let test_publish_rejects_target_as_temporary () =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs_mem.run @@ fun env ->
  let live = env#fs / "live" in
  Eio.Path.save ~create:(`Or_truncate 0o644) live "old";
  (match
     Eio_fs.Sync.publish ~max_bytes:3L ~tmp:live live
       (Eio.Flow.copy_string "new")
   with
  | () -> Alcotest.fail "target accepted as its own temporary"
  | exception Invalid_argument _ -> ());
  Alcotest.(check string) "target unchanged" "old" (Eio.Path.load live)

(* Two independently wrapped capabilities can name the same path without being
   physically equal. The post-open identity check must still reject that alias,
   and exclusive creation makes the absent-target case recoverable. *)
let test_publish_rejects_aliased_target () =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs_mem.run @@ fun env ->
  Eio.Path.with_subtree env#fs @@ fun left ->
  Eio.Path.with_subtree env#fs @@ fun right ->
  (match
     Eio_fs.Sync.publish ~max_bytes:3L ~tmp:(left / "same") (right / "same")
       (Eio.Flow.copy_string "new")
   with
  | () -> Alcotest.fail "aliased target accepted as its own temporary"
  | exception Invalid_argument _ -> ());
  Alcotest.(check bool)
    "partial target removed" true
    (Eio.Path.kind ~follow:false (env#fs / "same") = `Not_found)

let test_publish_does_not_remove_unowned_temporary () =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs_mem.run @@ fun env ->
  let tmp = env#fs / "foreign.tmp" in
  Eio.Path.save ~create:(`Or_truncate 0o644) tmp "foreign";
  (match
     Eio_fs.Sync.publish ~max_bytes:3L ~tmp (env#fs / "live")
       (Eio.Flow.copy_string "new")
   with
  | () -> Alcotest.fail "exclusive publish replaced a foreign temporary"
  | exception Eio.Io _ -> ());
  Alcotest.(check string)
    "foreign temporary preserved" "foreign" (Eio.Path.load tmp)

let test_publish_enforces_byte_limit () =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs_mem.run @@ fun env ->
  let live = env#fs / "live" in
  Eio.Path.save ~create:(`Or_truncate 0o644) live "old";
  (match
     Eio_fs.Sync.publish ~max_bytes:4L live (fun file ->
         Eio.Flow.copy_string "123" file;
         Eio.Flow.copy_string "45" file)
   with
  | () -> Alcotest.fail "publication exceeded its byte limit"
  | exception Eio_fs.Sync.Limit_exceeded { max_bytes; attempted } ->
      Alcotest.(check int64) "limit" 4L max_bytes;
      Alcotest.(check int64) "attempted" 5L attempted);
  Alcotest.(check string) "old target preserved" "old" (Eio.Path.load live);
  Alcotest.(check bool)
    "oversized temporary removed" true
    (Eio.Path.kind ~follow:false (env#fs / "live.tmp") = `Not_found)

(* Under the crash model, a default publish survives [Drop_unsynced], and one
   under [Barrier.none] loses its rename. *)
let test_publish_durability_flag () =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs_mem.run @@ fun env ->
  let module Crash = Eio_fs_chaos.Private.Crash in
  let ctl = Crash.v ~policy:Crash.Drop_unsynced () in
  let fs = Crash.wrap ctl env#fs in
  Eio_fs.Sync.publish ~max_bytes:1L ~tmp:(fs / "safe-tmp") (fs / "safe")
    (fun f -> Eio.Flow.copy_string "s" f);
  Eio_fs.Sync.publish ~barrier:Eio_fs.Sync.Barrier.none ~max_bytes:1L
    ~tmp:(fs / "fast-tmp") (fs / "fast") (fun f -> Eio.Flow.copy_string "f" f);
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "durable publish survives" "s"
    (Eio.Path.load Eio.Path.(post / "safe"));
  Alcotest.(check bool)
    "non-durable publish lost" true
    (Eio.Path.kind ~follow:false Eio.Path.(post / "fast") = `Not_found)

(* What the registration is for. Under the crash model a file's writes stay
   un-synced until the model sees a barrier on that file, and [Drop_unsynced]
   loses every un-synced write. Bytes barriered with the portable barrier must
   survive the cut; had the barrier answered anywhere but through the
   registration the model would have recorded none and the bytes would be
   gone. *)
let test_sync_file_reaches_the_crash_model () =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs_mem.run @@ fun env ->
  let module Crash = Eio_fs_chaos.Private.Crash in
  let ctl = Crash.v ~policy:Crash.Drop_unsynced () in
  let fs = Crash.wrap ctl env#fs in
  Eio.Switch.run (fun sw ->
      let file =
        Eio.Path.open_out ~sw ~create:(`If_missing 0o644) (fs / "image")
      in
      Eio.Flow.copy_string "payload" file;
      Eio.File.sync file);
  (* The name the bytes live under is the metadata journal's business, as
     fsync(2) divides them, so the entry takes the directory barrier. *)
  Eio.Path.sync_dir fs;
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "the barriered bytes survived the cut" "payload"
    (Eio.Path.load Eio.Path.(post / "image"))

(* An exclusive publication follows renameat2's RENAME_NOREPLACE contract:
   an existing name is never overwritten, including a dangling symlink. *)
let test_exclusive_publication () =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs_mem.run @@ fun env ->
  let path = env#fs / "state" in
  let publish path text =
    Eio_fs.Sync.publish ~max_bytes:3L ~exclusive:Eio_fs.Sync.Rename.capability
      path
      (Eio.Flow.copy_string text)
  in
  publish path "old";
  (match publish path "new" with
  | () -> Alcotest.fail "exclusive publication replaced a file"
  | exception Eio.Io (Eio.Fs.E (Already_exists _), _) -> ());
  Alcotest.(check string) "old contents retained" "old" (Eio.Path.load path);
  let link = env#fs / "link" in
  Eio.Path.symlink ~link_to:"missing" link;
  (match publish link "new" with
  | () -> Alcotest.fail "exclusive publication replaced a dangling link"
  | exception Eio.Io (Eio.Fs.E (Already_exists _), _) -> ());
  Alcotest.(check string) "link retained" "missing" (Eio.Path.read_link link);
  Alcotest.(check (list string))
    "temporary files removed" [ "link"; "state" ]
    (List.sort String.compare (Eio.Path.read_dir env#fs))

let test_exclusive_publication_survives_crash () =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs_mem.run @@ fun env ->
  let module Crash = Eio_fs_chaos.Private.Crash in
  let ctl = Crash.v () in
  let fs = Crash.wrap ctl env#fs in
  Eio_fs.Sync.publish ~max_bytes:7L ~exclusive:Eio_fs.Sync.Rename.capability
    (fs / "state")
    (Eio.Flow.copy_string "payload");
  Crash.cut ctl;
  Alcotest.(check string)
    "published data survives its barriers" "payload"
    (Eio.Path.load (Crash.post_crash ctl / "state"))

(* [save] publishes a string: the target holds it whole, replaced on a
   second save, and an empty string is a file of no bytes. *)
let test_save () =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs_mem.run @@ fun env ->
  let p = env#fs / "state.json" in
  Eio_fs.Sync.save p "{\"a\":1}";
  Alcotest.(check string) "saved" "{\"a\":1}" (Eio.Path.load p);
  Eio_fs.Sync.save ~barrier:Eio_fs.Sync.Barrier.none p "{}";
  Alcotest.(check string) "replaced" "{}" (Eio.Path.load p);
  Eio_fs.Sync.save p "";
  Alcotest.(check string) "empty" "" (Eio.Path.load p)

(* Windows keeps no Unix mode: a file's reads 666, and eio_windows refuses a
   chmod below a directory it opened. A save with any other mode is
   published without one, there and under a fault. *)
let test_save_on_windows () =
  Eio_mock.Backend.run (fun () ->
      let root = Eio_fs_mem.v ~os:`Windows () in
      Eio.Path.mkdir ~perm:0o755 (root / "d");
      Eio.Switch.run @@ fun sw ->
      let d = Eio.Path.open_dir ~sw (root / "d") in
      Eio_fs.Sync.save ~perm:0o644 (d / "f") "x";
      Alcotest.(check string) "saved" "x" (Eio.Path.load (d / "f")));
  Eio_fs_chaos.run ~os:`Windows (fun env ->
      Eio_fs.Sync.save ~perm:0o644 (Eio.Stdenv.cwd env / "f") "x")

(* Every durable entry point issues the barriers it is given, and none of
   them under Barrier.none. *)
let test_barrier_is_used () =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs_mem.run @@ fun env ->
  let files = ref 0 and dirs = ref 0 in
  let module Counting = Eio_fs.Sync.Barrier.Make (struct
    type t = unit

    let sync_file () _ = incr files
    let sync_dir () _ = incr dirs
  end) in
  let barrier = Counting.v () in
  let issued name ~files:f ~dirs:d fn =
    files := 0;
    dirs := 0;
    fn ();
    Alcotest.(check (pair int int)) name (f, d) (!files, !dirs)
  in
  let fs = env#fs in
  issued "publish" ~files:1 ~dirs:1 (fun () ->
      Eio_fs.Sync.publish ~barrier ~max_bytes:1L (fs / "p")
        (Eio.Flow.copy_string "x"));
  issued "save" ~files:1 ~dirs:1 (fun () ->
      Eio_fs.Sync.save ~barrier (fs / "s") "x");
  issued "save with no barrier" ~files:0 ~dirs:0 (fun () ->
      Eio_fs.Sync.save ~barrier:Eio_fs.Sync.Barrier.none (fs / "s") "y");
  issued "append_lines" ~files:1 ~dirs:1 (fun () ->
      Eio_fs.Sync.append_lines ~barrier (fs / "l") [ "x" ]);
  issued "publish_generation" ~files:1 ~dirs:3 (fun () ->
      Eio_fs.Sync.publish_generation ~barrier (fs / "g") (fun dir ->
          Eio.Path.save ~create:(`Exclusive 0o644) (dir / "f") "x"));
  issued "publish_link" ~files:0 ~dirs:1 (fun () ->
      match Eio_fs.Sync.publish_link ~barrier ~target:"target" (fs / "k") with
      | Ok () -> ()
      | Error _ -> Alcotest.fail "publish_link refused a new link")

(* Appending lines *)

let with_mem f =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs_mem.run @@ fun env -> f (env#fs / "log.jsonl")

let test_append_creates () =
  with_mem @@ fun p ->
  Eio_fs.Sync.append_lines p [ {|{"a":1}|}; {|{"b":2}|} ];
  Alcotest.(check string) "created" "{\"a\":1}\n{\"b\":2}\n" (Eio.Path.load p);
  Eio_fs.Sync.append_lines ~barrier:Eio_fs.Sync.Barrier.none p [ {|{"c":3}|} ];
  Alcotest.(check string)
    "appended" "{\"a\":1}\n{\"b\":2}\n{\"c\":3}\n" (Eio.Path.load p)

(* A crash part way through an append leaves a last line with no LF. The
   next line starts on a line of its own, so it is not read as the torn
   line's continuation, and the torn bytes stay as they were. *)
let test_append_repairs_a_torn_line () =
  with_mem @@ fun p ->
  Eio.Path.save ~create:(`Exclusive 0o644) p "{\"a\":1}\n{\"b\":";
  Eio_fs.Sync.append_lines p [ {|{"c":3}|} ];
  Alcotest.(check string)
    "torn line ended" "{\"a\":1}\n{\"b\":\n{\"c\":3}\n" (Eio.Path.load p);
  Eio_fs.Sync.append_lines p [ {|{"d":4}|} ];
  Alcotest.(check string)
    "an LF is not added twice" "{\"a\":1}\n{\"b\":\n{\"c\":3}\n{\"d\":4}\n"
    (Eio.Path.load p)

let test_append_to_empty () =
  with_mem @@ fun p ->
  Eio.Path.save ~create:(`Exclusive 0o644) p "";
  Eio_fs.Sync.append_lines p [ "x" ];
  Alcotest.(check string) "no leading LF" "x\n" (Eio.Path.load p)

let test_append_nothing () =
  with_mem @@ fun p ->
  Eio_fs.Sync.append_lines p [];
  Alcotest.(check bool)
    "no file" true
    (Eio.Path.kind ~follow:true p = `Not_found)

let test_append_refuses_lf () =
  with_mem @@ fun p ->
  Eio.Path.save ~create:(`Exclusive 0o644) p "a\n";
  (match Eio_fs.Sync.append_lines p [ "b"; "c\nd" ] with
  | () -> Alcotest.fail "a line holding LF was appended"
  | exception Invalid_argument _ -> ());
  Alcotest.(check string) "unchanged" "a\n" (Eio.Path.load p)

let test_append_posix () =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let dir = Filename.temp_file "eio-fs-sync-append" "" in
  Sys.remove dir;
  Eio.Path.mkdir ~perm:0o755 (fs / dir);
  Fun.protect ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true (fs / dir))
  @@ fun () ->
  let p = fs / dir / "log.jsonl" in
  Eio_fs.Sync.append_lines p [ "1" ];
  Eio.Path.with_open_out ~append:true ~create:`Never p (fun f ->
      Eio.Flow.copy_string "2" f);
  Eio_fs.Sync.append_lines p [ "3" ];
  Alcotest.(check string) "repaired" "1\n2\n3\n" (Eio.Path.load p)

(* A journal whose LF commits a record drops a torn last line before it
   appends: complete lines stay as they are, malformed ones included, and a
   tail longer than a read block goes whole. *)
let append_discards_a_torn_tail p =
  List.iter
    (fun (before, expected) ->
      Eio.Path.save ~create:(`Or_truncate 0o600) p before;
      Eio_fs.Sync.append_lines ~tail:`Discard p [ "new" ];
      Alcotest.(check string)
        (Fmt.str "after %S" before)
        expected (Eio.Path.load p))
    [
      ("", "new\n");
      ("pending\n", "pending\nnew\n");
      ("pending\nfinished", "pending\nnew\n");
      ("finished", "new\n");
      ("bad committed JSON\npartial", "bad committed JSON\nnew\n");
      ("pending\n" ^ String.make 9000 'x', "pending\nnew\n");
    ];
  Eio.Path.save ~create:(`Or_truncate 0o600) p "pending\ntorn";
  Eio_fs.Sync.append_lines ~tail:`Discard p [];
  Alcotest.(check string)
    "an empty append drops the torn line" "pending\n" (Eio.Path.load p);
  Eio.Path.unlink p;
  Eio_fs.Sync.append_lines ~tail:`Discard p [];
  Alcotest.(check bool)
    "an empty append makes no file" true
    (Eio.Path.kind ~follow:true p = `Not_found)

let test_append_discards_a_torn_tail () = with_mem append_discards_a_torn_tail

let test_append_discards_a_torn_tail_posix () =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let dir = Filename.temp_file "eio-fs-sync-tail" "" in
  Sys.remove dir;
  Eio.Path.mkdir ~perm:0o755 (fs / dir);
  Fun.protect ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true (fs / dir))
  @@ fun () -> append_discards_a_torn_tail (fs / dir / "journal")

(* The directory barrier that makes a created log's entry durable can fail
   after the file is written. The next durable append barriers the directory
   again, or the file it was told is durable is lost at a crash. *)
let test_append_retries_a_failed_parent_barrier () =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs_mem.run @@ fun env ->
  let dir_syncs = ref 0 in
  let schedule (op : Eio_fs_chaos.Op.t) : Eio_fs_chaos.Fault.t option =
    if op.kind = `Sync && op.path <> "log.jsonl" then incr dir_syncs;
    if op.kind = `Sync && String.equal op.path "" && op.nth = 1 then
      Some (Fail `Eio)
    else None
  in
  let p = Eio_fs_chaos.wrap schedule env#fs / "log.jsonl" in
  (match Eio_fs.Sync.append_lines p [ "1" ] with
  | () -> Alcotest.fail "the directory barrier did not fail"
  | exception Eio.Io _ -> ());
  dir_syncs := 0;
  Eio_fs.Sync.append_lines p [ "2" ];
  Alcotest.(check bool) "the directory is barriered again" true (!dir_syncs > 0);
  Alcotest.(check string) "both lines" "1\n2\n" (Eio.Path.load p)

(* A retry with nothing to append still owes the barriers: after it, the log
   created by an append whose directory barrier failed survives a cut. *)
let test_append_nothing_retries_the_barriers () =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs_mem.run @@ fun env ->
  let module Chaos = Eio_fs_chaos in
  let module Crash = Chaos.Private.Crash in
  let ctl = Crash.v () in
  let schedule (op : Chaos.Op.t) : Chaos.Fault.t option =
    if op.kind = `Sync && String.equal op.path "" && op.nth = 1 then
      Some (Fail `Eio)
    else None
  in
  let p = Chaos.wrap schedule (Crash.wrap ctl env#fs) / "log.jsonl" in
  (match Eio_fs.Sync.append_lines p [ "1" ] with
  | () -> Alcotest.fail "the directory barrier did not fail"
  | exception Eio.Io _ -> ());
  Eio_fs.Sync.append_lines p [];
  Crash.cut ctl;
  Alcotest.(check string)
    "the log survives" "1\n"
    (Eio.Path.load (Crash.post_crash ctl / "log.jsonl"))

let test_sparse_bounds_before_io () =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs_mem.run @@ fun env ->
  let operations = ref [] in
  let schedule op =
    operations := op :: !operations;
    None
  in
  let fs = Eio_fs_chaos.wrap schedule env#fs in
  let byte = Cstruct.of_string "x" in
  List.iter
    (fun (name, ceiling, size, spans, limit) ->
      operations := [];
      (match
         Eio_fs.Sync.publish_sparse ~max_bytes:ceiling ~size_bytes:size
           (fs / "image") spans
       with
      | () -> Alcotest.failf "%s was accepted" name
      | exception Eio_fs.Sync.Limit_exceeded { max_bytes; attempted } when limit
        ->
          Alcotest.(check int64) "ceiling" ceiling max_bytes;
          Alcotest.(check int64) "attempted logical size" size attempted
      | exception Invalid_argument _ when not limit -> ());
      Alcotest.(check int)
        (name ^ " refuses before any filesystem operation")
        0 (List.length !operations))
    [
      ("logical ceiling", 7L, 8L, [], true);
      ("negative ceiling", -1L, 0L, [], false);
      ("negative size", 8L, -1L, [], false);
      ("unrepresentable size", Int64.max_int, Int64.max_int, [], false);
      ("negative offset", 8L, 8L, [ (-1L, byte) ], false);
      ("offset beyond end", 8L, 8L, [ (Int64.max_int, byte) ], false);
      ("data beyond end", 8L, 8L, [ (8L, byte) ], false);
      ("overlap", 8L, 8L, [ (0L, byte); (0L, byte) ], false);
      ("unordered", 8L, 8L, [ (2L, byte); (0L, byte) ], false);
    ]

let test_sparse_exclusive () =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs_mem.run @@ fun env ->
  let fs = env#fs in
  Eio.Path.save ~create:(`Exclusive 0o600) (fs / "image") "foreign bytes";
  Eio.Path.symlink ~link_to:"missing" (fs / "link");
  List.iter
    (fun name ->
      (match
         Eio_fs.Sync.publish_sparse ~max_bytes:16L
           ~exclusive:Eio_fs.Sync.Rename.capability ~size_bytes:16L (fs / name)
           [ (0L, Cstruct.of_string "new") ]
       with
      | () -> Alcotest.failf "sparse publication replaced %s" name
      | exception Eio.Io (Eio.Fs.E (Already_exists _), _) -> ());
      Alcotest.(check string)
        "foreign file retained byte for byte" "foreign bytes"
        (Eio.Path.load (fs / "image"));
      Alcotest.(check string)
        "dangling symlink retained" "missing"
        (Eio.Path.read_link (fs / "link"));
      Alcotest.(check (list string))
        "no failed-publication temporary remains" [ "image"; "link" ]
        (List.sort String.compare (Eio.Path.read_dir fs)))
    [ "image"; "link" ]

let test_sparse_write_failure () =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs_mem.run @@ fun env ->
  Eio.Path.save ~create:(`Exclusive 0o600) (env#fs / "image") "foreign bytes";
  let schedule (op : Eio_fs_chaos.Op.t) : Eio_fs_chaos.Fault.t option =
    if op.kind = `Write && String.equal op.path "draft" && op.nth = 1 then
      Some (Torn 1)
    else None
  in
  let fs = Eio_fs_chaos.wrap schedule env#fs in
  (match
     Eio_fs.Sync.publish_sparse ~max_bytes:16L ~tmp:(fs / "draft")
       ~size_bytes:16L (fs / "image")
       [ (4L, Cstruct.of_string "new") ]
   with
  | () -> Alcotest.fail "torn extent was published"
  | exception Eio.Io _ -> ());
  Alcotest.(check string)
    "failed producer preserves the target" "foreign bytes"
    (Eio.Path.load (env#fs / "image"));
  Alcotest.(check bool)
    "failed temporary removed" true
    (Eio.Path.kind ~follow:false (env#fs / "draft") = `Not_found)

let test_sparse_durable_and_empty () =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs_mem.run @@ fun env ->
  let module Crash = Eio_fs_chaos.Private.Crash in
  let ctl = Crash.v () in
  let fs = Crash.wrap ctl env#fs in
  Eio_fs.Sync.publish_sparse ~max_bytes:16L
    ~exclusive:Eio_fs.Sync.Rename.capability ~size_bytes:16L (fs / "image")
    [ (0L, Cstruct.of_string "HDR"); (12L, Cstruct.of_string "TAIL") ];
  Eio_fs.Sync.publish_sparse ~max_bytes:0L
    ~exclusive:Eio_fs.Sync.Rename.capability ~size_bytes:0L (fs / "empty") [];
  Crash.cut ctl;
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "sparse bytes survive file and directory barriers"
    ("HDR" ^ String.make 9 '\000' ^ "TAIL")
    (Eio.Path.load (post / "image"));
  Alcotest.(check string)
    "zero-size file survives" ""
    (Eio.Path.load (post / "empty"))

let test_sparse_cancellation () =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs_mem.run @@ fun env ->
  Eio.Path.save ~create:(`Exclusive 0o600) (env#fs / "image") "foreign bytes";
  let ready, start = Eio.Promise.create () in
  let blocked, hold = Eio.Promise.create () in
  let schedule (op : Eio_fs_chaos.Op.t) =
    if op.kind = `Write then begin
      Eio.Promise.resolve start ();
      Eio.Promise.await blocked
    end;
    None
  in
  let fs = Eio_fs_chaos.wrap schedule env#fs in
  Eio.Fiber.first
    (fun () ->
      Eio_fs.Sync.publish_sparse ~max_bytes:16L ~tmp:(fs / "draft")
        ~size_bytes:16L (fs / "image")
        [ (4L, Cstruct.of_string "new") ];
      Alcotest.fail "gated producer completed before cancellation")
    (fun () -> Eio.Promise.await ready);
  Eio.Promise.resolve hold ();
  Alcotest.(check string)
    "cancelled producer preserves target bytes" "foreign bytes"
    (Eio.Path.load (env#fs / "image"));
  Alcotest.(check bool)
    "cancelled temporary removed" true
    (Eio.Path.kind ~follow:false (env#fs / "draft") = `Not_found)

exception Publication_origin

let[@inline never] raise_publication_origin () = raise Publication_origin

let test_error_backtrace generation () =
  let previous = Printexc.backtrace_status () in
  Printexc.record_backtrace true;
  Fun.protect ~finally:(fun () -> Printexc.record_backtrace previous)
  @@ fun () ->
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs_mem.run @@ fun env ->
  match
    if generation then
      Eio_fs.Sync.publish_generation ~barrier:Eio_fs.Sync.Barrier.none
        (env#fs / "generation") (fun _ -> raise_publication_origin ())
    else
      Eio_fs.Sync.publish ~max_bytes:1L (env#fs / "image") (fun _ ->
          raise_publication_origin ())
  with
  | () -> Alcotest.fail "producer exception was swallowed"
  | exception Publication_origin ->
      let trace = Printexc.get_raw_backtrace () in
      Alcotest.(check bool)
        "cleanup retains the producer's original throw site" true
        (contains ~affix:"raise_publication_origin"
           (Printexc.raw_backtrace_to_string trace))

(* A small value is copied through a small buffer: a 128 KiB bigarray for
   each save drives the major collector, which counts its bytes. *)
let test_small_copy_buffer () =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs_mem.run @@ fun env ->
  let sizes = ref [] in
  let module Src = struct
    type t = { mutable left : string }

    let read_methods = []

    let single_read t buf =
      sizes := Cstruct.length buf :: !sizes;
      if t.left = "" then raise End_of_file;
      let n = min (Cstruct.length buf) (String.length t.left) in
      Cstruct.blit_from_string t.left 0 buf 0 n;
      t.left <- String.sub t.left n (String.length t.left - n);
      n
  end in
  let src =
    Eio.Resource.T ({ Src.left = "v" }, Eio.Flow.Pi.source (module Src))
  in
  Eio_fs.Sync.publish ~max_bytes:3L (env#fs / "cfg") (fun f ->
      Eio.Flow.copy src f);
  Alcotest.(check string) "published" "v" (Eio.Path.load (env#fs / "cfg"));
  Alcotest.(check bool)
    "no read buffer above 4 KiB" true
    (List.for_all (fun n -> n <= 4096) !sizes)

(* A name of NAME_MAX length is a valid target: 255 bytes, or on macOS 255
   UTF-16 units, which 255 three-byte characters are. The temporaries, links
   and generations made beside it must be valid names too. *)
let publish_long_name fs unit =
  let name = String.concat "" (List.init 255 (fun _ -> unit)) in
  List.iter
    (fun d -> Eio.Path.mkdir ~perm:0o755 (fs / d))
    [ "file"; "tree"; "link" ];
  Eio_fs.Sync.save (fs / "file" / name) "saved";
  Alcotest.(check string)
    "a saved file" "saved"
    (Eio.Path.load (fs / "file" / name));
  let generation path i =
    Eio_fs.Sync.publish_generation path (fun d ->
        Eio.Path.save ~create:(`Exclusive 0o644) (d / "f") i)
  in
  (* A name sharing all but its last character has generations of its own. *)
  let other =
    String.sub name 0 (String.length name - String.length unit) ^ "-"
  in
  generation (fs / "tree" / other) "other";
  List.iter (generation (fs / "tree" / name)) [ "1"; "2"; "3" ];
  Alcotest.(check string)
    "a published generation" "3"
    (Eio.Path.load (fs / "tree" / name / "f"));
  Alcotest.(check string)
    "the other name's generation" "other"
    (Eio.Path.load (fs / "tree" / other / "f"));
  Alcotest.(check int)
    "two pointers, one generation of one, two of the other" 5
    (List.length (Eio.Path.read_dir (fs / "tree")));
  (match Eio_fs.Sync.publish_link ~target:"target" (fs / "link" / name) with
  | Ok () -> ()
  | Error _ -> Alcotest.fail "the link was refused");
  Alcotest.(check string)
    "a published link" "target"
    (Eio.Path.read_link (fs / "link" / name))

let test_long_name_in_memory os unit () =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs_mem.run ~os @@ fun env -> publish_long_name env#fs unit

let test_long_name_on_disk () =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let dir = Filename.temp_file "eio-fs-sync-long" "" in
  Sys.remove dir;
  Eio.Path.mkdir ~perm:0o755 (fs / dir);
  Fun.protect ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true (fs / dir))
  @@ fun () -> publish_long_name (fs / dir) "n"

(* Only a target's name is cut, never its directories: a temporary made in an
   ancestor fails where the user may not write, here the first of two
   100-byte directories. *)
let test_long_directories () =
  Eio_mock.Backend.run @@ fun () ->
  let root = Eio_fs_mem.v () in
  let d1 = String.make 100 'a' and d2 = String.make 100 'b' in
  Eio.Path.mkdir ~perm:0o755 (root / d1);
  Eio.Path.mkdir ~perm:0o755 (root / d1 / d2);
  Eio.Path.chown ~follow:true ~uid:1000L ~gid:1000L (root / d1 / d2);
  let alice =
    { Eio_fs_mem.uid = 1000L; gid = 1000L; groups = []; umask = 0o022 }
  in
  let dir = Eio_fs_mem.as_user alice root / d1 / d2 in
  let target = d1 ^ "/" ^ d2 ^ "/state" in
  Eio_fs.Sync.save (dir / "state") "saved";
  Alcotest.(check string) "a saved file" "saved" (Eio.Path.load (dir / "state"));
  Eio_fs.Sync.publish_generation (dir / "tree") (fun d ->
      Eio.Path.save ~create:(`Exclusive 0o644) (d / "f") "g");
  Alcotest.(check string)
    "a published generation" "g"
    (Eio.Path.load (dir / "tree" / "f"));
  (match Eio_fs.Sync.publish_link ~target:"target" (dir / "link") with
  | Ok () -> ()
  | Error _ -> Alcotest.fail "the link was refused");
  Eio.Path.mkdir ~perm:0o755 (dir / "long");
  publish_long_name (dir / "long") "n";
  Alcotest.(check (list string))
    "nothing made in the ancestor" [ d2 ]
    (Eio.Path.read_dir (root / d1));
  Alcotest.(check string)
    "a fresh name stays in the target's directory" (Filename.dirname target)
    (Filename.dirname (Eio_fs.Sync.fresh_name target))

let suite =
  ( "sync",
    [
      Alcotest.test_case "a 255-byte name in memory" `Quick
        (test_long_name_in_memory `Linux "n");
      Alcotest.test_case "a 255-byte name in memory, macOS" `Quick
        (test_long_name_in_memory `Macos "n");
      Alcotest.test_case "a 255-unit name in memory, macOS" `Quick
        (test_long_name_in_memory `Macos "\xe6\x97\xa5");
      Alcotest.test_case "a 255-byte name on disk" `Quick
        (Host.posix_only "Windows syncs no file open for reading only"
           test_long_name_on_disk);
      Alcotest.test_case "names under long directories" `Quick
        test_long_directories;
      Alcotest.test_case "a small value is copied through a small buffer" `Quick
        test_small_copy_buffer;
      Alcotest.test_case "sparse bounds precede all IO" `Quick
        test_sparse_bounds_before_io;
      Alcotest.test_case "sparse exclusive publication" `Quick
        test_sparse_exclusive;
      Alcotest.test_case "sparse write failure" `Quick test_sparse_write_failure;
      Alcotest.test_case "sparse durable and empty files" `Quick
        test_sparse_durable_and_empty;
      Alcotest.test_case "sparse cancellation removes temporary" `Quick
        test_sparse_cancellation;
      Alcotest.test_case "exclusive publication refuses existing names" `Quick
        test_exclusive_publication;
      Alcotest.test_case "exclusive publication survives crash" `Quick
        test_exclusive_publication_survives_crash;
      Alcotest.test_case "registered backend" `Quick test_registered_backend;
      Alcotest.test_case "save a string" `Quick test_save;
      Alcotest.test_case "save on Windows" `Quick test_save_on_windows;
      Alcotest.test_case "the barrier given is the one issued" `Quick
        test_barrier_is_used;
      Alcotest.test_case "append lines" `Quick test_append_creates;
      Alcotest.test_case "append ends a torn line" `Quick
        test_append_repairs_a_torn_line;
      Alcotest.test_case "append to an empty file" `Quick test_append_to_empty;
      Alcotest.test_case "append nothing" `Quick test_append_nothing;
      Alcotest.test_case "append refuses a line holding LF" `Quick
        test_append_refuses_lf;
      Alcotest.test_case "append on posix" `Quick
        (Host.posix_only "eio_windows 1.7 has no positioned read"
           test_append_posix);
      Alcotest.test_case "append discards a torn tail" `Quick
        test_append_discards_a_torn_tail;
      Alcotest.test_case "append discards a torn tail on posix" `Quick
        (Host.posix_only "eio_windows 1.7 has no positioned read"
           test_append_discards_a_torn_tail_posix);
      Alcotest.test_case "append nothing retries the barriers" `Quick
        test_append_nothing_retries_the_barriers;
      Alcotest.test_case "append retries a failed parent barrier" `Quick
        test_append_retries_a_failed_parent_barrier;
      Alcotest.test_case "posix needs no registration" `Quick
        test_posix_needs_no_registration;
      Alcotest.test_case "published mode ignores the umask" `Quick
        (Host.posix_only "Windows has no umask"
           test_publish_honours_perm_under_umask);
      Alcotest.test_case "unissuable barrier raises" `Quick
        (Host.posix_only
           "eio_windows syncs no directory, so it finds none missing"
           test_unissuable_barrier_raises);
      Alcotest.test_case "failed save cleans temporary" `Quick
        test_failed_save_cleans_temporary;
      Alcotest.test_case "publish with a remote temporary" `Quick
        test_publish_with_remote_tmp;
      Alcotest.test_case "publish with a streaming producer" `Quick
        test_publish_with_stream;
      Alcotest.test_case "failed producer preserves target" `Quick
        test_publish_with_failure_preserves_target;
      Alcotest.test_case "temporary differs from target" `Quick
        test_publish_rejects_target_as_temporary;
      Alcotest.test_case "aliased temporary differs from target" `Quick
        test_publish_rejects_aliased_target;
      Alcotest.test_case "foreign temporary is preserved" `Quick
        test_publish_does_not_remove_unowned_temporary;
      Alcotest.test_case "publication enforces byte limit" `Quick
        test_publish_enforces_byte_limit;
      Alcotest.test_case "publish durability flag" `Quick
        test_publish_durability_flag;
      Alcotest.test_case "file barrier reaches the crash model" `Quick
        test_sync_file_reaches_the_crash_model;
      Alcotest.test_case "file cleanup retains producer backtrace" `Quick
        (test_error_backtrace false);
      Alcotest.test_case "generation cleanup retains producer backtrace" `Quick
        (test_error_backtrace true);
    ] )

module Links = struct
  let fixture test =
    Eio_mock.Backend.run @@ fun () ->
    Eio_fs_mem.run @@ fun env -> test env#fs

  let publish fs ?(barrier = Eio_fs.Sync.Barrier.none) replace target =
    Eio_fs.Sync.publish_link ~barrier ?replace ~target (fs / "portal")

  let test_raw () =
    fixture @@ fun fs ->
    let raw = "../old library//./checkout" in
    Eio.Path.symlink ~link_to:raw (fs / "portal");
    ignore (Result.get_ok (publish fs (Some raw) "/selected/lib"));
    Alcotest.(check string)
      "whole selected link" "/selected/lib"
      (Eio.Path.read_link (fs / "portal"));
    ignore (Result.get_ok (publish fs (Some "/selected/lib") raw));
    Alcotest.(check string)
      "exact original raw bytes" raw
      (Eio.Path.read_link (fs / "portal"))

  let test_refusal () =
    fixture @@ fun fs ->
    Eio.Path.symlink ~link_to:"../foreign" (fs / "portal");
    (match publish fs (Some "../old") "/new" with
    | Error (`Changed (Some "../foreign")) -> ()
    | _ -> Alcotest.fail "changed portal replaced");
    Alcotest.(check string)
      "foreign target survives" "../foreign"
      (Eio.Path.read_link (fs / "portal"));
    (match publish fs (Some "../old") "../foreign" with
    | Error (`Changed (Some "../foreign")) -> ()
    | _ -> Alcotest.fail "changed portal taken as already published");
    Eio.Path.unlink (fs / "portal");
    (match publish fs (Some "../old") "/new" with
    | Error (`Changed None) -> ()
    | _ -> Alcotest.fail "missing portal replaced");
    Eio.Path.save ~create:(`Exclusive 0o644) (fs / "portal") "foreign file";
    (match publish fs None "/new" with
    | Error (`Invalid _) -> ()
    | _ -> Alcotest.fail "regular file replaced");
    Alcotest.(check string)
      "foreign file survives" "foreign file"
      (Eio.Path.load (fs / "portal"))

  let test_unwritable_target () =
    fixture @@ fun fs ->
    Eio.Path.symlink ~link_to:"../old" (fs / "portal");
    List.iter
      (fun target ->
        match publish fs (Some "../old") target with
        | Error (`Invalid _) -> ()
        | _ -> Alcotest.failf "target %S published" target)
      [ ""; "../new\000library" ];
    Alcotest.(check string)
      "old target survives" "../old"
      (Eio.Path.read_link (fs / "portal"));
    Alcotest.(check (list string))
      "nothing staged" [ "portal" ] (Eio.Path.read_dir fs)

  let test_crash () =
    fixture @@ fun fs ->
    let module Crash = Eio_fs_chaos.Private.Crash in
    Eio.Path.symlink ~link_to:"../old" (fs / "portal");
    Eio.Path.sync_dir fs;
    let ctl = Crash.v () in
    let wrapped = Crash.wrap ctl fs in
    ignore
      (Result.get_ok
         (publish wrapped ~barrier:Eio_fs.Sync.Barrier.portable (Some "../old")
            "../new library//./checkout"));
    Crash.cut ctl;
    Alcotest.(check string)
      "raw link survives native metadata barriers" "../new library//./checkout"
      (Eio.Path.read_link (Crash.post_crash ctl / "portal"))

  let test_durable_retry () =
    fixture @@ fun fs ->
    let module Crash = Eio_fs_chaos.Private.Crash in
    let module Chaos = Eio_fs_chaos in
    Eio.Path.symlink ~link_to:"../old" (fs / "portal");
    let ctl = Crash.v () in
    let disk = Crash.wrap ctl fs in
    let syncs = ref 0 in
    let schedule (op : Chaos.Op.t) =
      if op.kind = `Sync then (
        incr syncs;
        if !syncs = 2 then failwith "fixture post-rename barrier");
      None
    in
    let faults = Chaos.wrap schedule disk in
    (match
       publish faults ~barrier:Eio_fs.Sync.Barrier.portable (Some "../old")
         "../new"
     with
    | _ -> Alcotest.fail "post-publication barrier failure hidden"
    | exception Failure reason ->
        Alcotest.(check string)
          "uncertain durability reported" "fixture post-rename barrier" reason);
    Alcotest.(check string)
      "new link already visible" "../new"
      (Eio.Path.read_link (fs / "portal"));
    ignore
      (Result.get_ok
         (publish disk ~barrier:Eio_fs.Sync.Barrier.portable (Some "../new")
            "../new"));
    Crash.cut ctl;
    Alcotest.(check string)
      "durable no-op redeems publication" "../new"
      (Eio.Path.read_link (Crash.post_crash ctl / "portal"))

  let test_last_observation () =
    fixture @@ fun fs ->
    let module Chaos = Eio_fs_chaos in
    Eio.Path.symlink ~link_to:"../old" (fs / "portal");
    let changed = ref false in
    let schedule (op : Chaos.Op.t) =
      if op.kind = `Sync && not !changed then (
        changed := true;
        Eio.Path.unlink (fs / "portal");
        Eio.Path.symlink ~link_to:"../foreign" (fs / "portal"));
      None
    in
    let faults = Chaos.wrap schedule fs in
    (match
       publish faults ~barrier:Eio_fs.Sync.Barrier.portable (Some "../old")
         "../new"
     with
    | Error (`Changed (Some "../foreign")) -> ()
    | _ -> Alcotest.fail "final raw observation ignored");
    Alcotest.(check string)
      "late foreign target preserved" "../foreign"
      (Eio.Path.read_link (fs / "portal"));
    Alcotest.(check (list string))
      "owned staging cleaned" [ "portal" ] (Eio.Path.read_dir fs)

  let test_posix () =
    Eio_main.run @@ fun env ->
    let fs = Eio.Stdenv.fs env in
    let dir = Filename.temp_file "eio-fs-raw-link" "" in
    Sys.remove dir;
    Eio.Path.mkdir ~perm:0o755 (fs / dir);
    Fun.protect ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true (fs / dir))
    @@ fun () ->
    let root = fs / dir in
    Eio.Path.symlink ~link_to:"../old library//./checkout" (root / "portal");
    ignore
      (Result.get_ok
         (publish root ~barrier:Eio_fs.Sync.Barrier.portable
            (Some "../old library//./checkout") "/selected/library"));
    Alcotest.(check string)
      "native POSIX raw switch" "/selected/library"
      (Eio.Path.read_link (root / "portal"));
    Alcotest.(check (list string))
      "no native staging remains" [ "portal" ] (Eio.Path.read_dir root)

  (* A new link is made in place, where nothing stood: one already holding
     the target is the publication done, and anything else stays. *)
  let test_create () =
    fixture @@ fun fs ->
    ignore (Result.get_ok (publish fs None "/selected"));
    Alcotest.(check (list string))
      "made in place, nothing staged" [ "portal" ] (Eio.Path.read_dir fs);
    (match publish fs None "/selected" with
    | Ok () -> ()
    | Error _ -> Alcotest.fail "the link already made was refused");
    (match publish fs None "/other" with
    | Error (`Changed (Some "/selected")) -> ()
    | _ -> Alcotest.fail "a link to another target was replaced");
    Alcotest.(check string)
      "the first link stays" "/selected"
      (Eio.Path.read_link (fs / "portal"))
end

let link_suite =
  ( "publish_link",
    [
      Alcotest.test_case "durable retry after uncertain publication" `Quick
        Links.test_durable_retry;
      Alcotest.test_case "final raw observation preserves late foreign target"
        `Quick Links.test_last_observation;
      Alcotest.test_case "native POSIX raw link publication" `Quick
        Links.test_posix;
      Alcotest.test_case "exact raw publication and restoration" `Quick
        Links.test_raw;
      Alcotest.test_case "changed missing and regular portals refuse" `Quick
        Links.test_refusal;
      Alcotest.test_case "durable raw link survives crash" `Quick
        Links.test_crash;
      Alcotest.test_case "empty and NUL targets refuse" `Quick
        Links.test_unwritable_target;
      Alcotest.test_case "a new link is made in place" `Quick Links.test_create;
    ] )
