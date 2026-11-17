(* The crash model's writeback: which blocks, sectors and lengths a power cut
   keeps of the writes no sync made durable. *)

module Chaos = Eio_fs.Chaos
module Crash = Chaos.Private.Crash
open Chaos_helpers

let ( / ) = Eio.Path.( / )

(* Every sync fails with [EIO]. *)
let fail_syncs (op : Chaos.Op.t) =
  if op.kind = `Sync then Some (Chaos.Fault.Fail `Eio) else None

(* {2 Writeback in blocks} *)

(* A write of three blocks is not atomic: a crash keeps any of its blocks. *)
let test_crash_multi_block_write () =
  let block = 4096 in
  let data =
    String.init (3 * block) (fun i -> Char.chr (65 + Int.div i block))
  in
  let states = images (fun fs -> overwrite (fs / "f") data) in
  let partial s = s <> "" && s <> data in
  Alcotest.(check bool)
    "some state is partial" true
    (List.exists partial states);
  Alcotest.(check bool)
    "some state is a block prefix" true
    (List.mem (String.sub data 0 block) states);
  Alcotest.(check bool)
    "blocks come whole" true
    (List.for_all
       (fun s ->
         let ok = ref true in
         for b = 0 to Int.div (String.length s - 1) block do
           let blk =
             String.sub s (b * block)
               (min block (String.length s - (b * block)))
           in
           let mine = String.make (String.length blk) (Char.chr (65 + b)) in
           let zero = String.make (String.length blk) '\000' in
           if blk <> mine && blk <> zero then ok := false
         done;
         !ok)
       states)

(* Two appends in one block: the block is written back whole, so the second
   never lands without the first. *)
let appends fs =
  Eio.Path.with_open_out ~append:true ~create:`Never (fs / "f") (fun f ->
      Eio.Flow.copy_string "aaaa" f;
      Eio.Flow.copy_string "bbbb" f)

let test_crash_appends_in_one_block () =
  let states = images appends in
  Alcotest.(check bool)
    "no second append over a hole" false
    (List.mem "\000\000\000\000bbbb" states);
  Alcotest.(check bool) "both appends" true (List.mem "aaaabbbb" states);
  Alcotest.(check bool) "first append" true (List.mem "aaaa" states)

(* With blocks of four bytes the two appends are separate blocks. *)
let test_crash_block_size () =
  let states = images ~block_size:4 appends in
  Alcotest.(check bool)
    "the second append alone" true
    (List.mem "\000\000\000\000bbbb" states)

(* A rename over a file whose handle stays open: the write synced through
   the handle is durable in that file, which an uncommitted rename leaves at
   its name. *)
let test_crash_rename_over_an_open_file () =
  let ctl = Crash.v () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  write_new ~sync:true (fs / "b") "old";
  write_new ~sync:true (fs / "a") "other";
  sync_dirs fs;
  Eio.Switch.run (fun sw ->
      let h = Eio.Path.open_out ~sw ~create:`Never (fs / "b") in
      Eio.File.pwrite_all h ~file_offset:(i63 0) [ Cstruct.of_string "NEW" ];
      Eio.Path.rename (fs / "a") (fs / "b");
      Eio.File.sync h);
  let post = Crash.post_crash ctl in
  Alcotest.(check string) "b" "NEW" (Eio.Path.load (post / "b"));
  Alcotest.(check string) "a" "other" (Eio.Path.load (post / "a"))

(* A file created through a dangling symbolic link is a new entry at the
   link's target, journalled as any create. *)
let test_crash_create_through_a_dangling_link () =
  let ctl = Crash.v () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.symlink ~link_to:"target" (fs / "l");
  Eio.Path.with_open_out ~create:(`If_missing 0o644) (fs / "l") (fun f ->
      Eio.Flow.copy_string "data" f;
      Eio.File.sync f);
  Alcotest.(check bool)
    "the create is pending" true
    (List.exists
       (fun (u : Crash.unsynced) -> u.kind = Crash.Create)
       (Crash.unsynced ctl));
  Eio.Path.sync_dir fs;
  let post = Crash.post_crash ctl in
  Alcotest.(check string) "target" "data" (Eio.Path.load (post / "target"))

(* The model over a user's view reads and rebuilds the tree as root: the
   user cannot read root's files, and may lock themselves out of a
   directory. *)
let alice = { Eio_fs.uid = 1000L; gid = 1000L; groups = []; umask = 0o022 }

let test_crash_over_a_user_view () =
  Eio_mock.Backend.run @@ fun () ->
  let root = Eio_fs.Fs.v () in
  Eio.Path.save ~create:(`Exclusive 0o600) (root / "shadow") "s";
  Eio.Path.mkdir ~perm:0o755 (root / "home");
  Eio.Path.chown ~follow:true ~uid:1000L ~gid:1000L (root / "home");
  let a = Eio_fs.Fs.as_user alice root in
  ignore (Crash.wrap (Crash.v ()) a);
  let ctl = Crash.v () in
  let fs = Crash.wrap ctl (a / "home") in
  Eio.Path.mkdir ~perm:0o755 (fs / "d");
  write_new ~sync:true (fs / "d/f") "x";
  sync_dirs fs;
  Eio.Path.chmod ~follow:true ~perm:0o600 (fs / "d");
  Eio.Path.mkdir ~perm:0o755 (fs / "e");
  Eio.Path.chmod ~follow:true ~perm:0o000 (fs / "e");
  Eio.Path.mkdir ~perm:0o755 (fs / "g");
  Crash.crash ctl;
  Alcotest.(check (list string))
    "the durable tree" [ "d" ]
    (List.sort String.compare (Eio.Path.read_dir (root / "home")));
  Alcotest.(check string) "d/f" "x" (Eio.Path.load (root / "home/d/f"))

(* A crash rebuilds each entry with its owner: entries the disk held, removed
   through the wrapper with no barrier, come back as they were. *)
let test_crash_restores_owners () =
  Eio_mock.Backend.run @@ fun () ->
  let root = Eio_fs.Fs.v () in
  write_new ~sync:true (root / "f") "x";
  Eio.Path.chown ~follow:true ~uid:1000L ~gid:1000L (root / "f");
  Eio.Path.mkdir ~perm:0o700 (root / "d");
  Eio.Path.chown ~follow:true ~uid:1000L ~gid:1000L (root / "d");
  Eio.Path.symlink ~link_to:"f" (root / "l");
  Eio.Path.chown ~follow:false ~uid:1000L ~gid:1000L (root / "l");
  let ctl = Crash.v () in
  let fs = Crash.wrap ctl root in
  Eio.Path.unlink (fs / "f");
  Eio.Path.rmdir (fs / "d");
  Eio.Path.unlink (fs / "l");
  let post = Crash.post_crash ctl in
  let owner p =
    let s = Eio.Path.stat ~follow:false (post / p) in
    (s.uid, s.gid)
  in
  List.iter
    (fun p -> Alcotest.(check (pair int64 int64)) p (1000L, 1000L) (owner p))
    [ "f"; "d"; "l" ];
  Eio.Path.save ~create:`Never (Eio_fs.Fs.as_user alice post / "f") "y"

(* A barrier deferred on a directory belongs to that directory: once it is
   removed or renamed, a new directory of the same name does not inherit it. *)
let test_crash_deferred_barrier_identity () =
  let replaced how =
    let ctl = Crash.v () in
    with_crash ctl @@ fun ~raw:_ ~fs ->
    Eio.Path.mkdir ~perm:0o755 (fs / "d");
    write_new ~sync:true (fs / "d/f") "x";
    Eio.Path.sync_dir (fs / "d");
    how fs;
    Eio.Path.mkdir ~perm:0o755 (fs / "d");
    Eio.Path.sync_dir fs;
    Eio.Path.sync_dir (fs / "d");
    let post = Crash.post_crash ctl in
    ( Eio.Path.read_dir (post / "d"),
      List.sort String.compare (Eio.Path.read_dir post) )
  in
  let removed, _ =
    replaced (fun fs ->
        Eio.Path.unlink (fs / "d/f");
        Eio.Path.rmdir (fs / "d"))
  in
  Alcotest.(check (list string)) "after rmdir, the new d" [] removed;
  let renamed, top =
    replaced (fun fs -> Eio.Path.rename (fs / "d") (fs / "x"))
  in
  Alcotest.(check (list string)) "after rename, the new d" [] renamed;
  Alcotest.(check (list string)) "the root" [ "d"; "x" ] top

(* A change of mode or owner is metadata: lost unless a sync of the file, or
   a barrier on the directory itself, commits it. A barrier on the parent
   does not: ext4's fsync of a directory completes only its own last
   transaction. *)
let test_crash_chmod_journaled () =
  let mode ~sync =
    let ctl = Crash.v () in
    with_crash ctl @@ fun ~raw:_ ~fs ->
    write_new ~sync:true (fs / "f") "k";
    Eio.Path.mkdir ~perm:0o755 (fs / "d");
    sync_dirs fs;
    Eio.Path.chmod ~follow:true ~perm:0o600 (fs / "f");
    Eio.Path.chown ~follow:true ~uid:1000L ~gid:1000L (fs / "f");
    Eio.Path.chmod ~follow:true ~perm:0o700 (fs / "d");
    sync ctl fs;
    let post = Crash.post_crash ctl in
    let st p = Eio.Path.stat ~follow:false (post / p) in
    ((st "f").perm, (st "f").uid, (st "d").perm)
  in
  let check what expected got =
    Alcotest.(check (triple int int64 int)) what expected got
  in
  check "no sync" (0o644, 0L, 0o755) (mode ~sync:(fun _ _ -> ()));
  check "a sync of the file" (0o600, 1000L, 0o755)
    (mode ~sync:(fun _ fs ->
         Eio.Path.with_open_out ~create:`Never (fs / "f") Eio.File.sync));
  check "a barrier on the parent" (0o644, 0L, 0o755)
    (mode ~sync:(fun _ fs -> Eio.Path.sync_dir fs));
  check "a barrier on the directory itself" (0o644, 0L, 0o700)
    (mode ~sync:(fun _ fs -> Eio.Path.sync_dir (fs / "d")))

(* A sync makes a new file's mode durable before its name: whenever a crash
   keeps the name, the file has that mode. *)
let test_crash_synced_chmod_of_a_new_file () =
  let ctl = Crash.v ~policy:(Crash.Survive (fun _ -> true)) () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.with_open_out ~create:(`Exclusive 0o644) (fs / "f") (fun f ->
      Eio.Path.chmod ~follow:true ~perm:0o600 (fs / "f");
      Eio.File.sync f);
  let post = Crash.post_crash ctl in
  Alcotest.(check int)
    "mode" 0o600 (Eio.Path.stat ~follow:false (post / "f")).perm

(* A sync makes the file's mode durable wherever the file's name is: a
   pending rename, a rename onto itself, or a later directory of the old name
   change nothing. *)
let test_crash_synced_mode_follows_the_file () =
  let ctl = Crash.v () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  write_new ~sync:true (fs / "b") "x";
  sync_dirs fs;
  Eio.Path.chmod ~follow:true ~perm:0o777 (fs / "b");
  Eio.Path.rename (fs / "b") (fs / "l");
  Eio.Path.chmod ~follow:true ~perm:0o700 (fs / "l");
  Eio.Path.rename (fs / "l") (fs / "l");
  Eio.Path.with_open_out ~create:`Never (fs / "l") Eio.File.sync;
  Eio.Path.mkdir ~perm:0o755 (fs / "d");
  Eio.Path.chmod ~follow:true ~perm:0o711 (fs / "d");
  Eio.Path.rmdir (fs / "d");
  write_new ~sync:true (fs / "d") "y";
  let post = Crash.post_crash ctl in
  Alcotest.(check int) "b" 0o700 (Eio.Path.stat ~follow:false (post / "b")).perm;
  Alcotest.(check bool) "d" true (absent (post / "d"))

(* The model knows a file and a directory by inode, not by name: a user's
   file renamed over root's is never rewritten into root's, a file planted
   in a directory made again under a moved one's name never lands in the old
   one, and a mode synced through a read-only handle, or after a rename, is
   the file's. *)
let test_crash_inode_identity () =
  let setup () =
    let root = Eio_fs.Fs.v () in
    Eio.Path.mkdir ~perm:0o755 (root / "u");
    Eio.Path.chown ~follow:false ~uid:1000L ~gid:1000L (root / "u");
    root
  in
  let render post p = try Eio.Path.load (post / p) with Eio.Io _ -> "-" in
  ( Eio_mock.Backend.run @@ fun () ->
    let root = setup () in
    Eio.Path.save ~create:(`Exclusive 0o644) (root / "u/passwd") "root";
    Eio.Path.save ~create:(`Exclusive 0o644) (root / "u/mine") "alice";
    Eio.Path.chown ~follow:false ~uid:1000L ~gid:1000L (root / "u/mine");
    let ctl = Crash.v () in
    let fs = Crash.wrap ctl (Eio_fs.Fs.as_user alice root) in
    Eio.Path.rename (fs / "u/mine") (fs / "u/passwd");
    Eio.Path.with_open_out ~create:`Never (fs / "u/passwd") ignore;
    let post = Crash.post_crash ctl in
    Alcotest.(check (pair string string))
      "root's file keeps its bytes" ("root", "alice")
      (render post "u/passwd", render post "u/mine") );
  ( Eio_mock.Backend.run @@ fun () ->
    let root = setup () in
    Eio.Path.mkdir ~perm:0o755 (root / "u/d");
    let ctl = Crash.v () in
    let fs = Crash.wrap ctl (Eio_fs.Fs.as_user alice root) in
    Eio.Path.rename (fs / "u/d") (fs / "u/x");
    Eio.Path.mkdir ~perm:0o755 (fs / "u/d");
    write_new ~sync:true (fs / "u/d/planted") "evil";
    Eio.Path.sync_dir (fs / "u/d");
    let post = Crash.post_crash ctl in
    Alcotest.(check (list string))
      "root's directory is untouched" []
      (Eio.Path.read_dir (post / "u/d")) );
  Eio_mock.Backend.run @@ fun () ->
  let root = Eio_fs.Fs.v () in
  Eio.Path.save ~create:(`Exclusive 0o644) (root / "f") "x";
  Eio.Path.save ~create:(`Exclusive 0o644) (root / "g") "y";
  let ctl = Crash.v () in
  let fs = Crash.wrap ctl root in
  Eio.Path.chmod ~follow:false ~perm:0o600 (fs / "f");
  Eio.Switch.run (fun sw ->
      Eio_fs_sync.Barrier.(sync_file portable) (Eio.Path.open_in ~sw (fs / "f")));
  Eio.Path.chmod ~follow:false ~perm:0o600 (fs / "g");
  Eio.Path.rename (fs / "g") (fs / "h");
  Eio.Path.with_open_out ~create:`Never (fs / "h") Eio.File.sync;
  let post = Crash.post_crash ctl in
  let mode p = (Eio.Path.stat ~follow:false (post / p)).perm in
  Alcotest.(check (pair int int)) "modes" (0o600, 0o600) (mode "f", mode "g")

(* An entry takes effect whole, with the pending entries it needs, and a
   rename moves the image's own node: synced bytes follow their file to a
   renamed directory, a durable file survives a rename out of a directory
   later removed but is lost with a new directory whose parent was never
   synced, and a moved directory carries only what the image holds, with an
   earlier rename in the same source directory. *)
let test_crash_entries_whole () =
  let run setup ops =
    let ctl = Crash.v () in
    Eio_mock.Backend.run @@ fun () ->
    let raw = Eio_fs.Fs.v () in
    setup raw;
    let fs = Crash.wrap ctl raw in
    ops fs;
    let post = Crash.post_crash ctl in
    let rec walk p acc =
      List.fold_left
        (fun acc n ->
          let q = Eio.Path.(post / p / n) in
          let key = if p = "" then n else p ^ "/" ^ n in
          match Eio.Path.kind ~follow:false q with
          | `Directory -> walk key ((key ^ "/") :: acc)
          | _ -> (key ^ "=" ^ Eio.Path.load q) :: acc)
        acc
        (List.sort String.compare (Eio.Path.read_dir Eio.Path.(post / p)))
    in
    List.rev (walk "" [])
  in
  let dirs raw l =
    List.iter (fun d -> Eio.Path.mkdir ~perm:0o755 (raw / d)) l
  in
  let save raw p s = Eio.Path.save ~create:(`Exclusive 0o644) (raw / p) s in
  let check what expected got =
    Alcotest.(check (list string)) what expected got
  in
  check "synced bytes in a renamed directory" [ "D/"; "D/f=synced" ]
    (run
       (fun raw -> dirs raw [ "D" ])
       (fun fs ->
         Eio.Path.rename (fs / "D") (fs / "E");
         write_new ~sync:true (fs / "E/f") "synced";
         Eio.Path.sync_dir (fs / "E")));
  check "a move out of a directory then removed"
    [ "P/"; "Q/"; "Q/C/"; "Q/C/x=durable" ]
    (run
       (fun raw ->
         dirs raw [ "P"; "P/A"; "P/A/B"; "Q" ];
         save raw "P/A/B/x" "durable")
       (fun fs ->
         Eio.Path.rename (fs / "P/A/B") (fs / "Q/C");
         Eio.Path.rmdir (fs / "P/A");
         Eio.Path.sync_dir (fs / "P")));
  check "a rename into a new directory, its parent never synced" [ "P/" ]
    (run
       (fun raw ->
         dirs raw [ "P" ];
         save raw "P/f" "durable")
       (fun fs ->
         Eio.Path.mkdir ~perm:0o755 (fs / "Q");
         Eio.Path.rename (fs / "P/f") (fs / "Q/f");
         Eio.Path.sync_dir (fs / "P")));
  check "a moved directory carries only what the image holds"
    [ "A/"; "B/"; "B/e/"; "B/e/f=data" ]
    (run
       (fun raw ->
         dirs raw [ "A"; "A/d"; "B" ];
         save raw "A/f" "data")
       (fun fs ->
         Eio.Path.rename (fs / "A/f") (fs / "A/d/f");
         write_new ~sync:true (fs / "A/d/new") "x";
         Eio.Path.rename (fs / "A/d") (fs / "B/e");
         Eio.Path.sync_dir (fs / "B")))

(* A crash gives each entry its owner before its mode: a chown clears
   setuid and setgid, which the mode then sets back. *)
let test_crash_keeps_privilege_bits () =
  Eio_mock.Backend.run @@ fun () ->
  let raw = Eio_fs.Fs.v () in
  Eio.Path.save ~create:(`Exclusive 0o644) (raw / "suid") "x";
  Eio.Path.chown ~follow:false ~uid:1000L ~gid:1000L (raw / "suid");
  Eio.Path.chmod ~follow:false ~perm:0o6755 (raw / "suid");
  Eio.Path.mkdir ~perm:0o755 (raw / "sg");
  Eio.Path.chown ~follow:false ~uid:1000L ~gid:1000L (raw / "sg");
  Eio.Path.chmod ~follow:false ~perm:0o2775 (raw / "sg");
  let ctl = Crash.v () in
  ignore (Crash.wrap ctl raw);
  let post = Crash.post_crash ctl in
  let mode p = (Eio.Path.stat ~follow:false (post / p)).perm in
  Alcotest.(check (pair int int))
    "modes" (0o6755, 0o2775)
    (mode "suid", mode "sg")

(* A user's write clears setuid; the sync after it makes that mode durable. *)
let test_crash_sync_records_the_mode () =
  Eio_mock.Backend.run @@ fun () ->
  let raw = Eio_fs.Fs.v () in
  Eio.Path.save ~create:(`Exclusive 0o644) (raw / "suid") "x";
  Eio.Path.chown ~follow:false ~uid:1000L ~gid:1000L (raw / "suid");
  Eio.Path.chmod ~follow:false ~perm:0o4755 (raw / "suid");
  let ctl = Crash.v () in
  let fs = Crash.wrap ctl (Eio_fs.Fs.as_user alice raw) in
  Eio.Path.with_open_out ~create:`Never (fs / "suid") (fun f ->
      Eio.File.pwrite_all f ~file_offset:(i63 0) [ Cstruct.of_string "y" ];
      Eio.File.sync f);
  let post = Crash.post_crash ctl in
  Alcotest.(check (pair int string))
    "mode and bytes" (0o755, "y")
    ( (Eio.Path.stat ~follow:false (post / "suid")).perm,
      Eio.Path.load (post / "suid") )

(* A file truncated and rewritten, unsynced: writeback may land the new
   length over the block from before the truncation, so the bytes it cut
   come back; at the old length, the block the truncation would free holds
   its old bytes. Every outcome is one of those. *)
let test_crash_truncation_follows_writeback () =
  let seen = Hashtbl.create 8 in
  for seed = 0 to 199 do
    Eio_mock.Backend.run @@ fun () ->
    let raw = Eio_fs.Fs.v () in
    Eio.Path.save ~create:(`Exclusive 0o644) (raw / "f") "dgjutfwss";
    let ctl = Crash.v ~policy:(Reorder_unsynced seed) () in
    let fs = Crash.wrap ctl raw in
    Eio.Path.save ~create:(`Or_truncate 0o644) (fs / "f") "wtw";
    Hashtbl.replace seen (Eio.Path.load (Crash.post_crash ctl / "f")) ()
  done;
  Alcotest.(check (list string))
    "outcomes"
    (List.sort String.compare [ ""; "dgj"; "dgjutfwss"; "wtw" ])
    (List.sort String.compare (List.of_seq (Hashtbl.to_seq_keys seen)))

(* A file's sync commits the file's mode, not an earlier directory operation
   on another name. *)
let test_crash_sync_commits_the_mode_alone () =
  Eio_mock.Backend.run @@ fun () ->
  let raw = Eio_fs.Fs.v () in
  Eio.Path.save ~create:(`Exclusive 0o644) (raw / "x") "x";
  Eio.Path.save ~create:(`Exclusive 0o644) (raw / "y") "y";
  let ctl = Crash.v ~policy:Drop_unsynced () in
  let fs = Crash.wrap ctl raw in
  Eio.Path.unlink (fs / "x");
  Eio.Path.chmod ~follow:false ~perm:0o755 (fs / "y");
  Eio.Path.with_open_out ~create:`Never (fs / "y") Eio.File.sync;
  let post = Crash.post_crash ctl in
  Alcotest.(check (pair bool int))
    "x kept, y's mode" (true, 0o755)
    ( Eio.Path.is_file (post / "x"),
      (Eio.Path.stat ~follow:false (post / "y")).perm )

(* A directory opened, renamed, and another made at its old name: syncing
   the handle commits the directory it opened, not the one at that name. *)
let test_crash_dir_handle_barrier_by_inode () =
  Eio_mock.Backend.run @@ fun () ->
  let raw = Eio_fs.Fs.v () in
  List.iter (fun d -> Eio.Path.mkdir ~perm:0o755 (raw / d)) [ "a"; "b"; "a/d" ];
  let ctl = Crash.v () in
  let fs = Crash.wrap ctl raw in
  Eio.Switch.run @@ fun sw ->
  let h = Eio.Path.open_in ~sw (fs / "a/d") in
  Eio.Path.rename (fs / "a/d") (fs / "b/d");
  Eio.Path.mkdir ~perm:0o755 (fs / "a/d");
  Eio.Path.sync_dir (fs / "a");
  Eio.Path.sync_dir (fs / "b");
  Eio.Path.save ~create:(`Exclusive 0o644) (fs / "b/d/x") "x";
  Eio.Path.save ~create:(`Exclusive 0o644) (fs / "a/d/y") "y";
  (match Eio_fs_sync.file_barrier h with
  | Some barrier -> barrier ~full:false
  | None -> Alcotest.fail "no barrier");
  let post = Crash.post_crash ctl in
  Alcotest.(check (pair bool bool))
    "b/d/x kept, a/d/y lost" (true, false)
    (Eio.Path.is_file (post / "b/d/x"), Eio.Path.is_file (post / "a/d/y"))

(* A barrier on a new directory, deferred until the directory is durable:
   wherever the cut falls, the directory never shows without the file the
   barrier covered. *)
let test_crash_deferred_barrier_whole () =
  for cut = 0 to 6 do
    Eio_mock.Backend.run @@ fun () ->
    let raw = Eio_fs.Fs.v () in
    let keep (u : Crash.unsynced) = u.kind = Data || u.seq <= cut in
    let ctl = Crash.v ~policy:(Survive keep) () in
    let fs = Crash.wrap ctl raw in
    Eio.Path.mkdir ~perm:0o755 (fs / "c");
    Eio.Path.mkdir ~perm:0o755 (fs / "d");
    Eio.Path.save ~create:(`Exclusive 0o644) (fs / "d/x") "x";
    Eio.Path.rename (fs / "d/x") (fs / "c/x");
    Eio.Path.sync_dir (fs / "c");
    let post = Crash.post_crash ctl in
    if
      Eio.Path.is_directory (post / "c")
      && not (Eio.Path.is_file (post / "c/x"))
    then Alcotest.failf "cut %d: c without c/x" cut
  done

(* A directory moved into a directory that an earlier pending rename took
   out of it: the barrier commits that rename too, rather than dropping the
   directory into itself. *)
let test_crash_move_needs_ancestor_moves () =
  Eio_mock.Backend.run @@ fun () ->
  let raw = Eio_fs.Fs.v () in
  List.iter (fun d -> Eio.Path.mkdir ~perm:0o755 (raw / d)) [ "a"; "b"; "a/m" ];
  Eio.Path.save ~create:(`Exclusive 0o644) (raw / "a/m/f") "0";
  let ctl = Crash.v () in
  let fs = Crash.wrap ctl raw in
  Eio.Path.rename (fs / "a/m") (fs / "b/m");
  Eio.Path.rename (fs / "a") (fs / "b/m/a");
  Eio.Path.sync_dir fs;
  let image =
    List.map
      (fun (p, _, k) ->
        match k with
        | `Dir -> p ^ "/"
        | `File s -> p ^ "=" ^ s
        | `Link t -> p ^ "->" ^ t)
      (Chaos.Private.Internal.crash_image
         (Crash.crash ctl;
          ctl))
    |> List.sort String.compare
  in
  Alcotest.(check (list string))
    "image"
    [ "b/"; "b/m/"; "b/m/a/"; "b/m/f=0" ]
    image

(* A synced file in a new directory, and a barrier on that directory: the
   directory itself still needs a barrier on its parent. *)
let test_crash_new_dir_needs_parent_barrier () =
  let run ~parent =
    Eio_mock.Backend.run @@ fun () ->
    let raw = Eio_fs.Fs.v () in
    let ctl = Crash.v () in
    let fs = Crash.wrap ctl raw in
    Eio.Path.mkdir ~perm:0o755 (fs / "d");
    write_new ~sync:true (fs / "d/f") "x";
    Eio.Path.sync_dir (fs / "d");
    if parent then Eio.Path.sync_dir fs;
    Eio.Path.is_file (Crash.post_crash ctl / "d/f")
  in
  Alcotest.(check (pair bool bool))
    "without, then with the parent's barrier" (false, true)
    (run ~parent:false, run ~parent:true)

(* A file durable before the wrapper, first opened after an uncommitted
   rename of itself or of a directory above it: what it syncs lands in the
   file the image holds under its old name. *)
let test_crash_preexisting_found_by_inode () =
  let run ~dir policy =
    Eio_mock.Backend.run @@ fun () ->
    let raw = Eio_fs.Fs.v () in
    Eio.Path.mkdir ~perm:0o755 (raw / "d");
    Eio.Path.save ~create:(`Exclusive 0o644) (raw / "d/f") "OLD";
    let ctl = Crash.v ~policy () in
    let fs = Crash.wrap ctl raw in
    let now =
      if dir then (
        Eio.Path.rename (fs / "d") (fs / "e");
        "e/f")
      else (
        Eio.Path.rename (fs / "d/f") (fs / "d/g");
        "d/g")
    in
    Eio.Path.with_open_out ~create:`Never (fs / now) (fun f ->
        Eio.File.pwrite_all f ~file_offset:(i63 0) [ Cstruct.of_string "NEW" ];
        if policy = Crash.Drop_unsynced then Eio.File.sync f);
    Eio.Path.load (Crash.post_crash ctl / "d/f")
  in
  let keep_data = Crash.Survive (fun u -> u.kind = Data) in
  Alcotest.(check (list string))
    "renamed file, renamed directory, then a surviving write"
    [ "NEW"; "NEW"; "NEW" ]
    [
      run ~dir:false Drop_unsynced;
      run ~dir:true Drop_unsynced;
      run ~dir:false keep_data;
    ]

(* A file moved out of a directory, then another directory renamed over it:
   the barrier that commits the rename commits the move that emptied the
   directory it replaces, so the file is not lost with it. *)
let test_crash_rename_over_filled_dir () =
  Eio_mock.Backend.run @@ fun () ->
  let raw = Eio_fs.Fs.v () in
  List.iter (fun d -> Eio.Path.mkdir ~perm:0o755 (raw / d)) [ "a"; "b" ];
  Eio.Path.save ~create:(`Exclusive 0o644) (raw / "b/g") "G";
  let ctl = Crash.v () in
  let fs = Crash.wrap ctl raw in
  Eio.Path.rename (fs / "b/g") (fs / "a/g");
  Eio.Path.rename (fs / "a") (fs / "b");
  Eio.Path.sync_dir fs;
  Alcotest.(check string)
    "b/g" "G"
    (Eio.Path.load (Crash.post_crash ctl / "b/g"))

(* A directory opened, emptied, removed and made again: a barrier through
   the opened directory commits the removed one's own entries, not the new
   one's. *)
let test_crash_subtree_barrier_by_inode () =
  Eio_mock.Backend.run @@ fun () ->
  let raw = Eio_fs.Fs.v () in
  Eio.Path.mkdir ~perm:0o755 (raw / "a");
  Eio.Path.save ~create:(`Exclusive 0o644) (raw / "a/old") "O";
  let ctl = Crash.v () in
  let fs = Crash.wrap ctl raw in
  Eio.Switch.run @@ fun sw ->
  let d = Eio.Path.open_dir ~sw (fs / "a") in
  Eio.Path.unlink (fs / "a/old");
  Eio.Path.rmdir (fs / "a");
  Eio.Path.mkdir ~perm:0o755 (fs / "a");
  Eio.Path.save ~create:(`Exclusive 0o644) (fs / "a/g") "G";
  Eio.Path.sync_dir d;
  let post = Crash.post_crash ctl in
  Alcotest.(check (pair bool bool))
    "a/old and a/g" (false, false)
    (Eio.Path.is_file (post / "a/old"), Eio.Path.is_file (post / "a/g"))

(* A file truncated to nothing, then written past its end: the hole the
   write leaves can come back zeroed, as written back, and not only with the
   bytes the truncation cut. *)
let test_crash_hole_lands_zeroed () =
  let seen = Hashtbl.create 8 in
  for seed = 0 to 199 do
    Eio_mock.Backend.run @@ fun () ->
    let raw = Eio_fs.Fs.v () in
    Eio.Path.save ~create:(`Exclusive 0o644) (raw / "f") "ABCDEFGHIJ";
    let ctl = Crash.v ~policy:(Reorder_unsynced seed) ~block_size:1 () in
    let fs = Crash.wrap ctl raw in
    Eio.Path.with_open_out ~create:`Never (fs / "f") (fun f ->
        Eio.File.truncate f (i63 0);
        Eio.File.pwrite_all f ~file_offset:(i63 2) [ Cstruct.of_string "x" ]);
    Hashtbl.replace seen (Eio.Path.load (Crash.post_crash ctl / "f")) ()
  done;
  Alcotest.(check (pair bool bool))
    "zeroed hole, stale bytes" (true, true)
    (Hashtbl.mem seen "\000\000x", Hashtbl.mem seen "ABx")

(* A user's write clears setgid: a crash keeps the old mode, unless a sync
   of the file committed the clear since. *)
let test_crash_sync_commits_cleared_mode () =
  let run ~commit =
    Eio_mock.Backend.run @@ fun () ->
    let raw = Eio_fs.Fs.v () in
    Eio.Path.save ~create:(`Exclusive 0o644) (raw / "g") "KLM";
    Eio.Path.chown ~follow:false ~uid:1000L ~gid:1000L (raw / "g");
    Eio.Path.chmod ~follow:false ~perm:0o2775 (raw / "g");
    let ctl = Crash.v () in
    let fs = Crash.wrap ctl (Eio_fs.Fs.as_user alice raw) in
    Eio.Path.with_open_out ~create:`Never (fs / "g") (fun f ->
        Eio.File.pwrite_all f ~file_offset:(i63 0) [ Cstruct.of_string "x" ]);
    if commit then
      Eio.Path.with_open_out ~create:`Never (fs / "g") Eio.File.sync;
    (Eio.Path.stat ~follow:false (Crash.post_crash ctl / "g")).perm
  in
  Alcotest.(check (pair int int))
    "without, then with a sync" (0o2775, 0o775)
    (run ~commit:false, run ~commit:true)

(* A chmod of a new directory, then a barrier on it: the chmod is listed as
   unsynced until the directory is durable, and survives with it whatever a
   predicate says. *)
let test_crash_attrs_wait_for_their_node () =
  Eio_mock.Backend.run @@ fun () ->
  let raw = Eio_fs.Fs.v () in
  let keep (u : Crash.unsynced) = u.kind = Mkdir in
  let ctl = Crash.v ~policy:(Survive keep) () in
  let fs = Crash.wrap ctl raw in
  Eio.Path.mkdir ~perm:0o755 (fs / "a");
  Eio.Path.chmod ~follow:false ~perm:0o700 (fs / "a");
  Eio.Path.sync_dir (fs / "a");
  let kinds =
    List.map
      (fun (u : Crash.unsynced) -> Fmt.str "%a" Crash.pp_unsynced u)
      (Crash.unsynced ctl)
  in
  Alcotest.(check int) "mkdir and chmod unsynced" 2 (List.length kinds);
  Alcotest.(check int)
    "mode" 0o700 (Eio.Path.stat ~follow:false (Crash.post_crash ctl / "a")).perm

(* A truncation is metadata: a failed sync does not lose it, and a later
   sync makes it durable, with the bytes the page cache then holds. *)
let test_crash_failed_sync_keeps_truncation () =
  let run init work =
    let ctl = Crash.v ~policy:Drop_unsynced () in
    with_crash ctl @@ fun ~raw:_ ~fs ->
    write_new ~sync:true (fs / "f") init;
    Eio.Path.sync_dir fs;
    let sched = at `Sync "f" (Fail `Eio) in
    let cfs = Chaos.wrap sched fs in
    Eio.Path.with_open_out ~create:`Never (cfs / "f") (fun f ->
        Eio.File.truncate f (i63 0);
        (try Eio.File.sync f with Eio.Io _ -> ());
        work f;
        Eio.File.sync f);
    Eio.Path.load (Crash.post_crash ctl / "f")
  in
  let big = String.make 8192 'A' in
  Alcotest.(check (list string))
    "sync again, regrow, write past the cut"
    [ ""; "\000"; String.make 4096 '\000' ^ "c" ]
    [
      run "AB" ignore;
      run "AB" (fun f -> Eio.File.truncate f (i63 1));
      run big (fun f ->
          Eio.File.pwrite_all f ~file_offset:(i63 4096)
            [ Cstruct.of_string "c" ]);
    ]

(* A record lost to a failed sync, then another in the same page and a sync
   that succeeds: the page cache may have kept the lost record, which lands
   with the new one, or evicted it, so the new one lands over the old bytes. *)
let test_crash_lost_page_kept_or_evicted () =
  let seen = Hashtbl.create 4 in
  for seed = 0 to 199 do
    let ctl = Crash.v ~policy:(Reorder_unsynced seed) () in
    with_crash ctl @@ fun ~raw:_ ~fs ->
    write_new ~sync:true (fs / "f") "";
    Eio.Path.sync_dir fs;
    let sched = at `Sync "f" (Fail `Eio) in
    let cfs = Chaos.wrap sched fs in
    Eio.Path.with_open_out ~create:`Never (cfs / "f") (fun f ->
        Eio.File.pwrite_all f ~file_offset:(i63 0) [ Cstruct.of_string "rec1" ];
        (try Eio.File.sync f with Eio.Io _ -> ());
        Eio.File.pwrite_all f ~file_offset:(i63 4) [ Cstruct.of_string "rec2" ];
        Eio.File.sync f);
    Hashtbl.replace seen (Eio.Path.load (Crash.post_crash ctl / "f")) ()
  done;
  Alcotest.(check (list string))
    "outcomes"
    [ "\000\000\000\000rec2"; "rec1rec2" ]
    (List.sort String.compare (List.of_seq (Hashtbl.to_seq_keys seen)))

(* A user's write clears setuid, which ext4 journals at once: a crash with
   no sync may keep it, and a later barrier that commits anything after it
   carries it. *)
let test_crash_clear_journaled_at_write () =
  let run ~barrier policy =
    Eio_mock.Backend.run @@ fun () ->
    let raw = Eio_fs.Fs.v () in
    Eio.Path.save ~create:(`Exclusive 0o644) (raw / "f") "X";
    Eio.Path.chown ~follow:false ~uid:1000L ~gid:1000L (raw / "f");
    Eio.Path.chmod ~follow:false ~perm:0o4755 (raw / "f");
    Eio.Path.chmod ~follow:false ~perm:0o777 raw;
    let ctl = Crash.v ~policy () in
    let fs = Crash.wrap ctl (Eio_fs.Fs.as_user alice raw) in
    Eio.Path.with_open_out ~create:`Never (fs / "f") (fun f ->
        Eio.File.pwrite_all f ~file_offset:(i63 0) [ Cstruct.of_string "Y" ]);
    if barrier then begin
      Eio.Path.mkdir ~perm:0o755 (fs / "n");
      Eio.Path.sync_dir fs
    end;
    (Eio.Path.stat ~follow:false (Crash.post_crash ctl / "f")).perm
  in
  let keep_all = Crash.Survive (fun _ -> true) in
  Alcotest.(check (list int))
    "no sync, kept; no sync, dropped; a later barrier" [ 0o755; 0o4755; 0o755 ]
    [
      run ~barrier:false keep_all;
      run ~barrier:false Drop_unsynced;
      run ~barrier:true Drop_unsynced;
    ]

(* A truncation a failed sync keeps carries its length only: the bytes in
   its block are the durable ones, not those of a write the sync lost. *)
let test_crash_kept_truncation_length_only () =
  let run policy work =
    let ctl = Crash.v ~policy () in
    with_crash ctl @@ fun ~raw:_ ~fs ->
    write_new ~sync:true (fs / "f") "WXYZ";
    Eio.Path.sync_dir fs;
    let sched = at `Sync "f" (Fail `Eio) in
    let cfs = Chaos.wrap sched fs in
    Eio.Path.with_open_out ~create:`Never (cfs / "f") (fun f ->
        work f;
        (try Eio.File.sync f with Eio.Io _ -> ());
        Eio.File.sync f);
    Eio.Path.load (Crash.post_crash ctl / "f")
  in
  let write f off s =
    Eio.File.pwrite_all f ~file_offset:(i63 off) [ Cstruct.of_string s ]
  and trunc f n = Eio.File.truncate f (i63 n) in
  let keep_all = Crash.Survive (fun _ -> true) in
  Alcotest.(check (list string))
    "write then cut, cut then write, a cut that keeps the length"
    [ "W"; "W"; "W"; "WXYZ" ]
    [
      run Drop_unsynced (fun f ->
          write f 0 "a";
          trunc f 1);
      run keep_all (fun f ->
          write f 0 "a";
          trunc f 1);
      run Drop_unsynced (fun f ->
          trunc f 1;
          write f 0 "a";
          trunc f 1);
      run Drop_unsynced (fun f ->
          write f 0 "a";
          trunc f 4);
    ]

(* A second wrap would reset the durable image and split the files' state
   between two wrappers: it is refused. *)
let test_crash_wrap_once () =
  Eio_mock.Backend.run @@ fun () ->
  let raw = Eio_fs.Fs.v () in
  let ctl = Crash.v () in
  ignore (Crash.wrap ctl raw);
  match Crash.wrap ctl raw with
  | _ -> Alcotest.fail "a second wrap was accepted"
  | exception Invalid_argument _ -> ()

(* A writeback that fails only in part writes the pages that succeeded:
   Reorder_unsynced keeps any subset of the blocks a failed sync lost, and
   Drop_unsynced none. *)
let test_crash_partial_writeback () =
  let run policy =
    let ctl = Crash.v ~policy ~block_size:2 () in
    with_crash ctl @@ fun ~raw:_ ~fs ->
    write_new ~sync:true (fs / "f") "aaaa";
    Eio.Path.sync_dir fs;
    let sched = at `Sync "f" (Fail `Eio) in
    let cfs = Chaos.wrap sched fs in
    Eio.Path.with_open_out ~create:`Never (cfs / "f") (fun f ->
        Eio.File.pwrite_all f ~file_offset:(i63 0) [ Cstruct.of_string "BBCC" ];
        try Eio.File.sync f with Eio.Io _ -> ());
    Eio.Path.load (Crash.post_crash ctl / "f")
  in
  let seen = Hashtbl.create 4 in
  for seed = 0 to 199 do
    Hashtbl.replace seen (run (Reorder_unsynced seed)) ()
  done;
  Alcotest.(check string) "drop" "aaaa" (run Drop_unsynced);
  Alcotest.(check (list string))
    "reorder"
    [ "BBCC"; "BBaa"; "aaCC"; "aaaa" ]
    (List.sort String.compare (List.of_seq (Hashtbl.to_seq_keys seen)))

(* A truncation kept by a failed sync is in the page cache already: a
   second failure leaves the cache as it is, so a later write past its end
   dirties only its own blocks, and the bytes the first failure lost stay
   lost. *)
let test_crash_second_failure () =
  let ctl = Crash.v ~policy:Drop_unsynced ~block_size:2 () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  write_new ~sync:true (fs / "f") "xyz";
  Eio.Path.sync_dir fs;
  let sched = fail_syncs in
  let cfs = Chaos.wrap sched fs in
  Eio.Path.with_open_out ~create:`Never (fs / "f") (fun f ->
      Eio.File.truncate f (i63 1);
      Eio.File.pwrite_all f ~file_offset:(i63 0) [ Cstruct.of_string "bb" ]);
  for _ = 1 to 2 do
    try Eio.Path.with_open_out ~create:`Never (cfs / "f") Eio.File.sync
    with Eio.Io _ -> ()
  done;
  Eio.Path.with_open_out ~create:`Never (fs / "f") (fun f ->
      Eio.File.pwrite_all f ~file_offset:(i63 3) [ Cstruct.of_string "e" ];
      Eio.File.sync f);
  Alcotest.(check string)
    "f" "x\000\000e"
    (Eio.Path.load (Crash.post_crash ctl / "f"))

(* A page a failed sync lost, then a truncation that cuts it away, made
   durable by a sync: the page never comes back, even when the cut falls on
   a block boundary. *)
let test_crash_durable_cut_frees_lost_pages () =
  let images init steps =
    let seen = Hashtbl.create 8 in
    for seed = 0 to 399 do
      let ctl = Crash.v ~policy:(Reorder_unsynced seed) ~block_size:2 () in
      with_crash ctl @@ fun ~raw:_ ~fs ->
      write_new ~sync:true (fs / "f") init;
      Eio.Path.sync_dir fs;
      let sched = at `Sync "f" (Fail `Eio) in
      let cfs = Chaos.wrap sched fs in
      Eio.Path.with_open_out ~create:`Never (fs / "f") (fun f ->
          List.iter
            (function
              | `W (o, s) ->
                  Eio.File.pwrite_all f ~file_offset:(i63 o)
                    [ Cstruct.of_string s ]
              | `T n -> Eio.File.truncate f (i63 n)
              | `S -> Eio.File.sync f
              | `F -> (
                  try
                    Eio.Path.with_open_out ~create:`Never (cfs / "f")
                      Eio.File.sync
                  with Eio.Io _ -> ()))
            steps);
      Hashtbl.replace seen (Eio.Path.load (Crash.post_crash ctl / "f")) ()
    done;
    List.of_seq (Hashtbl.to_seq_keys seen)
  in
  let a = images "" [ `W (0, "A"); `F; `T 0; `S; `T 1 ] in
  Alcotest.(check bool) "no A" false (List.mem "A" a);
  let b = images "pqr" [ `W (3, "A"); `F; `T 1; `S; `W (1, "BB") ] in
  Alcotest.(check bool)
    "no r at 2" false
    (List.exists (fun s -> String.length s > 2 && s.[2] = 'r') b)

(* A failed sync injected two wrappers above the model reaches it, through
   a read-write and a read-only handle: the next sync does not make the
   lost write durable. *)
let test_crash_stacked_wrappers_relay_failure () =
  let run ~ro =
    let ctl = Crash.v ~policy:Drop_unsynced () in
    Eio_mock.Backend.run @@ fun () ->
    let raw = Eio_fs.Fs.v () in
    Eio.Path.save ~create:(`Exclusive 0o644) (raw / "f") "pqr";
    let mid = Chaos.wrap (fun _ -> None) (Crash.wrap ctl raw) in
    let rule = at `Sync "/f" (Fail `Eio) in
    Eio.Switch.run (fun sw ->
        let h = Eio.Path.open_out ~sw ~create:`Never (mid / "f") in
        Eio.File.pwrite_all h ~file_offset:(i63 0) [ Cstruct.of_string "A" ];
        (if ro then
           Eio.Path.with_open_in
             (Chaos.wrap rule mid / "f")
             (fun g ->
               match Eio_fs_sync.file_barrier g with
               | Some b -> (
                   match b ~full:false with
                   | () -> Alcotest.fail "no failure"
                   | exception Eio.Io _ -> ())
               | None -> Alcotest.fail "no barrier")
         else
           match
             Eio.Path.with_open_out ~create:`Never
               (Chaos.wrap rule mid / "f")
               Eio.File.sync
           with
           | () -> Alcotest.fail "no failure"
           | exception Eio.Io _ -> ());
        Eio.File.sync h);
    Eio.Path.load (Crash.post_crash ctl / "f")
  in
  Alcotest.(check (pair string string))
    "read-write, read-only" ("pqr", "pqr")
    (run ~ro:false, run ~ro:true)

(* The images seeds 0 to 399 leave of [init] after [steps], at blocks of 2
   bytes, a failed sync injected above the model. *)
let lost_images ?(init = "pqr") steps =
  let seen = Hashtbl.create 8 in
  for seed = 0 to 399 do
    let ctl = Crash.v ~policy:(Reorder_unsynced seed) ~block_size:2 () in
    with_crash ctl @@ fun ~raw:_ ~fs ->
    write_new ~sync:true (fs / "f") init;
    Eio.Path.sync_dir fs;
    let sched = fail_syncs in
    let cfs = Chaos.wrap sched fs in
    Eio.Path.with_open_out ~create:`Never (fs / "f") (fun f ->
        List.iter
          (function
            | `W (o, s) ->
                Eio.File.pwrite_all f ~file_offset:(i63 o)
                  [ Cstruct.of_string s ]
            | `T n -> Eio.File.truncate f (i63 n)
            | `F -> (
                try
                  Eio.Path.with_open_out ~create:`Never (cfs / "f")
                    Eio.File.sync
                with Eio.Io _ -> ()))
          steps);
    Hashtbl.replace seen (Eio.Path.load (Crash.post_crash ctl / "f")) ()
  done;
  List.of_seq (Hashtbl.to_seq_keys seen)

(* A write to a page a truncation then cut away, and a failed sync: the
   writeback had no page to write, so with the truncation lost the block
   keeps its bytes on disk. *)
let test_crash_cut_page_not_written () =
  let i = lost_images ~init:"pqrs" [ `W (2, "A"); `T 2; `F ] in
  Alcotest.(check (list string)) "images" [ "pq"; "pqrs" ] (List.sort compare i)

(* The image [Choose] draws for [steps] over a durable [init], grown by a
   synced truncation to [grow] bytes, with each draw made by [draw]. *)
let chosen_image ?grow ~init steps draw =
  let ctl = Crash.v ~policy:(Choose draw) ~block_size:512 () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  write_new ~sync:true (fs / "f") init;
  Eio.Path.sync_dir fs;
  Eio.Path.with_open_out ~create:`Never (fs / "f") (fun f ->
      Option.iter
        (fun n ->
          Eio.File.truncate f (i63 n);
          Eio.File.sync f)
        grow;
      List.iter
        (fun (o, d) ->
          Eio.File.pwrite_all f ~file_offset:(i63 o) [ Cstruct.of_string d ])
        steps);
  Eio.Path.load (Crash.post_crash ctl / "f")

(* Every image [Choose] draws for [steps], enumerated by walking the tree
   of its draws. *)
let chosen_images ?grow ~init steps =
  let seen = Hashtbl.create 8 in
  let rec go prefix =
    let trail = ref [] and pos = ref 0 in
    let draw n =
      let c = if !pos < Array.length prefix then prefix.(!pos) else 0 in
      incr pos;
      trail := (c, n) :: !trail;
      c
    in
    Hashtbl.replace seen (chosen_image ?grow ~init steps draw) ();
    let rec next = function
      | [] -> None
      | (c, n) :: rest when c + 1 < n ->
          Some (Array.of_list (List.rev_map fst rest @ [ c + 1 ]))
      | _ :: rest -> next rest
    in
    Option.iter go (next !trail)
  in
  go [||];
  List.sort compare (List.of_seq (Hashtbl.to_seq_keys seen))

(* A crash leaves a length the file had, or a page boundary a write grew it
   across, as ext4 does. A sector is written whole: in a page the disk held,
   as it was or after a write; in a page it did not, past the end or in a
   hole, also as zeros or stale data, as ext4 may commit the page before its
   bytes. *)
let test_crash_choose_as_ext4 () =
  Alcotest.(check (list string))
    "a write to an empty file"
    [ ""; "\000\000"; "77"; "AB" ]
    (chosen_images ~init:"" [ (0, "AB") ]);
  Alcotest.(check (list string))
    "an append"
    [ "pq"; "pq\000\000"; "pqAB" ]
    (chosen_images ~init:"pq" [ (2, "AB") ]);
  let rng = Random.State.make [| 7 |] in
  let lengths =
    List.init 200 (fun _ ->
        String.length
          (chosen_image ~init:""
             [ (0, String.make 4200 'A') ]
             (fun n -> Random.State.int rng n)))
    |> List.sort_uniq Int.compare
  in
  Alcotest.(check (list int))
    "the lengths of a long write" [ 0; 4096; 4200 ] lengths;
  let hole =
    chosen_images ~grow:8192 ~init:"pq" [ (4096, String.make 1024 'A') ]
  in
  let sectors a b =
    "pq" ^ String.make 4094 '\000' ^ a ^ b ^ String.make 3072 '\000'
  in
  let a = String.make 512 'A' and z = String.make 512 '\000' in
  let s = String.make 512 '7' in
  Alcotest.(check bool)
    "a write into a hole tears, and may show stale data" true
    (List.mem (sectors a z) hole
    && List.mem (sectors s a) hole
    && List.mem (sectors a a) hole);

  Alcotest.(check (list string))
    "an overwrite" [ "pXYs"; "pqrs" ]
    (chosen_images ~init:"pqrs" [ (1, "XY") ])

(* Every image of [new] a cut after [steps] leaves, over a disk that holds
   [old], enumerated by walking the tree of its draws. *)
let images_over_old steps =
  let seen = Hashtbl.create 8 in
  let rec go prefix =
    let trail = ref [] and pos = ref 0 in
    let draw n =
      let c = if !pos < Array.length prefix then prefix.(!pos) else 0 in
      incr pos;
      trail := (c, n) :: !trail;
      c
    in
    let ctl = Crash.v ~policy:(Choose draw) ~block_size:512 () in
    let image =
      Eio_mock.Backend.run @@ fun () ->
      Eio_fs.run @@ fun env ->
      write_new (env#fs / "old") (String.make 512 'o');
      let fs = Crash.wrap ctl env#fs in
      steps ctl fs;
      match Eio.Path.load (Crash.post_crash ctl / "new") with
      | s -> s
      | exception Eio.Io _ -> "-"
    in
    Hashtbl.replace seen image ();
    let rec next = function
      | [] -> None
      | (c, n) :: rest when c + 1 < n ->
          Some (Array.of_list (List.rev_map fst rest @ [ c + 1 ]))
      | _ :: rest -> next rest
    in
    Option.iter go (next !trail)
  in
  go [||];
  List.of_seq (Hashtbl.to_seq_keys seen)

(* A file the disk held when the controller was wrapped, removed, and
   committed by a metadata sync that takes a new snapshot: its blocks are
   free, and a new file's page may hold what it held there. *)
let test_crash_removed_before_snapshot () =
  let images =
    images_over_old (fun _ fs ->
        Eio.Path.unlink (fs / "old");
        sync_dirs fs;
        write_new (fs / "new") (String.make 512 'n'))
  in
  Alcotest.(check bool)
    "new holds what old held" true
    (List.mem (String.make 512 'o') images)

(* A write into a hole: ext4 allocates the whole page and writes it whole,
   so a sector of it the write did not reach, within the length, may hold
   stale data too. *)
let test_crash_hole_page_stale () =
  let images =
    chosen_images ~grow:5632 ~init:"pq" [ (4096, String.make 512 'A') ]
  in
  let sector k s = String.sub s (512 * k) 512 in
  Alcotest.(check bool)
    "a sector of the page the write did not reach is stale" true
    (List.exists
       (fun s ->
         String.length s = 5632
         && sector 8 s = String.make 512 'A'
         && sector 9 s = String.make 512 '7')
       images)

(* A file the disk held before the wrap keeps its holes when the user the
   wrapper runs as cannot search a directory on its path: [a] is closed
   after [b] was opened, and a write through [b] into a hole may still show
   stale data in the sector of its page it did not reach. *)
let test_crash_hole_past_a_closed_directory () =
  let image draw =
    let ctl = Crash.v ~policy:(Choose draw) ~block_size:512 () in
    Eio_mock.Backend.run @@ fun () ->
    let root = Eio_fs.Fs.v () in
    Eio.Path.mkdirs ~perm:0o755 (root / "home/a/b");
    Eio.Path.with_open_out ~create:(`Exclusive 0o644) (root / "home/a/b/f")
      (fun f ->
        Eio.Flow.copy_string "pq" f;
        Eio.File.truncate f (i63 8192));
    List.iter
      (fun p -> Eio.Path.chown ~follow:false ~uid:1000L ~gid:1000L (root / p))
      [ "home"; "home/a"; "home/a/b"; "home/a/b/f" ];
    let fs = Crash.wrap ctl (Eio_fs.Fs.as_user alice root / "home") in
    Eio.Switch.run (fun sw ->
        let b = Eio.Path.open_dir ~sw (fs / "a/b") in
        Eio.Path.chmod ~follow:false ~perm:0o600 (fs / "a");
        Eio.Path.with_open_out ~create:`Never (b / "f") (fun f ->
            Eio.File.pwrite_all f ~file_offset:(i63 4096)
              [ Cstruct.of_string (String.make 512 'A') ]));
    ignore (Crash.post_crash ctl : Eio.Fs.dir_ty Eio.Path.t);
    Eio.Path.load (root / "home/a/b/f")
  in
  let seen = Hashtbl.create 8 in
  let rec go prefix =
    let trail = ref [] and pos = ref 0 in
    let draw n =
      let c = if !pos < Array.length prefix then prefix.(!pos) else 0 in
      incr pos;
      trail := (c, n) :: !trail;
      c
    in
    Hashtbl.replace seen (image draw) ();
    let rec next = function
      | [] -> None
      | (c, n) :: rest when c + 1 < n ->
          Some (Array.of_list (List.rev_map fst rest @ [ c + 1 ]))
      | _ :: rest -> next rest
    in
    Option.iter go (next !trail)
  in
  go [||];
  let stale = String.make 512 '7' in
  let sector k s = String.sub s (512 * k) 512 in
  Alcotest.(check bool)
    "a sector of the hole's page the write did not reach is stale" true
    (Hashtbl.fold
       (fun s () found ->
         found || (String.length s = 8192 && sector 9 s = stale))
       seen false)

(* The images [Choose] draws at random for [steps] over a durable [init],
   each a write or a truncation, with no sync. *)
let sampled_images ~init steps =
  let rng = Random.State.make [| 42 |] in
  let seen = Hashtbl.create 64 in
  for _ = 1 to 400 do
    let ctl =
      Crash.v
        ~policy:(Choose (fun n -> Random.State.int rng n))
        ~block_size:512 ()
    in
    let image =
      with_crash ctl @@ fun ~raw:_ ~fs ->
      write_new ~sync:true (fs / "f") init;
      Eio.Path.sync_dir fs;
      Eio.Path.with_open_out ~create:`Never (fs / "f") (fun f ->
          List.iter
            (function
              | `W (o, d) ->
                  Eio.File.pwrite_all f ~file_offset:(i63 o)
                    [ Cstruct.of_string d ]
              | `T n -> Eio.File.truncate f (i63 n))
            steps);
      Eio.Path.load (Crash.post_crash ctl / "f")
    in
    Hashtbl.replace seen image ()
  done;
  List.of_seq (Hashtbl.to_seq_keys seen)

(* A truncation frees the pages past it only once it commits: at the length
   before it, those pages hold their old bytes, whatever a later write put
   in the blocks they get. *)
let test_crash_truncation_then_reuse () =
  let i =
    sampled_images ~init:(String.make 8192 'a') [ `T 100; `W (5000, "b") ]
  in
  List.iter
    (fun s ->
      if
        String.length s = 8192 && String.sub s 4096 4096 <> String.make 4096 'a'
      then Alcotest.fail "page 1 does not hold its old bytes")
    i;
  let i =
    sampled_images ~init:(String.make 600 'a')
      [ `T 0; `W (0, String.make 100 'b') ]
  in
  List.iter
    (fun s ->
      if String.length s = 600 && s <> String.make 600 'a' then
        Alcotest.fail "the old length does not hold the old bytes")
    i

(* ext4 sets the length on disk as it writes back a page the write put bytes
   in, not the pages of the hole before it. *)
let test_crash_sparse_write_lengths () =
  let i = sampled_images ~init:(String.make 100 'a') [ `W (10000, "b") ] in
  Alcotest.(check (list int))
    "lengths" [ 100; 10001 ]
    (List.sort_uniq Int.compare (List.map String.length i))

(* A truncation zeroes the rest of its 4096-byte block, which the disk may
   write before the new length, and sector by sector. *)
let test_crash_truncation_zeroes_block () =
  let i = sampled_images ~init:(String.make 4096 'a') [ `T 100 ] in
  let zero k s = String.sub s (512 * k) 512 = String.make 512 '\000' in
  Alcotest.(check bool)
    "a sector past the cut zeroed, at the old length" true
    (List.exists
       (fun s -> String.length s = 4096 && zero 3 s && not (zero 5 s))
       i)

(* Two writes into one sector of bytes the file held: a sector is written
   whole, so the second never lands without the first. *)
let test_crash_survive_keeps_a_sector () =
  let image keep =
    let ctl =
      Crash.v ~policy:(Survive (fun u -> List.mem u.file_offset keep)) ()
    in
    with_crash ctl @@ fun ~raw:_ ~fs ->
    write_new ~sync:true (fs / "hdr") (String.make 16 '.');
    Eio.Path.sync_dir fs;
    Eio.Path.with_open_out ~create:`Never (fs / "hdr") (fun f ->
        Eio.File.pwrite_all f ~file_offset:(i63 0) [ Cstruct.of_string "MAGIC" ];
        Eio.File.pwrite_all f ~file_offset:(i63 8) [ Cstruct.of_string "42" ]);
    Eio.Path.load (Crash.post_crash ctl / "hdr")
  in
  Alcotest.(check (list string))
    "the first, the second, both"
    [ "MAGIC..........."; "MAGIC...42......"; "MAGIC...42......" ]
    [ image [ 0 ]; image [ 8 ]; image [ 0; 8 ] ]

(* Two failed writebacks of one block: either may have landed, the second
   also over the page re-read from disk once the first was evicted. *)
let test_crash_each_failure_may_land () =
  let i = lost_images [ `W (0, "A"); `F; `W (1, "B"); `F ] in
  Alcotest.(check (list bool))
    "first, second, second over the evicted page" [ true; true; true ]
    [ List.mem "Aqr" i; List.mem "ABr" i; List.mem "pBr" i ]

(* A lost page, then a write to it and a crash with no sync: the page
   cache may have evicted the lost page, so the write lands over the bytes
   on disk. *)
let test_crash_evicted_before_sync () =
  let i = lost_images [ `W (0, "A"); `F; `W (1, "B") ] in
  Alcotest.(check (list bool))
    "kept, evicted" [ true; true ]
    [ List.mem "ABr" i; List.mem "pBr" i ]

(* A user's write that grows a setuid file, or a truncation of it: the new
   length lands only with the setuid clear before it, as ext4 journals both
   in program order. An overwrite within the length may land without it. *)
let test_crash_length_after_clear () =
  let run op policy =
    Eio_mock.Backend.run @@ fun () ->
    let raw = Eio_fs.Fs.v () in
    Eio.Path.save ~create:(`Exclusive 0o644) (raw / "f") "WW";
    Eio.Path.chmod ~follow:false ~perm:0o4777 (raw / "f");
    let ctl = Crash.v ~policy ~block_size:2 () in
    let fs = Crash.wrap ctl (Eio_fs.Fs.as_user alice raw) in
    Eio.Path.with_open_out ~create:`Never (fs / "f") (fun f ->
        match op with
        | `Grow ->
            Eio.File.pwrite_all f ~file_offset:(i63 0)
              [ Cstruct.of_string "aaaa" ]
        | `Cut -> Eio.File.truncate f (i63 1)
        | `Over ->
            Eio.File.pwrite_all f ~file_offset:(i63 0)
              [ Cstruct.of_string "aa" ]);
    let post = Crash.post_crash ctl in
    (Eio.Path.load (post / "f"), (Eio.Path.stat ~follow:false (post / "f")).perm)
  in
  let policies =
    Crash.Survive (fun u -> u.kind = Data || u.kind = Truncate)
    :: List.init 100 (fun s -> Crash.Reorder_unsynced s)
  in
  List.iter
    (fun op ->
      List.iter
        (fun policy ->
          let data, perm = run op policy in
          if String.length data <> 2 && not (Int.equal perm 0o777) then
            Alcotest.failf "%S with mode %o" data perm)
        policies)
    [ `Grow; `Cut ];
  Alcotest.(check bool)
    "an overwrite without its clear" true
    (List.exists
       (fun p ->
         let data, perm = run `Over p in
         String.equal data "aa" && Int.equal perm 0o4777)
       policies)

(* A fault wrapper between the controller and a user's view: the crash still
   rebuilds the tree with root's rights. *)
let test_crash_over_chaos_over_user () =
  Eio_mock.Backend.run @@ fun () ->
  let raw = Eio_fs.Fs.v () in
  Eio.Path.mkdir ~perm:0o755 (raw / "d");
  Eio.Path.save ~create:(`Exclusive 0o644) (raw / "d" / "f") "x";
  Eio.Path.mkdir ~perm:0o755 (raw / "u");
  Eio.Path.chown ~follow:false ~uid:1000L ~gid:1000L (raw / "u");
  let ctl = Crash.v () in
  let fs =
    Crash.wrap ctl (Chaos.wrap (fun _ -> None) (Eio_fs.Fs.as_user alice raw))
  in
  Eio.Path.save ~create:(`Exclusive 0o644) (fs / "u" / "g") "y";
  let post = Crash.post_crash ctl in
  Alcotest.(check (pair string bool))
    "d/f kept, u/g dropped" ("x", false)
    (Eio.Path.load (post / "d" / "f"), Eio.Path.is_file (post / "u" / "g"))

let suite =
  ( "writeback",
    [
      Alcotest.test_case "crash: a multi-block write" `Quick
        test_crash_multi_block_write;
      Alcotest.test_case "crash: two appends in one block" `Quick
        test_crash_appends_in_one_block;
      Alcotest.test_case "crash: the block size" `Quick test_crash_block_size;
      Alcotest.test_case "crash: a rename over an open file" `Quick
        test_crash_rename_over_an_open_file;
      Alcotest.test_case "crash: a create through a dangling link" `Quick
        test_crash_create_through_a_dangling_link;
      Alcotest.test_case "crash: over a user's view" `Quick
        test_crash_over_a_user_view;
      Alcotest.test_case "crash: owners are restored" `Quick
        test_crash_restores_owners;
      Alcotest.test_case
        "crash: a deferred barrier holds for its directory only" `Quick
        test_crash_deferred_barrier_identity;
      Alcotest.test_case "crash: chmod and chown are journalled" `Quick
        test_crash_chmod_journaled;
      Alcotest.test_case "crash: a synced chmod of a file whose name is pending"
        `Quick test_crash_synced_chmod_of_a_new_file;
      Alcotest.test_case "crash: a synced mode follows the file" `Quick
        test_crash_synced_mode_follows_the_file;
      Alcotest.test_case "crash: files and directories are known by inode"
        `Quick test_crash_inode_identity;
      Alcotest.test_case
        "crash: an entry lands whole, a rename moves the image's node" `Quick
        test_crash_entries_whole;
      Alcotest.test_case "crash: a crash keeps setuid and setgid" `Quick
        test_crash_keeps_privilege_bits;
      Alcotest.test_case "crash: a sync makes a cleared setuid durable" `Quick
        test_crash_sync_records_the_mode;
      Alcotest.test_case "crash: a truncation follows writeback" `Quick
        test_crash_truncation_follows_writeback;
      Alcotest.test_case "crash: a file sync commits its mode alone" `Quick
        test_crash_sync_commits_the_mode_alone;
      Alcotest.test_case
        "crash: a directory handle's barrier is on the inode it opened" `Quick
        test_crash_dir_handle_barrier_by_inode;
      Alcotest.test_case "crash: a deferred barrier lands with what it needs"
        `Quick test_crash_deferred_barrier_whole;
      Alcotest.test_case
        "crash: a rename needs the renames above where it lands" `Quick
        test_crash_move_needs_ancestor_moves;
      Alcotest.test_case
        "crash: a new directory is durable only by its parent's barrier" `Quick
        test_crash_new_dir_needs_parent_barrier;
      Alcotest.test_case
        "crash: a file met after a rename of it is found by inode" `Quick
        test_crash_preexisting_found_by_inode;
      Alcotest.test_case
        "crash: a rename over a directory needs what emptied it" `Quick
        test_crash_rename_over_filled_dir;
      Alcotest.test_case "crash: an opened directory's barrier is on its inode"
        `Quick test_crash_subtree_barrier_by_inode;
      Alcotest.test_case
        "crash: the hole a write past the end leaves can land zeroed" `Quick
        test_crash_hole_lands_zeroed;
      Alcotest.test_case "crash: a sync commits a write's setgid clear" `Quick
        test_crash_sync_commits_cleared_mode;
      Alcotest.test_case
        "crash: a reached chmod stays unsynced until its node is durable" `Quick
        test_crash_attrs_wait_for_their_node;
      Alcotest.test_case "crash: a failed sync keeps a truncation" `Quick
        test_crash_failed_sync_keeps_truncation;
      Alcotest.test_case "crash: a lost page is kept or evicted" `Quick
        test_crash_lost_page_kept_or_evicted;
      Alcotest.test_case
        "crash: a write's setuid clear is journalled at the write" `Quick
        test_crash_clear_journaled_at_write;
      Alcotest.test_case "crash: a kept truncation brings no lost write back"
        `Quick test_crash_kept_truncation_length_only;
      Alcotest.test_case "crash: a controller is wrapped once" `Quick
        test_crash_wrap_once;
      Alcotest.test_case "crash: a failed sync may have written some blocks"
        `Quick test_crash_partial_writeback;
      Alcotest.test_case "crash: a second failed sync keeps the page cache"
        `Quick test_crash_second_failure;
      Alcotest.test_case "crash: a durable cut frees the pages a failure lost"
        `Quick test_crash_durable_cut_frees_lost_pages;
      Alcotest.test_case "crash: stacked wrappers relay a failed sync" `Quick
        test_crash_stacked_wrappers_relay_failure;
      Alcotest.test_case "crash: rebuilds as root beneath a fault wrapper"
        `Quick test_crash_over_chaos_over_user;
      Alcotest.test_case "crash: either failed writeback of a block may land"
        `Quick test_crash_each_failure_may_land;
      Alcotest.test_case "crash: lengths and sectors as ext4 leaves them" `Quick
        test_crash_choose_as_ext4;
      Alcotest.test_case "crash: a hole's page may be stale where not written"
        `Quick test_crash_hole_page_stale;
      Alcotest.test_case
        "crash: a file met past a closed directory keeps its holes" `Quick
        test_crash_hole_past_a_closed_directory;
      Alcotest.test_case "crash: no freed page reused before its truncation"
        `Quick test_crash_truncation_then_reuse;
      Alcotest.test_case "crash: a removal a snapshot folded frees its blocks"
        `Quick test_crash_removed_before_snapshot;
      Alcotest.test_case "crash: a sparse write's lengths" `Quick
        test_crash_sparse_write_lengths;
      Alcotest.test_case "crash: a truncation zeroes the rest of its block"
        `Quick test_crash_truncation_zeroes_block;
      Alcotest.test_case "crash: a sector's overwrites survive together" `Quick
        test_crash_survive_keeps_a_sector;
      Alcotest.test_case "crash: a page cut before a failure is not written"
        `Quick test_crash_cut_page_not_written;
      Alcotest.test_case "crash: a lost page may be evicted before any sync"
        `Quick test_crash_evicted_before_sync;
      Alcotest.test_case "crash: a length lands with the setuid clear before it"
        `Quick test_crash_length_after_clear;
    ] )
