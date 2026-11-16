(** Fault-injection tests for {!Eio_fs.Chaos.Private}: each fault kind is pinned
    with a scripted schedule and asserted to occur exactly, and a seeded
    schedule is shown to reproduce deterministically. *)

module Chaos = Eio_fs.Chaos.Private
module Schedule = Chaos.Schedule
module Fault = Chaos.Fault
module Op = Chaos.Op
module Crash = Chaos.Crash
open Chaos_helpers

let ( / ) = Eio.Path.( / )

let expect_io name fn =
  match fn () with
  | () -> Alcotest.failf "%s: expected Eio.Io, got success" name
  | exception Eio.Io _ -> ()

(* {2 Scripted, exact-scenario faults} *)

let test_short_read () =
  let sched =
    Schedule.scripted
      [
        {
          trigger = Op_on_path { kind = `Read; path = "f" };
          fault = Short_read 3;
        };
      ]
  in
  with_chaos sched @@ fun ~raw ~chaos ->
  Eio.Path.save ~create:(`Or_truncate 0o644) (raw / "f") "hello";
  Eio.Path.with_open_in (chaos / "f") @@ fun file ->
  let buf = Cstruct.create 5 in
  let got = Eio.Flow.single_read file buf in
  Alcotest.(check int) "read split to 3 bytes" 3 got;
  Alcotest.(check string)
    "first 3 bytes correct" "hel"
    (Cstruct.to_string (Cstruct.sub buf 0 got))

(* A short read of an empty buffer has nothing to shorten: the read answers
   what the base answers, and the fault's arithmetic raises nothing. *)
let test_short_read_empty () =
  let sched =
    Schedule.scripted
      [
        {
          trigger = Op_on_path { kind = `Read; path = "f" };
          fault = Short_read 3;
        };
      ]
  in
  with_chaos sched @@ fun ~raw ~chaos ->
  Eio.Path.save ~create:(`Or_truncate 0o644) (raw / "f") "hello";
  let answer dir =
    Eio.Path.with_open_in (dir / "f") @@ fun file ->
    match Eio.Flow.single_read file Cstruct.empty with
    | n -> Fmt.str "%d" n
    | exception ((End_of_file | Eio.Io _) as ex) -> Printexc.to_string ex
  in
  let expected = answer raw in
  Alcotest.(check string) "as the base answers" expected (answer chaos);
  Alcotest.(check string)
    "fault consumed once" "scripted(0 rules)"
    (Fmt.str "%a" Schedule.pp sched)

let test_read_error positional () =
  let observed = ref [] in
  let schedule =
    Schedule.scripted
      [
        {
          trigger = Op_on_path { kind = `Read; path = "f" };
          fault = Read_error `Eio;
        };
      ]
  in
  let sched =
    Schedule.observing
      (fun op -> if op.Op.kind = `Read then observed := op :: !observed)
      schedule
  in
  with_chaos sched @@ fun ~raw ~chaos ->
  Eio.Path.save ~create:(`Or_truncate 0o644) (raw / "f") "hello";
  Eio.Path.with_open_in (chaos / "f") @@ fun file ->
  let buf = Cstruct.create 3 in
  let read () =
    if positional then Eio.File.pread file ~file_offset:(i63 1) [ buf ]
    else Eio.Flow.single_read file buf
  in
  expect_io "read failure" (fun () -> ignore (read ()));
  Alcotest.(check string)
    "failure delivers no bytes" "\000\000\000" (Cstruct.to_string buf);
  Alcotest.(check string)
    "read fault consumed once" "scripted(0 rules)"
    (Fmt.str "%a" Schedule.pp schedule);
  (match !observed with
  | [ op ] ->
      Alcotest.(check string) "read retains opened path" "f" op.Op.path;
      Alcotest.(check int)
        "requested read offset"
        (if positional then 1 else 0)
        op.Op.file_offset;
      Alcotest.(check int) "requested read length" 3 op.Op.length
  | _ -> Alcotest.fail "expected exactly one observed read");
  Alcotest.(check int) "next read reaches unchanged data" 3 (read ());
  (match !observed with
  | [ next; _ ] ->
      Alcotest.(check int)
        "failed read preserves scheduled cursor"
        (if positional then 1 else 0)
        next.Op.file_offset
  | _ -> Alcotest.fail "expected exactly two observed reads");
  Alcotest.(check string)
    "failed read preserves streaming cursor"
    (if positional then "ell" else "hel")
    (Cstruct.to_string buf)

let test_seeded_read_error positional () =
  let sched =
    Schedule.seeded ~probability:1. ~kinds:[ `Read_error ] ~seed:29 ()
  in
  with_chaos sched @@ fun ~raw ~chaos ->
  Eio.Path.save ~create:(`Or_truncate 0o644) (raw / "f") "hello";
  Eio.Path.with_open_in (chaos / "f") @@ fun file ->
  let buf = Cstruct.create 3 in
  expect_io "seeded read failure" (fun () ->
      ignore
        (if positional then Eio.File.pread file ~file_offset:(i63 1) [ buf ]
         else Eio.Flow.single_read file buf))

let test_short_write () =
  let sched =
    Schedule.scripted
      [
        {
          trigger = Op_on_path { kind = `Write; path = "f" };
          fault = Short_write 2;
        };
      ]
  in
  with_chaos sched @@ fun ~raw:_ ~chaos ->
  Eio.Path.with_open_out ~create:(`Or_truncate 0o644) (chaos / "f")
  @@ fun file ->
  let got =
    Eio.File.pwrite_single file ~file_offset:(i63 0)
      [ Cstruct.of_string "hello" ]
  in
  Alcotest.(check int) "write accepted only 2 bytes" 2 got;
  Alcotest.(check int)
    "only 2 bytes committed" 2
    (Optint.Int63.to_int (Eio.File.size file))

(* The durability hazard: a torn write commits its prefix to the store and then
   reports failure. Pinned at offset 512 during a 1024-byte "superblock" flush. *)
let test_torn_write () =
  let sched =
    Schedule.scripted
      [
        {
          trigger = Write_covering { path = "sb"; file_offset = 512 };
          fault = Torn_write 512;
        };
      ]
  in
  with_chaos sched @@ fun ~raw ~chaos ->
  let block = Cstruct.create 1024 in
  Cstruct.memset block 0xAB;
  Eio.Path.with_open_out ~create:(`Or_truncate 0o644) (chaos / "sb")
    (fun file ->
      expect_io "torn write" (fun () ->
          Eio.File.pwrite_all file ~file_offset:(i63 0) [ block ]));
  (* 512 bytes reached the store despite the raised error. *)
  let committed = Eio.Path.load (raw / "sb") in
  Alcotest.(check int) "prefix length committed" 512 (String.length committed);
  Alcotest.(check string)
    "prefix bytes committed" (String.make 512 '\xAB') committed

let test_write_error () =
  let sched =
    Schedule.scripted
      [
        {
          trigger = Op_on_path { kind = `Write; path = "f" };
          fault = Write_error `Enospc;
        };
      ]
  in
  with_chaos sched @@ fun ~raw ~chaos ->
  Eio.Path.with_open_out ~create:(`Or_truncate 0o644) (chaos / "f") (fun file ->
      expect_io "enospc" (fun () ->
          Eio.File.pwrite_all file ~file_offset:(i63 0)
            [ Cstruct.of_string "data" ]));
  Alcotest.(check string) "nothing committed" "" (Eio.Path.load (raw / "f"))

let test_sync_error () =
  let sched =
    Schedule.scripted
      [
        {
          trigger = Op_on_path { kind = `Sync; path = "f" };
          fault = Sync_error `Eio;
        };
      ]
  in
  with_chaos sched @@ fun ~raw:_ ~chaos ->
  Eio.Path.with_open_out ~create:(`Or_truncate 0o644) (chaos / "f")
  @@ fun file ->
  (* the write itself is not perturbed *)
  Eio.File.pwrite_all file ~file_offset:(i63 0) [ Cstruct.of_string "data" ];
  expect_io "fsync failure" (fun () -> Eio.File.sync file)

(* fsync(2) flushes a file through any descriptor on it, so a file opened
   read-only answers the barrier too -- a durable publish syncs a read-only
   file that way -- and the schedule fails it as it fails a read-write one. *)
let test_read_only_file_sync () =
  let sched =
    Schedule.scripted
      [
        {
          trigger = Nth_op_on_path { n = 2; kind = `Sync; path = "f" };
          fault = Sync_error `Eio;
        };
      ]
  in
  with_chaos sched @@ fun ~raw ~chaos ->
  Eio.Path.save ~create:(`Exclusive 0o444) (raw / "f") "data";
  Eio.Switch.run @@ fun sw ->
  let file = Eio.Path.open_in ~sw (chaos / "f") in
  match Eio_fs_sync.file_barrier file with
  | None -> Alcotest.fail "a read-only file registers no barrier"
  | Some barrier ->
      barrier ~full:false;
      expect_io "the scheduled sync failure" (fun () -> barrier ~full:false)

(* [Fault.compare] orders faults by constructor, then by argument, and agrees
   with [Fault.equal]. *)
let test_fault_compare () =
  let faults =
    Fault.
      [
        Short_read 1;
        Short_read 2;
        Read_error `Enoent;
        Read_error `Eio;
        Short_write 3;
        Torn_write 0;
        Write_error `Enospc;
        Sync_error `Eio;
        Open_error `Eacces;
        Stat_error `Enoent;
        Swap_symlink "a";
        Swap_symlink "b";
      ]
  in
  List.iteri
    (fun i a ->
      List.iteri
        (fun j b ->
          let c = Fault.compare a b in
          Alcotest.(check int)
            (Fmt.str "%a vs %a" Fault.pp a Fault.pp b)
            (Int.compare i j)
            (if c < 0 then -1 else if c > 0 then 1 else 0);
          Alcotest.(check bool) "agrees with equal" (Fault.equal a b) (c = 0))
        faults)
    faults

let test_open_error () =
  let sched =
    Schedule.scripted
      [
        {
          trigger = Op_on_path { kind = `Open_read; path = "secret" };
          fault = Open_error `Enoent;
        };
      ]
  in
  with_chaos sched @@ fun ~raw ~chaos ->
  Eio.Path.save ~create:(`Or_truncate 0o644) (raw / "secret") "x";
  expect_io "open" (fun () -> ignore (Eio.Path.load (chaos / "secret")))

(* A chmod and a chown change an entry: a fault on [`Change] fails them
   before they are made. *)
let test_change_error_on_chmod_chown () =
  let sched =
    Schedule.hook (fun (op : Op.t) ->
        if op.kind = `Change then Some (Fault.Change_error `Eperm) else None)
  in
  with_chaos sched @@ fun ~raw ~chaos ->
  Eio.Path.save ~create:(`Exclusive 0o644) (raw / "f") "x";
  expect_io "chmod" (fun () ->
      Eio.Path.chmod ~follow:false ~perm:0o600 (chaos / "f"));
  expect_io "chown" (fun () ->
      Eio.Path.chown ~follow:false ~uid:1L ~gid:1L (chaos / "f"));
  let st = Eio.Path.stat ~follow:false (raw / "f") in
  Alcotest.(check (pair int int64))
    "mode and owner" (0o644, 0L) (st.perm, st.uid)

(* An open for reading and one that may write are told apart. *)
let test_open_kinds () =
  let seen = ref [] in
  let sched =
    Schedule.hook (fun (op : Op.t) ->
        (match op.kind with
        | (`Open_read | `Open_write) as k -> seen := (k, op.path) :: !seen
        | _ -> ());
        None)
  in
  with_chaos sched @@ fun ~raw ~chaos ->
  Eio.Path.mkdir ~perm:0o755 (raw / "d");
  Eio.Path.save ~create:(`Exclusive 0o644) (chaos / "f") "x";
  ignore (Eio.Path.load (chaos / "f"));
  Eio.Path.with_open_dir (chaos / "d") ignore;
  let name = function `Open_read -> "read" | `Open_write -> "write" in
  Alcotest.(check (list (pair string string)))
    "opens"
    [ ("write", "f"); ("read", "f"); ("read", "d") ]
    (List.rev_map (fun (k, p) -> (name k, p)) !seen)

let test_descriptor_stat_error writable () =
  let observed = ref [] in
  let schedule =
    Schedule.scripted
      [
        {
          trigger = Op_on_path { kind = `Stat; path = "f" };
          fault = Stat_error `Eacces;
        };
      ]
  in
  let sched =
    Schedule.observing
      (fun op -> if op.Op.kind = `Stat then observed := op.Op.path :: !observed)
      schedule
  in
  with_chaos sched @@ fun ~raw ~chaos ->
  Eio.Path.save ~create:(`Or_truncate 0o644) (raw / "f") "hello";
  Eio.Switch.run @@ fun sw ->
  let file =
    if writable then
      (Eio.Path.open_out ~sw ~create:`Never (chaos / "f")
        :> Eio.File.ro_ty Eio.Resource.t)
    else Eio.Path.open_in ~sw (chaos / "f")
  in
  expect_io "descriptor metadata" (fun () -> ignore (Eio.File.size file));
  Alcotest.(check (list string))
    "one descriptor stat retains its opened path" [ "f" ] (List.rev !observed);
  Alcotest.(check string)
    "one scripted metadata fault was consumed" "scripted(0 rules)"
    (Fmt.str "%a" Schedule.pp schedule);
  Alcotest.(check int)
    "the unchanged descriptor metadata succeeds afterward" 5
    (Optint.Int63.to_int (Eio.File.size file))

let test_stat_error () =
  let sched =
    Schedule.scripted
      [
        {
          trigger = Op_on_path { kind = `Stat; path = "f" };
          fault = Stat_error `Eacces;
        };
      ]
  in
  with_chaos sched @@ fun ~raw ~chaos ->
  Eio.Path.save ~create:(`Or_truncate 0o644) (raw / "f") "x";
  expect_io "stat" (fun () ->
      ignore (Eio.Path.stat ~follow:false (chaos / "f")))

let directory_names entries path =
  if entries then Eio.Path.read_dir_entries path |> List.map snd
  else Eio.Path.read_dir path

let test_directory_error entries () =
  let observed = ref [] in
  let scripted =
    Schedule.scripted
      [
        {
          trigger = Op_on_path { kind = `Read_dir; path = "tree" };
          fault = Read_error `Eacces;
        };
      ]
  in
  let sched =
    Schedule.observing (fun op -> observed := op :: !observed) scripted
  in
  with_chaos sched @@ fun ~raw ~chaos ->
  Eio.Path.mkdir ~perm:0o755 (raw / "tree");
  Eio.Path.save ~create:(`Or_truncate 0o644) (raw / "tree" / "f") "payload";
  let callbacks = ref 0 in
  expect_io "directory refusal" (fun () ->
      if entries then
        Eio.Path.with_dir_entries (chaos / "tree") (fun _ -> incr callbacks)
      else ignore (directory_names false (chaos / "tree")));
  Alcotest.(check int) "no callback before refusal" 0 !callbacks;
  (match !observed with
  | [ op ] ->
      Alcotest.(check string) "directory path" "tree" op.Op.path;
      Alcotest.(check bool) "directory operation" true (op.kind = `Read_dir);
      Alcotest.(check int) "no byte offset" 0 op.file_offset;
      Alcotest.(check int) "no byte length" 0 op.length
  | _ -> Alcotest.fail "expected exactly one directory opportunity");
  Alcotest.(check string)
    "fault consumed once" "scripted(0 rules)"
    (Fmt.str "%a" Schedule.pp scripted);
  Alcotest.(check (list string))
    "subsequent read recovers" [ "f" ]
    (directory_names entries (chaos / "tree"));
  Alcotest.(check string)
    "refusal preserves payload" "payload"
    (Eio.Path.load (raw / "tree" / "f"))

let test_seeded_directory_error entries () =
  let sched =
    Schedule.seeded ~probability:1. ~kinds:[ `Read_error ] ~seed:29 ()
  in
  with_chaos sched @@ fun ~raw ~chaos ->
  Eio.Path.mkdir ~perm:0o755 (raw / "tree");
  expect_io "seeded directory refusal" (fun () ->
      ignore (directory_names entries (chaos / "tree")))

let test_directory_short_read_inapplicable () =
  let sched =
    Schedule.seeded ~probability:1. ~kinds:[ `Short_read ] ~seed:29 ()
  in
  let op : Op.t =
    {
      kind = `Read_dir;
      path = "tree";
      file_offset = 0;
      length = 0;
      space = false;
      dest = None;
    }
  in
  Alcotest.(check bool)
    "no byte fault selected for directory" true
    (Chaos.Internal.next sched op = None);
  with_chaos sched @@ fun ~raw ~chaos ->
  Eio.Path.mkdir ~perm:0o755 (raw / "tree");
  Alcotest.(check (list string))
    "directory read is not a byte read" []
    (directory_names false (chaos / "tree"))

(* An operation longer than [2^30] bytes is split like any other. *)
let test_seeded_short_read_long_op () =
  let sched =
    Schedule.seeded ~probability:1. ~kinds:[ `Short_read ] ~seed:29 ()
  in
  let length = (1 lsl 30) + 1 in
  let op : Op.t =
    {
      kind = `Read;
      path = "f";
      file_offset = 0;
      length;
      space = false;
      dest = None;
    }
  in
  match Chaos.Internal.next sched op with
  | Some (Short_read n) ->
      Alcotest.(check bool)
        "split inside the operation" true
        (n >= 1 && n < length)
  | _ -> Alcotest.fail "no short read for a long operation"

(* A [Swap_symlink] fault reproduces a TOCTOU: the scripted [`Stat] on "victim"
   still returns the original regular file, then the path becomes a symlink to
   the secret, so a follow-up open on the same path -- as a builder that stats
   then opens would do -- resolves to what was swapped in. The check saw one
   thing; the use sees another. *)
let test_swap_symlink () =
  let sched =
    Schedule.scripted
      [
        {
          trigger = Op_on_path { kind = `Stat; path = "victim" };
          fault = Swap_symlink "secret";
        };
      ]
  in
  with_chaos sched @@ fun ~raw ~chaos ->
  Eio.Path.save ~create:(`Or_truncate 0o644) (raw / "victim") "safe";
  Eio.Path.save ~create:(`Or_truncate 0o644) (raw / "secret") "SECRET";
  (* The check: the stat through chaos still sees the original regular file... *)
  let st = Eio.Path.stat ~follow:false (chaos / "victim") in
  Alcotest.(check bool)
    "the checked path is a regular file at stat time" true
    (match st.Eio.File.Stat.kind with `Regular_file -> true | _ -> false);
  (* ...but the fault swapped it underneath: the path is now a symlink to the
     secret, so the use that follows the check reads what was swapped in. *)
  Alcotest.(check bool)
    "the path became a symlink after the stat" true
    (match Eio.Path.kind ~follow:false (raw / "victim") with
    | `Symbolic_link -> true
    | _ -> false);
  Alcotest.(check string)
    "the swapped-in link points at the secret" "secret"
    (Eio.Path.read_link (raw / "victim"))

(* An open leaves the same window a stat does, and the descriptor it hands back
   is what makes it worth exploiting: the caller keeps the file the name meant at
   the open, while the name goes on to mean the target, so everything the caller
   asks the path afterwards -- how long it is, what it holds -- is answered about
   another file. Scripted on the [`Open], the fault pins that window for a
   reader. *)
let test_swap_symlink_on_open_in () =
  let sched =
    Schedule.scripted
      [
        {
          trigger = Op_on_path { kind = `Open_read; path = "current" };
          fault = Swap_symlink "decoy";
        };
      ]
  in
  with_chaos sched @@ fun ~raw ~chaos ->
  Eio.Path.save ~create:(`Or_truncate 0o644) (raw / "log") "the original";
  Eio.Path.save ~create:(`Or_truncate 0o644) (raw / "decoy") "decoy";
  Eio.Path.symlink ~link_to:"log" (raw / "current");
  let read = Eio.Path.with_open_in (chaos / "current") contents in
  Alcotest.(check string)
    "the handle reads the file the name meant at the open" "the original" read;
  Alcotest.(check string)
    "and the name points at the target from then on" "decoy"
    (Eio.Path.read_link (raw / "current"));
  Alcotest.(check string)
    "so the path answers about the other file" "decoy"
    (Eio.Path.load (raw / "current"))

(* The writer's half of the window, which costs more: the bytes go to the file
   the open returned, and the name they were written under has come to mean
   another file, so a reader following the name finds neither them nor the file
   they landed in. *)
let test_swap_symlink_on_open_out () =
  let sched =
    Schedule.scripted
      [
        {
          trigger = Op_on_path { kind = `Open_write; path = "current" };
          fault = Swap_symlink "decoy";
        };
      ]
  in
  with_chaos sched @@ fun ~raw ~chaos ->
  Eio.Path.save ~create:(`Or_truncate 0o644) (raw / "log") "";
  Eio.Path.save ~create:(`Or_truncate 0o644) (raw / "decoy") "decoy";
  Eio.Path.symlink ~link_to:"log" (raw / "current");
  Eio.Path.with_open_out ~create:(`If_missing 0o644) (chaos / "current")
    (fun file ->
      Eio.File.pwrite_all file ~file_offset:(i63 0)
        [ Cstruct.of_string "written" ]);
  Alcotest.(check string)
    "the write lands in the file the name meant at the open" "written"
    (Eio.Path.load (raw / "log"));
  Alcotest.(check string)
    "the file the name means now is untouched" "decoy"
    (Eio.Path.load (raw / "decoy"));
  Alcotest.(check string)
    "and the name points at it" "decoy"
    (Eio.Path.read_link (raw / "current"))

(* A subtree opens the same way, and everything reached through the handle keeps
   resolving under the directory it was opened on while the name it was opened
   under leads elsewhere. *)
let test_swap_symlink_on_open_subtree () =
  let sched =
    Schedule.scripted
      [
        {
          trigger = Op_on_path { kind = `Open_read; path = "current" };
          fault = Swap_symlink "other";
        };
      ]
  in
  with_chaos sched @@ fun ~raw ~chaos ->
  Eio.Path.mkdir ~perm:0o755 (raw / "tree");
  Eio.Path.save ~create:(`Or_truncate 0o644) (raw / "tree" / "f") "in the tree";
  Eio.Path.mkdir ~perm:0o755 (raw / "other");
  Eio.Path.save ~create:(`Or_truncate 0o644) (raw / "other" / "f") "elsewhere";
  Eio.Path.symlink ~link_to:"tree" (raw / "current");
  let read =
    Eio.Path.with_open_dir (chaos / "current") @@ fun dir ->
    Eio.Path.load (dir / "f")
  in
  Alcotest.(check string)
    "the handle reads under the directory the name meant at the open"
    "in the tree" read;
  Alcotest.(check string)
    "and the name points at the target from then on" "other"
    (Eio.Path.read_link (raw / "current"));
  Alcotest.(check string)
    "so the path answers about the other tree" "elsewhere"
    (Eio.Path.load (raw / "current" / "f"))

(* {2 A swap over a real POSIX filesystem}

   Unlinking a directory is EPERM on Darwin and EISDIR on Linux, so freeing a
   name with a plain unlink is portable for a symlink and a regular file and
   nothing else. The swap must free its name kind-aware: clear the entries,
   then [try rmdir with Eio.Io _ -> unlink]. The two directory cases are
   separate on purpose, since a staging directory a store swaps under a caller
   is never empty. *)

(* Run [fn] over a private temp directory on the real filesystem, opened as a
   subtree so the paths the schedule matches are relative to it. *)
let with_real_dir fn =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let name = Filename.temp_file "eio-fs-chaos-swap" "" in
  Eio.Path.unlink (fs / name);
  Eio.Path.mkdir ~perm:0o755 (fs / name);
  Fun.protect ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true (fs / name))
  @@ fun () -> Eio.Path.with_open_dir (fs / name) fn

(* A wrapper carries over none of the backend's own interfaces: a host
   directory behind it is no working directory a child is started in, the
   child being a way around the schedule. *)
let test_real_child_cwd () =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let name = Filename.temp_file "eio-fs-chaos-child" "" in
  Eio.Path.unlink (fs / name);
  Eio.Path.mkdir ~perm:0o755 (fs / name);
  Fun.protect ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true (fs / name))
  @@ fun () ->
  Eio.Path.save ~create:(`Or_truncate 0o644)
    (fs / name / "payload")
    "the child's exact cwd payload\n";
  let chaos = Chaos.wrap Schedule.empty (fs / name) in
  match
    Eio.Process.parse_out
      (Eio.Stdenv.process_mgr env)
      ~cwd:chaos Eio.Buf_read.take_all [ "cat"; "payload" ]
  with
  | got -> Alcotest.failf "a child ran in the wrapped directory: %S" got
  | exception Invalid_argument _ -> ()

let swap_on_open path target =
  Schedule.scripted
    [
      {
        trigger = Op_on_path { kind = `Open_read; path };
        fault = Swap_symlink target;
      };
    ]

(* The name the swap frees is a symlink: the portable case, and the guard rail
   for the fixture -- if this one fails the real-filesystem harness is broken,
   not the directory cases under it. *)
let test_swap_on_real_symlink_subtree () =
  with_real_dir @@ fun raw ->
  let chaos = Chaos.wrap (swap_on_open "current" "other") raw in
  Eio.Path.mkdir ~perm:0o755 (raw / "tree");
  Eio.Path.save ~create:(`Or_truncate 0o644) (raw / "tree" / "f") "in the tree";
  Eio.Path.mkdir ~perm:0o755 (raw / "other");
  Eio.Path.save ~create:(`Or_truncate 0o644) (raw / "other" / "f") "elsewhere";
  Eio.Path.symlink ~link_to:"tree" (raw / "current");
  let read =
    Eio.Path.with_open_dir (chaos / "current") @@ fun dir ->
    Eio.Path.load (dir / "f")
  in
  Alcotest.(check string)
    "real fs: the handle reads under the tree the name meant at the open"
    "in the tree" read;
  Alcotest.(check string)
    "real fs: the name points at the target from then on" "other"
    (Eio.Path.read_link (raw / "current"));
  Alcotest.(check string)
    "real fs: so the path answers about the other tree" "elsewhere"
    (Eio.Path.load (raw / "current" / "f"))

(* An empty true directory. The open completes, the name is a symlink to the
   target afterwards, and every path-based follow-up is answered about the
   target -- the whole point of the fault. *)
let test_swap_on_real_empty_directory_subtree () =
  with_real_dir @@ fun raw ->
  let chaos = Chaos.wrap (swap_on_open "current" "other") raw in
  Eio.Path.mkdir ~perm:0o755 (raw / "current");
  Eio.Path.mkdir ~perm:0o755 (raw / "other");
  Eio.Path.save ~create:(`Or_truncate 0o644) (raw / "other" / "f") "elsewhere";
  Eio.Path.with_open_dir (chaos / "current") (fun (_ : _ Eio.Path.t) -> ());
  Alcotest.(check bool)
    "real fs: the empty directory name became a symlink" true
    (Eio.Path.kind ~follow:false (raw / "current") = `Symbolic_link);
  Alcotest.(check string)
    "real fs: the name points at the target from then on" "other"
    (Eio.Path.read_link (raw / "current"));
  Alcotest.(check string)
    "real fs: so the path answers about the other tree" "elsewhere"
    (Eio.Path.load (raw / "current" / "f"))

(* A populated true directory -- the shape a store's staging directory has, and
   the shape the in-memory subtree case above uses. Freeing this name takes the
   entries with it, so the swap must clear them rather than stop at ENOTEMPTY.
*)
let test_swap_on_real_populated_directory_subtree () =
  with_real_dir @@ fun raw ->
  let chaos = Chaos.wrap (swap_on_open "current" "other") raw in
  Eio.Path.mkdir ~perm:0o755 (raw / "current");
  Eio.Path.save ~create:(`Or_truncate 0o644)
    (raw / "current" / "f")
    "in the tree";
  Eio.Path.mkdir ~perm:0o755 (raw / "other");
  Eio.Path.save ~create:(`Or_truncate 0o644) (raw / "other" / "f") "elsewhere";
  Eio.Path.with_open_dir (chaos / "current") (fun (_ : _ Eio.Path.t) -> ());
  Alcotest.(check bool)
    "real fs: the populated directory name became a symlink" true
    (Eio.Path.kind ~follow:false (raw / "current") = `Symbolic_link);
  Alcotest.(check string)
    "real fs: the name points at the target from then on" "other"
    (Eio.Path.read_link (raw / "current"));
  Alcotest.(check string)
    "real fs: so the path answers about the other tree" "elsewhere"
    (Eio.Path.load (raw / "current" / "f"))

(* {2 Schedule as an inspectable value} *)

(* A scripted rule fires the first time its trigger matches, then is spent. *)
let test_scripted_fires_once () =
  let sched =
    Schedule.scripted [ { trigger = Nth_op 2; fault = Short_read 1 } ]
  in
  let op =
    {
      Op.kind = `Read;
      path = "f";
      file_offset = 0;
      length = 10;
      space = false;
      dest = None;
    }
  in
  let check name expected =
    Alcotest.(check bool)
      name true
      (match (Chaos.Internal.next sched op, expected) with
      | None, None -> true
      | Some a, Some b -> Fault.equal a b
      | _ -> false)
  in
  check "op1 passes through" None;
  check "op2 fires" (Some (Fault.Short_read 1));
  check "op3 passes through (rule spent)" None

(* The third open of "f" fails, and no other operation: not the opens
   before it, not an open of another path between them, not a read of "f",
   and not the open after it, the rule being spent. *)
let test_nth_op_on_path () =
  let sched =
    Schedule.scripted
      [
        {
          trigger = Nth_op_on_path { n = 3; kind = `Open_read; path = "f" };
          fault = Open_error `Enoent;
        };
      ]
  in
  let op kind path =
    { Op.kind; path; file_offset = 0; length = 0; space = false; dest = None }
  in
  let faults =
    List.map
      (fun o -> Option.is_some (Chaos.Internal.next sched o))
      [
        op `Open_read "f";
        op `Open_read "g";
        op `Read "f";
        op `Open_read "f";
        op `Read "f";
        op `Open_read "f";
        op `Open_read "f";
      ]
  in
  Alcotest.(check (list bool))
    "only the third open of f fails"
    [ false; false; false; false; false; true; false ]
    faults

(* {2 Seeded determinism} *)

let ops_fixture =
  let mk kind path file_offset length : Op.t =
    { kind; path; file_offset; length; space = false; dest = None }
  in
  [
    mk `Open_write "a" 0 0;
    mk `Write "a" 0 4096;
    mk `Write "a" 4096 4096;
    mk `Sync "a" 0 0;
    mk `Read "a" 0 8192;
    mk `Stat "b" 0 0;
    mk `Open_write "b" 0 0;
    mk `Write "b" 0 512;
    mk `Read "b" 0 512;
    mk `Sync "b" 0 0;
  ]

let run_schedule seed =
  let s = Schedule.seeded ~seed ~probability:0.7 () in
  List.map (Chaos.Internal.next s) ops_fixture

let test_seed_determinism () =
  let a = run_schedule 1234 and b = run_schedule 1234 in
  Alcotest.(check int) "same length" (List.length a) (List.length b);
  List.iter2
    (fun x y ->
      Alcotest.(check bool)
        "same fault at step" true
        (match (x, y) with
        | None, None -> true
        | Some fx, Some fy -> Fault.equal fx fy
        | _ -> false))
    a b;
  (* non-vacuous: at this probability at least one op must have faulted *)
  Alcotest.(check bool)
    "faults actually occur" true
    (List.exists Option.is_some a)

(* End-to-end determinism over the composed stack: the same seed drives the
   same committed bytes and the same success/failure outcome, and some seed
   perturbs the clean result (so the wrapper really injects). *)
let run_workload schedule =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env ->
  let fs = Chaos.wrap schedule env#fs in
  let outcome =
    try
      Eio.Path.with_open_out ~create:(`Or_truncate 0o644) (fs / "f")
        (fun file ->
          for i = 0 to 9 do
            Eio.File.pwrite_all file
              ~file_offset:(i63 (i * 16))
              [ Fmt.kstr Cstruct.of_string "%016d" i ]
          done;
          Eio.File.sync file);
      "ok"
    with Eio.Io _ -> "io-error"
  in
  (* read back the truly-committed bytes through the un-perturbed base *)
  let committed =
    try Eio.Path.load (env#fs / "f") with Eio.Io _ -> "<none>"
  in
  (outcome, committed)

let test_seed_reproduces () =
  let a = run_workload (Schedule.seeded ~seed:99 ~probability:0.4 ()) in
  let b = run_workload (Schedule.seeded ~seed:99 ~probability:0.4 ()) in
  Alcotest.(check (pair string string)) "same outcome and bytes" a b

(* Permission faults: a directory change or an open for writing refused with
   EACCES, EPERM or EROFS, and nothing changed. *)
let test_change_error () =
  let rule kind path fault =
    { Schedule.trigger = Op_on_path { kind; path }; fault }
  in
  with_chaos
    (Schedule.scripted
       [
         rule `Change "f" (Change_error `Erofs);
         rule `Change "d" (Change_error `Eperm);
         {
           trigger = Nth_op_on_path { n = 2; kind = `Change; path = "f" };
           fault = Change_error `Eacces;
         };
         rule `Open_write "conf" (Open_error `Erofs);
       ])
  @@ fun ~raw ~chaos ->
  Eio.Path.save ~create:(`Exclusive 0o644) (raw / "f") "x";
  expect_io "unlink on a read-only filesystem" (fun () ->
      Eio.Path.unlink (chaos / "f"));
  (match Eio.Path.mkdir ~perm:0o755 (chaos / "d") with
  | () -> Alcotest.fail "mkdir succeeded"
  | exception Eio.Io (Eio.Fs.E (Eio.Fs.Permission_denied _), _) -> ());
  (match Eio.Path.rename (chaos / "f") (chaos / "g") with
  | () -> Alcotest.fail "rename succeeded"
  | exception Eio.Io (Eio.Fs.E (Eio.Fs.Permission_denied _), _) -> ());
  expect_io "open for writing on a read-only filesystem" (fun () ->
      Eio.Path.save ~create:(`Exclusive 0o644) (chaos / "conf") "y");
  Alcotest.(check (list string))
    "nothing changed" [ "f" ] (Eio.Path.read_dir raw)

let test_seeded_change_error () =
  let s = Schedule.seeded ~probability:1. ~kinds:[ `Change_error ] ~seed:3 () in
  Alcotest.(check string)
    "printed"
    "Eio_fs.Chaos.Private.Schedule.seeded ~probability:1. ~kinds:[ \
     `Change_error ] ~seed:3 ()"
    (Fmt.str "%a" Schedule.pp s);
  with_chaos s @@ fun ~raw ~chaos ->
  expect_io "a seeded mkdir" (fun () ->
      Eio.Path.mkdir ~perm:0o755 (chaos / "d"));
  Alcotest.(check (list string)) "nothing made" [] (Eio.Path.read_dir raw)

(* A seeded schedule prints as the expression that rebuilds it. *)
let test_seeded_pp_rebuilds () =
  let s =
    Schedule.seeded ~probability:0.25
      ~kinds:[ `Torn_write; `Sync_error ]
      ~seed:42 ()
  in
  Alcotest.(check string)
    "printed"
    "Eio_fs.Chaos.Private.Schedule.seeded ~probability:0.25 ~kinds:[ \
     `Torn_write; `Sync_error ] ~seed:42 ()"
    (Fmt.str "%a" Schedule.pp s);
  let rebuilt =
    Eio_fs.Chaos.Private.Schedule.seeded ~probability:0.25
      ~kinds:[ `Torn_write; `Sync_error ]
      ~seed:42 ()
  in
  let faults s = List.map (Chaos.Internal.next s) ops_fixture in
  Alcotest.(check bool)
    "same faults" true
    (List.equal (Option.equal Fault.equal) (faults s) (faults rebuilt))

(* A probability prints exactly, so the printed schedule draws the same
   faults. *)
let test_seeded_pp_floats () =
  let printed p =
    Fmt.str "%a" Schedule.pp
      (Schedule.seeded ~probability:p ~kinds:[] ~seed:1 ())
  in
  let expect p lit =
    Alcotest.(check string)
      lit
      ("Eio_fs.Chaos.Private.Schedule.seeded ~probability:" ^ lit
     ^ " ~kinds:[] ~seed:1 ()")
      (printed p)
  in
  expect (0.1 +. 0.2) "0.30000000000000004";
  expect 0.3 "0.3";
  expect 1. "1.";
  expect infinity "infinity";
  expect neg_infinity "neg_infinity";
  expect (-0.5) "(-0.5)";
  expect 1e-300 "1e-300";
  Alcotest.(check bool)
    "the literal is the float" true
    (Float.equal (0.1 +. 0.2) 0.30000000000000004)

let test_chaos_perturbs () =
  let clean = run_workload Schedule.empty in
  Alcotest.(check (pair string string))
    "clean run succeeds fully"
    ( "ok",
      "0000000000000000000000000000000100000000000000020000000000000003000000000000000400000000000000050000000000000006000000000000000700000000000000080000000000000009"
    )
    clean;
  let perturbed =
    List.init 40 (fun seed ->
        run_workload (Schedule.seeded ~seed ~probability:0.5 ()))
  in
  Alcotest.(check bool)
    "some seed perturbs the clean result" true
    (List.exists (fun r -> r <> clean) perturbed)

let suite =
  ( "chaos",
    [
      Alcotest.test_case "short read" `Quick test_short_read;
      Alcotest.test_case "short read of an empty buffer" `Quick
        test_short_read_empty;
      Alcotest.test_case "streaming read error" `Quick (test_read_error false);
      Alcotest.test_case "seeded streaming read error" `Quick
        (test_seeded_read_error false);
      Alcotest.test_case "seeded positional read error" `Quick
        (test_seeded_read_error true);
      Alcotest.test_case "positional read error" `Quick (test_read_error true);
      Alcotest.test_case "short write" `Quick test_short_write;
      Alcotest.test_case "torn write" `Quick test_torn_write;
      Alcotest.test_case "write error" `Quick test_write_error;
      Alcotest.test_case "sync error" `Quick test_sync_error;
      Alcotest.test_case "read-only file sync" `Quick test_read_only_file_sync;
      Alcotest.test_case "fault compare" `Quick test_fault_compare;
      Alcotest.test_case "open error" `Quick test_open_error;
      Alcotest.test_case "chmod and chown are changes" `Quick
        test_change_error_on_chmod_chown;
      Alcotest.test_case "opens for reading and writing" `Quick test_open_kinds;
      Alcotest.test_case "stat error" `Quick test_stat_error;
      Alcotest.test_case "real child cwd" `Quick test_real_child_cwd;
      Alcotest.test_case "directory error" `Quick (test_directory_error false);
      Alcotest.test_case "directory entries error" `Quick
        (test_directory_error true);
      Alcotest.test_case "seeded directory error" `Quick
        (test_seeded_directory_error false);
      Alcotest.test_case "seeded directory entries error" `Quick
        (test_seeded_directory_error true);
      Alcotest.test_case "directory short-read inapplicable" `Quick
        test_directory_short_read_inapplicable;
      Alcotest.test_case "seeded short read of a long operation" `Quick
        test_seeded_short_read_long_op;
      Alcotest.test_case "read-only descriptor stat error" `Quick
        (test_descriptor_stat_error false);
      Alcotest.test_case "read-write descriptor stat error" `Quick
        (test_descriptor_stat_error true);
      Alcotest.test_case "swap-symlink TOCTOU" `Quick test_swap_symlink;
      Alcotest.test_case "swap-symlink on open_in" `Quick
        test_swap_symlink_on_open_in;
      Alcotest.test_case "swap-symlink on open_out" `Quick
        test_swap_symlink_on_open_out;
      Alcotest.test_case "swap-symlink on open_subtree" `Quick
        test_swap_symlink_on_open_subtree;
      Alcotest.test_case "swap on a symlink subtree (real fs)" `Quick
        test_swap_on_real_symlink_subtree;
      Alcotest.test_case "swap on an empty directory subtree (real fs)" `Quick
        test_swap_on_real_empty_directory_subtree;
      Alcotest.test_case "swap on a populated directory subtree (real fs)"
        `Quick test_swap_on_real_populated_directory_subtree;
      Alcotest.test_case "scripted fires once" `Quick test_scripted_fires_once;
      Alcotest.test_case "nth operation on a path" `Quick test_nth_op_on_path;
      Alcotest.test_case "seed determinism" `Quick test_seed_determinism;
      Alcotest.test_case "seed reproduces" `Quick test_seed_reproduces;
      Alcotest.test_case "chaos perturbs" `Quick test_chaos_perturbs;
      Alcotest.test_case "change error" `Quick test_change_error;
      Alcotest.test_case "seeded change error" `Quick test_seeded_change_error;
      Alcotest.test_case "a seeded schedule prints its rebuild" `Quick
        test_seeded_pp_rebuilds;
      Alcotest.test_case "a seeded schedule prints its probability exactly"
        `Quick test_seeded_pp_floats;
    ] )
