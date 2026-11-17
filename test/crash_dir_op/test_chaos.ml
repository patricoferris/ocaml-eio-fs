(* A directory operation with no barrier of its own leaves a window: a cut
   right after it rolls it back, and a barrier after it commits it. *)

module Crash = Eio_fs_chaos.Private.Crash
open Chaos_helpers

let ( / ) = Eio.Path.( / )

(* The unlink case, the shape [relay_at_page_size] has: a removal with no
   barrier of its own, cut before the operation whose barrier would commit
   it, rolls back. *)
let test_after_unlink () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  write_new ~sync:true (fs / "a") "A";
  write_new ~sync:true (fs / "b") "B";
  sync_dirs fs;
  Eio.Path.unlink (fs / "a");
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "the unbarriered removal rolled back" "A"
    (Eio.Path.load (post / "a"));
  Alcotest.(check string)
    "the file committed before it is untouched" "B"
    (Eio.Path.load (post / "b"))

(* The same workload cut after the following barrier: the removal is
   durable and the missing barrier is invisible. *)
let test_barrier_hides_the_window () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  write_new ~sync:true (fs / "a") "A";
  write_new ~sync:true (fs / "b") "B";
  sync_dirs fs;
  Eio.Path.unlink (fs / "a");
  write_new ~sync:true (fs / "c") "C";
  sync_dirs fs;
  let post = Crash.post_crash ctl in
  Alcotest.(check bool)
    "the removal committed on the following barrier" true
    (absent (post / "a"));
  Alcotest.(check string)
    "and so did the file created after it" "C"
    (Eio.Path.load (post / "c"))

(* A directory removal reaching no barrier: the directory comes back. *)
let test_after_rmdir () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  Eio.Path.mkdir ~perm:0o755 (fs / "sub");
  sync_dirs fs;
  Eio.Path.rmdir (fs / "sub");
  let post = Crash.post_crash ctl in
  Alcotest.(check bool)
    "the unbarriered removal rolled back" true
    (Eio.Path.kind ~follow:false (post / "sub") = `Directory)

(* A rename reaching no barrier: the tmp-then-rename publish with the barrier
   left out. *)
let test_after_rename () =
  let ctl = Crash.v ~policy:Drop_unsynced () in
  with_crash ctl @@ fun ~raw:_ ~fs ->
  write_new ~sync:true (fs / "x") "data";
  sync_dirs fs;
  Eio.Path.rename (fs / "x") (fs / "y");
  let post = Crash.post_crash ctl in
  Alcotest.(check string)
    "the source is back at its path" "data"
    (Eio.Path.load (post / "x"));
  Alcotest.(check bool)
    "the destination never appeared" true
    (absent (post / "y"))

let suite =
  ( "chaos",
    [
      Alcotest.test_case "after unlink" `Quick test_after_unlink;
      Alcotest.test_case "a barrier hides the window" `Quick
        test_barrier_hides_the_window;
      Alcotest.test_case "after rmdir" `Quick test_after_rmdir;
      Alcotest.test_case "after rename" `Quick test_after_rename;
    ] )
