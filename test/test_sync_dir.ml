(* [Eio.Path.sync_dir] as the crash model's directory barrier. *)

module Crash = Eio_fs_chaos.Power

let ( / ) = Eio.Path.( / )

type barrier = No_barrier | By_name | Through_subtree

(* [a] and [b] made in the root, which is barriered after [a] alone; then
   [a/f] made and synced, and [a] barriered as [barrier] says. *)
let cut_after barrier =
  Eio_mock.Backend.run @@ fun () ->
  let t = Crash.v Lose_unsynced in
  let fs = Crash.wrap t (Eio_fs_mem.v ()) in
  Eio.Path.mkdir ~perm:0o755 (fs / "a");
  Eio.Path.sync_dir fs;
  Eio.Path.mkdir ~perm:0o755 (fs / "b");
  Chaos_helpers.write_new ~sync:true (fs / "a/f") "data";
  (match barrier with
  | No_barrier -> ()
  | By_name -> Eio.Path.sync_dir (fs / "a")
  | Through_subtree ->
      Eio.Path.with_open_dir (fs / "a") (fun a -> Eio.Path.sync_dir (a / "")));
  let post = Crash.cut t in
  (Eio.Path.read_dir post, Eio.Path.read_dir (post / "a"))

let test_barrier () =
  let check what expected barrier =
    Alcotest.(check (pair (list string) (list string)))
      what expected (cut_after barrier)
  in
  check "no barrier on a" ([ "a" ], []) No_barrier;
  check "a barrier on a, by name" ([ "a" ], [ "f" ]) By_name;
  check "a barrier on a, through the subtree" ([ "a" ], [ "f" ]) Through_subtree

let suite =
  ( "sync_dir",
    [
      Alcotest.test_case "commits the entries of its directory alone" `Quick
        test_barrier;
    ] )
