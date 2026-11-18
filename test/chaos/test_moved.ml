(* What the cut of a controller built by hand keeps of entries the program
   moved, renamed directories included, under directory barriers. Such a
   controller keeps what reached its base other than through the wrapper, so
   its image merges the store's tree with the model's. *)

module P = Eio.Path
module Crash = Eio_fs_chaos.Power
module Model = Eio_fs_chaos.Private.Crash

let keep_all = Crash.Keep (fun _ -> true)

(* The tree below [p]: each entry's path, with a file's bytes and an entry's
   mode. *)
let rec tree p =
  List.sort String.compare (P.read_dir p)
  |> List.concat_map (fun name ->
      let q = P.(p / name) in
      let perm = (P.stat ~follow:false q).perm in
      match P.kind ~follow:false q with
      | `Directory -> Fmt.str "%s/ %o" name perm :: subtree name q
      | _ -> [ Fmt.str "%s %o %S" name perm (P.load q) ])

and subtree name q = List.map (fun s -> name ^ "/" ^ s) (tree q)

(* [run] on a tree setup made durable, then the tree as it stands and the
   tree a cut by [policy] leaves. *)
let cut ?os ?(setup = ignore) policy run =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs_mem.run @@ fun env ->
  setup env#fs;
  let t = Crash.v ?os policy in
  let fs = Crash.wrap t env#fs in
  run fs;
  let live = tree fs in
  (live, tree (Crash.cut t))

let check_live (live, image) =
  Alcotest.(check (list string)) "the image is the live tree" live image

let check_image want (_, image) =
  Alcotest.(check (list string)) "image" want image

let save p v =
  P.with_open_out ~create:(`Exclusive 0o644) p @@ fun f ->
  Eio.Flow.copy_string v f;
  Eio.File.sync f

let fails f = match f () with () -> () | exception Eio.Io _ -> ()

(* [d] holds [f], and [e] is empty. *)
let two_dirs fs =
  P.mkdir ~perm:0o755 P.(fs / "d");
  P.mkdir ~perm:0o755 P.(fs / "e");
  P.save ~create:(`Exclusive 0o644) P.(fs / "d/f") "F"

(* A rename between two durable directories is one change: a barrier on
   either directory commits it whole. *)
let rename_across sync () =
  let run fs =
    P.rename P.(fs / "d/f") P.(fs / "e/f");
    Eio.Path.sync_dir P.(fs / sync)
  in
  let want = [ "d/ 755"; "e/ 755"; "e/f 644 \"F\"" ] in
  check_image want (cut ~setup:two_dirs Lose_unsynced run);
  check_live (cut ~setup:two_dirs keep_all run)

(* A rename into a directory whose own entry is not durable: a barrier on
   the source commits the rename, and the file is in neither directory until
   a barrier on the new directory's parent. *)
let rename_into_new_dir () =
  let setup fs =
    P.mkdir ~perm:0o755 P.(fs / "d");
    P.save ~create:(`Exclusive 0o644) P.(fs / "d/f") "F"
  in
  let run ~parent fs =
    P.mkdir ~perm:0o755 P.(fs / "e");
    P.rename P.(fs / "d/f") P.(fs / "e/f");
    Eio.Path.sync_dir P.(fs / "d");
    if parent then Eio.Path.sync_dir fs
  in
  check_image [ "d/ 755" ] (cut ~setup Lose_unsynced (run ~parent:false));
  check_image
    [ "d/ 755"; "e/ 755"; "e/f 644 \"F\"" ]
    (cut ~setup Lose_unsynced (run ~parent:true));
  check_live (cut ~setup keep_all (run ~parent:false))

(* Every barrier on one directory at a time, then every change kept. *)
let barriers_one_by_one () =
  check_live
    (cut ~setup:two_dirs keep_all (fun fs ->
         P.mkdir ~perm:0o755 P.(fs / "d/x");
         Eio.Path.sync_dir P.(fs / "d/x");
         P.rename P.(fs / "d/f") P.(fs / "d/x/f");
         Eio.Path.sync_dir P.(fs / "e");
         P.rename P.(fs / "d/x") P.(fs / "e/x");
         Eio.Path.sync_dir P.(fs / "d");
         save P.(fs / "e/g") "G";
         Eio.Path.sync_dir fs;
         P.rename P.(fs / "e") P.(fs / "d/e");
         Eio.Path.sync_dir P.(fs / "d")))

(* A file made and synced in a new directory, which is then renamed: the
   file's bytes are durable, so a cut that keeps every change keeps them. *)
let synced_in_moved_dir () =
  check_live
    (cut ~setup:two_dirs keep_all (fun fs ->
         P.mkdir ~perm:0o755 P.(fs / "s");
         save P.(fs / "s/g") "G";
         P.rename P.(fs / "s") P.(fs / "d/s")))

(* An operation that fails on the old name of a renamed directory changes
   nothing a cut keeps. *)
let fails_at_old_name () =
  check_live
    (cut ~setup:two_dirs keep_all (fun fs ->
         P.mkdir ~perm:0o755 P.(fs / "e/x");
         Eio.Path.sync_dir P.(fs / "e");
         P.rename P.(fs / "e") P.(fs / "d/e");
         fails (fun () -> P.rename P.(fs / "e/x") P.(fs / "d/y"))))

(* A mode no sync of the file made durable is lost, whatever barriers commit
   the renames of the file. *)
let mode_of_moved_file () =
  check_image
    [ "d/ 755"; "e/ 755"; "f 644 \"F\"" ]
    (cut ~setup:two_dirs Lose_unsynced (fun fs ->
         P.chmod ~follow:false ~perm:0o600 P.(fs / "d/f");
         P.rename P.(fs / "d/f") P.(fs / "f");
         Eio.Path.sync_dir P.(fs / "d");
         Eio.Path.sync_dir fs))

(* On macOS, fsync(2) of a directory stops in the drive's cache until an
   F_FULLFSYNC drains it: what it drains is that directory's entries, under
   whatever name it has by then. *)
let renamed_before_drain () =
  let setup fs =
    P.mkdir ~perm:0o755 P.(fs / "d");
    P.save ~create:(`Exclusive 0o644) P.(fs / "g") "G"
  in
  check_image
    [ "d/ 755"; "d/f 644 \"F\""; "g 644 \"G\"" ]
    (cut ~os:`Macos ~setup Lose_unsynced (fun fs ->
         P.with_open_out ~create:(`Exclusive 0o644)
           P.(fs / "d/f")
           (fun f ->
             Eio.Flow.copy_string "F" f;
             Full.file f);
         Eio.Path.sync_dir P.(fs / "d");
         P.rename P.(fs / "d") P.(fs / "e");
         P.with_open_out ~create:`Never P.(fs / "g") Full.file))

(* A write and a truncation, then renames of the file and of its
   directory: each change names the file as it was named when it was made. *)
let named_when_made () =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs_mem.run @@ fun env ->
  let t = Model.v ~policy:Drop_unsynced () in
  let fs = Model.wrap t env#fs in
  P.mkdir ~perm:0o755 P.(fs / "d");
  P.with_open_out ~create:(`Exclusive 0o644)
    P.(fs / "d/f")
    (fun f ->
      Eio.Flow.copy_string "vv" f;
      P.rename P.(fs / "d/f") P.(fs / "d/g");
      Eio.File.truncate f (Optint.Int63.of_int 1));
  P.rename P.(fs / "d") P.(fs / "e");
  let changes =
    List.filter_map
      (fun (u : Model.unsynced) ->
        match u.kind with
        | Data | Truncate -> Fmt.kstr Option.some "%a" Model.pp_unsynced u
        | _ -> None)
      (Model.unsynced t)
  in
  Alcotest.(check (list string))
    "changes"
    [ "d/f @0+2 #3"; "truncate d/g to 1 #5" ]
    changes

let suite =
  ( "moved",
    [
      Alcotest.test_case "a rename commits with its destination's barrier"
        `Quick (rename_across "e");
      Alcotest.test_case "a rename commits with its source's barrier" `Quick
        (rename_across "d");
      Alcotest.test_case "a rename into a new directory" `Quick
        rename_into_new_dir;
      Alcotest.test_case "every change kept after barriers one by one" `Quick
        barriers_one_by_one;
      Alcotest.test_case "a synced file in a renamed directory keeps its bytes"
        `Quick synced_in_moved_dir;
      Alcotest.test_case "a failure at a renamed directory's old name" `Quick
        fails_at_old_name;
      Alcotest.test_case "a moved file's mode needs its sync" `Quick
        mode_of_moved_file;
      Alcotest.test_case "macOS drains a directory's fsync under a new name"
        `Quick renamed_before_drain;
      Alcotest.test_case "a change names its file as it was when made" `Quick
        named_when_made;
    ] )
