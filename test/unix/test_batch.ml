(* A batch is confined to its directory as openat(2) with O_NOFOLLOW resolves
   it one component at a time: no [..], no symbolic link before the last
   component, and every operation answers for itself. *)
module Batch = Eio_fs_unix.Batch
module Chaos = Eio_fs.Chaos.Private

let outside_contents = "outside, must not change"

(* [with_tree fn] makes [base/inside], the batch root, beside [base/outside], a
   file, and [base/outside_dir], an empty directory, and calls [fn] with the
   native base and the open batch. *)
let with_tree fn =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let base = Filename.temp_dir "eio-fs-batch" "" in
  Fun.protect
    ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / base))
    (fun () ->
      Unix.mkdir (Filename.concat base "inside") 0o755;
      Unix.mkdir (Filename.concat base "outside_dir") 0o755;
      Out_channel.with_open_bin (Filename.concat base "outside") (fun oc ->
          output_string oc outside_contents);
      Eio.Switch.run @@ fun sw ->
      let batch = Batch.v ~sw Eio.Path.(fs / Filename.concat base "inside") in
      fn base batch)

let read path = In_channel.with_open_bin path In_channel.input_all

(* The identity of what must not change: the outside file's bytes catch a
   write through an escape, and the outside directory's listing catches a
   creation, a rename or a symbolic link made there. *)
let check_outside base =
  Alcotest.(check string)
    "outside file untouched" outside_contents
    (read (Filename.concat base "outside"));
  Alcotest.(check (array string))
    "outside directory untouched" [||]
    (Sys.readdir (Filename.concat base "outside_dir"));
  Alcotest.(check (list string))
    "nothing new beside the root"
    [ "inside"; "outside"; "outside_dir" ]
    (List.sort compare (Array.to_list (Sys.readdir base)))

let refused message = function
  | Error (`Refused _) -> ()
  | Error (#Batch.error as e) ->
      Alcotest.failf "%s: expected a refusal, got %a" message Batch.pp_error e
  | Ok _ -> Alcotest.failf "%s: expected a refusal, got success" message

let escaping path = Batch.Create { path; perm = 0o644; contents = "escaped" }

let test_dotdot () =
  with_tree @@ fun base batch ->
  let ops =
    [
      escaping "../outside";
      escaping "../escaped";
      escaping "sub/../../escaped";
      Batch.Mkdir { path = "../outside_dir/x"; perm = 0o755 };
      Batch.Symlink { path = "../outside_dir/l"; target = "x" };
      Batch.Rename { src = "../outside"; dst = "stolen" };
      Batch.Link { src = "../outside"; dst = "stolen" };
      Batch.Link { src = "f"; dst = "../outside_dir/aliased" };
      Batch.Unlink { path = "../outside"; dir = false };
      escaping "/tmp/escaped";
      escaping "./a";
      escaping "a//b";
      escaping "";
    ]
  in
  let results = Batch.run batch ops in
  Alcotest.(check int)
    "one result per op" (List.length ops) (List.length results);
  List.iteri (fun i r -> refused (Fmt.str "op %d" i) r) results;
  check_outside base;
  Alcotest.(check (array string))
    "root still empty" [||]
    (Sys.readdir (Filename.concat base "inside"))

let test_symlink_component () =
  with_tree @@ fun base batch ->
  let inside = Filename.concat base "inside" in
  Unix.symlink "../outside_dir" (Filename.concat inside "link");
  Unix.symlink "../outside" (Filename.concat inside "leaf");
  Out_channel.with_open_bin (Filename.concat inside "f") (fun oc ->
      output_string oc "f");
  let results =
    Batch.run batch
      [
        escaping "link/x";
        Batch.Mkdir { path = "link/sub"; perm = 0o755 };
        Batch.Symlink { path = "link/s"; target = "x" };
        Batch.Rename { src = "f"; dst = "link/f" };
        Batch.Link { src = "f"; dst = "link/aliased" };
        Batch.Lstat "link/x";
      ]
  in
  List.iteri (fun i r -> refused (Fmt.str "op %d" i) r) results;
  (* The last component is never followed: creating over a planted link to
     the outside file fails with EEXIST and writes nothing through it. *)
  (match Batch.run batch [ escaping "leaf" ] with
  | [ Error (`Unix (Unix.EEXIST, _, _)) ] -> ()
  | [ Error e ] ->
      Alcotest.failf "leaf: expected EEXIST, got %a" Batch.pp_error e
  | _ -> Alcotest.fail "leaf: expected EEXIST");
  check_outside base

(* [linkat(2)] with no flags links the symbolic link at [src] itself (IEEE Std
   1003.1-2017, linkat), so a planted link to the outside file gains a second
   name while the outside file keeps its one. A regular file beneath the root
   gains a second name sharing its inode. *)
let test_link () =
  with_tree @@ fun base batch ->
  let inside = Filename.concat base "inside" in
  Unix.symlink "../outside" (Filename.concat inside "leaf");
  Out_channel.with_open_bin (Filename.concat inside "f") (fun oc ->
      output_string oc "f");
  (match
     Batch.run batch
       [
         Batch.Link { src = "f"; dst = "g" };
         Batch.Link { src = "leaf"; dst = "leaf2" };
       ]
   with
  | [ Ok Batch.Done; Ok Batch.Done ] -> ()
  | _ -> Alcotest.fail "expected both links to be made");
  let f = Unix.lstat (Filename.concat inside "f")
  and g = Unix.lstat (Filename.concat inside "g") in
  Alcotest.(check int) "g shares f's inode" f.Unix.st_ino g.Unix.st_ino;
  Alcotest.(check int) "f has two names" 2 f.Unix.st_nlink;
  Alcotest.(check bool)
    "leaf2 is the link itself" true
    ((Unix.lstat (Filename.concat inside "leaf2")).Unix.st_kind = Unix.S_LNK);
  Alcotest.(check int)
    "outside file keeps one name" 1
    (Unix.lstat (Filename.concat base "outside")).Unix.st_nlink;
  check_outside base

let kind = function
  | Ok (Batch.Stat st) ->
      Fmt.str "%a" Eio.File.Stat.pp_kind st.Eio.File.Stat.kind
  | Ok Batch.Done -> "done"
  | Error (`Refused _) -> "refused"
  | Error (`Unix (code, _, _)) -> Unix.error_message code

let test_per_op_errors () =
  with_tree @@ fun base batch ->
  let inside = Filename.concat base "inside" in
  let results =
    Batch.run batch
      [
        Batch.Create { path = "a"; perm = 0o640; contents = "abc" };
        Batch.Create { path = "a"; perm = 0o640; contents = "zzzz" };
        Batch.Mkdir { path = "d"; perm = 0o755 };
        Batch.Create { path = "d/b"; perm = 0o751; contents = "hello" };
        Batch.Lstat "missing";
        Batch.Rename { src = "a"; dst = "d/c" };
        Batch.Symlink { path = "d/l"; target = "c" };
        Batch.Lstat "d/l";
        Batch.Unlink { path = "d"; dir = true };
        Batch.Lstat "d/b";
      ]
  in
  let ok = "done" in
  Alcotest.(check (list string))
    "each op answers for itself"
    [
      ok;
      Unix.error_message Unix.EEXIST;
      ok;
      ok;
      Unix.error_message Unix.ENOENT;
      ok;
      ok;
      "symbolic link";
      Unix.error_message Unix.ENOTEMPTY;
      "regular file";
    ]
    (List.map kind results);
  (match List.nth results 9 with
  | Ok (Batch.Stat st) ->
      Alcotest.(check int) "size of d/b" 5 (Optint.Int63.to_int st.size);
      Alcotest.(check int) "perm of d/b, umask not applied" 0o751 st.perm
  | _ -> Alcotest.fail "d/b: expected a stat");
  Alcotest.(check string)
    "the failed create left the first one's bytes" "abc"
    (read (Filename.concat inside "d/c"));
  Alcotest.(check string) "d/b" "hello" (read (Filename.concat inside "d/b"));
  Alcotest.(check string)
    "d/l" "c"
    (Unix.readlink (Filename.concat inside "d/l"));
  check_outside base

let test_one_hop () =
  with_tree @@ fun base batch ->
  let n = 256 in
  let ops =
    Batch.Mkdir { path = "d"; perm = 0o755 }
    :: List.init n (fun i ->
        Batch.Create
          {
            path = Fmt.str "d/f%03d" i;
            perm = 0o644;
            contents = string_of_int i;
          })
  in
  let before = Eio_fs_unix.Private.batch_hops () in
  let results = Batch.run batch ops in
  let after = Eio_fs_unix.Private.batch_hops () in
  Alcotest.(check int) "N + 1 operations, one hop" 1 (after - before);
  Alcotest.(check int)
    "every op succeeded" (n + 1)
    (List.length (List.filter Result.is_ok results));
  Alcotest.(check int)
    "every file written" n
    (Array.length (Sys.readdir (Filename.concat base "inside/d")))

(* A time whose nanoseconds round its sum up to the next second: Eio keeps it
   in its own second, and so must a batch's stat of the same entry. *)
let test_lstat_times_match_eio () =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let base = Filename.temp_dir "eio-fs-batch" "" in
  Fun.protect ~finally:(fun () ->
      Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / base))
  @@ fun () ->
  let file = Filename.concat base "f" in
  Out_channel.with_open_bin file ignore;
  let touched =
    Unix.system
      (Filename.quote_command "touch"
         [ "-d"; "2023-11-14T22:13:20.999999999"; file ])
  in
  Alcotest.(check bool)
    "touch set the time" true
    (match touched with Unix.WEXITED 0 -> true | _ -> false);
  let eio = Eio.Path.stat ~follow:false Eio.Path.(fs / file) in
  Eio.Switch.run @@ fun sw ->
  match Batch.run (Batch.v ~sw Eio.Path.(fs / base)) [ Batch.Lstat "f" ] with
  | [ Ok (Batch.Stat stat) ] ->
      Alcotest.(check (float 0.)) "mtime" eio.mtime stat.mtime;
      Alcotest.(check (float 0.)) "atime" eio.atime stat.atime
  | _ -> Alcotest.fail "lstat failed"

(* {1 Held parents}

   [with_parent] hands its callback the final leaf of a capability path,
   beneath held non-symlink parents, on a native backend and on one with no
   descriptors. Its leaf works with Eio.Path's own operations, and its result
   is whatever the callback returns. *)

let _ : _ Eio.Path.t -> (Eio.Fs.dir_ty Eio.Path.t -> 'a) -> 'a =
  Batch.with_parent

let _ : _ Eio.Path.t -> Eio.Fs.dir_ty Eio.Path.t * string =
 fun path ->
  Batch.with_parent path (fun (leaf : Eio.Fs.dir_ty Eio.Path.t) ->
      Option.get (Eio.Path.split leaf))

let native fn =
  Eio_main.run @@ fun env ->
  let name = Filename.temp_file "held-parent-runtime" "" in
  Sys.remove name;
  let path = Eio.Path.(Eio.Stdenv.fs env / name) in
  Eio.Path.mkdir ~perm:0o700 path;
  Fun.protect
    ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true path)
    (fun () ->
      Eio.Path.with_open_dir path (fun root ->
          fn (root :> Eio.Fs.dir_ty Eio.Path.t)))

let memory fn =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env -> fn env#fs

let require what b = Alcotest.(check bool) what true b

(* What stat says a path is: its device, inode and kind. *)
let identity p =
  let s = Eio.Path.stat ~follow:false p in
  Fmt.str "%Ld:%Ld:%a" s.dev s.ino Eio.File.Stat.pp_kind s.kind

let io f = match f () with () -> false | exception Eio.Io _ -> true

let denied f =
  match f () with
  | () -> false
  | exception Eio.Io (Eio.Fs.E (Eio.Fs.Permission_denied _), _) -> true

(* A leaf used after its callback returned: its borrowed parent is closed. *)
let expired leaf =
  match Eio.Path.load leaf with
  | _ -> false
  | exception (Eio.Io _ | Invalid_argument _) -> true

let ( / ) = Eio.Path.( / )

(* The leaf and the callback's result and exception pass through; the
   borrowed parents go on either exit; malformed paths, missing and blocked
   parents refuse without calling back; a final link stays a link; a renamed
   parent keeps its leaf; a parent swapped between its check and its open is
   refused. *)
let held_parent root =
  Eio.Path.mkdir ~perm:0o700 (root / "parent");
  Eio.Path.save ~create:(`Exclusive 0o600) (root / "parent/leaf") "original";
  let original = identity (root / "parent/leaf") in
  let token = ref "callback return identity" in
  let borrowed = ref None in
  let result =
    Batch.with_parent (root / "parent/leaf") (fun leaf ->
        borrowed := Some leaf;
        Alcotest.(check string) "nested leaf identity" original (identity leaf);
        Alcotest.(check string)
          "nested leaf content" "original" (Eio.Path.load leaf);
        token)
  in
  require "callback result physical identity" (result == token);
  let gone () =
    match !borrowed with
    | None -> Alcotest.fail "callback was not entered"
    | Some leaf -> require "borrowed authority expired" (expired leaf)
  in
  gone ();
  let marker =
    Failure (Bytes.to_string (Bytes.of_string "callback exception identity"))
  in
  require "exception physical identity"
    (match
       Batch.with_parent (root / "parent/leaf") (fun leaf ->
           borrowed := Some leaf;
           raise marker)
     with
    | _ -> false
    | exception (Failure _ as ex) -> ex == marker);
  gone ();
  Alcotest.(check string)
    "caller root authority survives both exits" original
    (identity (root / "parent/leaf"));
  List.iter
    (fun raw ->
      let called = ref false in
      let base, _ = root in
      require
        ("typed invalid path refusal: " ^ String.escaped raw)
        (io (fun () -> Batch.with_parent (base, raw) (fun _ -> called := true)));
      require "invalid path did not call callback" (not !called))
    [
      "";
      "/parent/leaf";
      ".";
      "..";
      "parent/./leaf";
      "parent/../leaf";
      "parent//leaf";
      "parent/leaf\000";
    ];
  Alcotest.(check string)
    "invalid paths preserve original content" "original"
    (Eio.Path.load (root / "parent/leaf"));
  let called = ref false in
  require "missing parent typed Not_found"
    (match
       Batch.with_parent (root / "missing/leaf") (fun _ -> called := true)
     with
    | () -> false
    | exception Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> true);
  require "missing parent no callback" (not !called);
  Eio.Path.save ~create:(`Exclusive 0o600) (root / "blocked") "blocked";
  Eio.Path.mkdir ~perm:0o700 (root / "outside");
  Eio.Path.save ~create:(`Exclusive 0o600) (root / "outside/leaf") "outside";
  let outside = identity (root / "outside/leaf") in
  Eio.Path.symlink ~link_to:"outside" (root / "link");
  List.iter
    (fun name ->
      called := false;
      require "blocked parent typed Permission_denied"
        (denied (fun () ->
             Batch.with_parent
               (root / (name ^ "/leaf"))
               (fun _ -> called := true)));
      require "blocked parent no callback" (not !called))
    [ "blocked"; "link" ];
  Eio.Path.symlink ~link_to:"../outside/leaf" (root / "parent/final-link");
  let link = identity (root / "parent/final-link") in
  Batch.with_parent (root / "parent/final-link") (fun leaf ->
      Alcotest.(check string)
        "final link own lstat identity" link (identity leaf);
      require "final link remains symbolic"
        (match Eio.Path.kind ~follow:false leaf with
        | `Symbolic_link -> true
        | _ -> false));
  Batch.with_parent (root / "parent/leaf") (fun leaf ->
      Eio.Path.rename (root / "parent") (root / "held-original");
      Eio.Path.symlink ~link_to:"outside" (root / "parent");
      Alcotest.(check string)
        "renamed parent retains original leaf identity" original (identity leaf);
      Eio.Path.save ~create:`Never leaf "held write");
  Alcotest.(check string)
    "held write reached original authority" "held write"
    (Eio.Path.load (root / "held-original/leaf"));
  Alcotest.(check string)
    "replacement outside inode survives" outside
    (identity (root / "outside/leaf"));
  Alcotest.(check string)
    "replacement outside content survives" "outside"
    (Eio.Path.load (root / "outside/leaf"));
  Eio.Path.mkdir ~perm:0o700 (root / "observed");
  Eio.Path.save ~create:(`Exclusive 0o600) (root / "observed/leaf") "observed";
  let observed_leaf = identity (root / "observed/leaf") in
  let swapped = ref false in
  let schedule =
    Chaos.Schedule.observing
      (fun (op : Chaos.Op.t) ->
        if
          (op.kind = `Open_read || op.kind = `Open_write)
          && String.equal op.path "observed"
          && not !swapped
        then begin
          swapped := true;
          Eio.Path.rename (root / "observed") (root / "observed-original");
          Eio.Path.mkdir ~perm:0o700 (root / "observed");
          Eio.Path.save ~create:(`Exclusive 0o600) (root / "observed/leaf")
            "replacement"
        end)
      Chaos.Schedule.empty
  in
  let wrapped = Chaos.wrap schedule root in
  called := false;
  require "observed/open identity mismatch denied"
    (denied (fun () ->
         Batch.with_parent (wrapped / "observed/leaf") (fun _ -> called := true)));
  require "deterministic parent-open rendezvous executed" !swapped;
  require "mismatch never enters callback" (not !called);
  Alcotest.(check string)
    "observed original leaf identity preserved" observed_leaf
    (identity (root / "observed-original/leaf"));
  Alcotest.(check string)
    "observed original content preserved" "observed"
    (Eio.Path.load (root / "observed-original/leaf"));
  Alcotest.(check string)
    "replacement content preserved" "replacement"
    (Eio.Path.load (root / "observed/leaf"))

(* A direct child has no parent to traverse, and its leaf is still borrowed
   for the callback alone; the caller's root keeps its authority on both
   exits. *)
let direct_leaf root =
  let original_path = root / "leaf" in
  Eio.Path.save ~create:(`Exclusive 0o600) original_path "original direct leaf";
  let original = identity original_path in
  let captured = ref None in
  let inspect leaf =
    captured := Some leaf;
    Alcotest.(check string)
      "direct callback original dev/ino/kind" original (identity leaf);
    Alcotest.(check string)
      "direct callback original content" "original direct leaf"
      (Eio.Path.load leaf)
  in
  let gone exit =
    match !captured with
    | None -> Alcotest.fail "direct callback was not entered"
    | Some leaf ->
        require
          ("direct borrowed capability expires after " ^ exit)
          (expired leaf)
  in
  let caller_survives exit =
    Alcotest.(check string)
      ("caller root retains original dev/ino/kind after " ^ exit)
      original (identity original_path);
    Alcotest.(check string)
      ("caller root retains original content after " ^ exit)
      "original direct leaf"
      (Eio.Path.load original_path)
  in
  let token = ref [ "arbitrary callback result" ] in
  let result =
    Batch.with_parent original_path (fun leaf ->
        inspect leaf;
        token)
  in
  require "direct callback result physical identity" (result == token);
  gone "normal return";
  caller_survives "normal return";
  captured := None;
  let marker =
    Failure (Bytes.to_string (Bytes.of_string "direct exit token"))
  in
  require "direct callback exception physical identity"
    (match
       Batch.with_parent original_path (fun leaf ->
           inspect leaf;
           raise marker)
     with
    | _ -> false
    | exception (Failure _ as ex) -> ex == marker);
  gone "raised exit";
  caller_survives "raised exit"

(* Over a backend with no descriptors, a batch takes its held root without
   reopening the empty path, and a hard link it refuses leaves the source as
   it was and makes no destination. *)
let test_memory_batch_refuses_a_link () =
  memory @@ fun root ->
  Eio.Path.save ~create:(`Exclusive 0o600) (root / "source") "source";
  let before = identity (root / "source") in
  let empty_open = ref false in
  let schedule =
    Chaos.Schedule.observing
      (fun (op : Chaos.Op.t) ->
        if
          (op.kind = `Open_read || op.kind = `Open_write)
          && String.equal op.path ""
        then empty_open := true)
      Chaos.Schedule.empty
  in
  let wrapped = Chaos.wrap schedule root in
  Eio.Switch.run (fun sw ->
      let batch = Batch.v ~sw wrapped in
      require "the link is refused"
        (match
           Batch.run batch
             [ Batch.Link { src = "source"; dst = "destination" } ]
         with
        | [ Error (`Refused _) ] -> true
        | _ -> false));
  require "the held root is not reopened" (not !empty_open);
  Alcotest.(check string)
    "the source keeps its identity" before
    (identity (root / "source"));
  Alcotest.(check string)
    "the source keeps its content" "source"
    (Eio.Path.load (root / "source"));
  require "no destination"
    (match Eio.Path.stat ~follow:false (root / "destination") with
    | _ -> false
    | exception Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> true)

let suite =
  ( "batch",
    [
      Alcotest.test_case "dot-dot is refused" `Quick test_dotdot;
      Alcotest.test_case "symlink component is refused" `Quick
        test_symlink_component;
      Alcotest.test_case "per-op errors" `Quick test_per_op_errors;
      Alcotest.test_case "a held parent on a native backend" `Quick (fun () ->
          native held_parent);
      Alcotest.test_case "a held parent in memory" `Quick (fun () ->
          memory held_parent);
      Alcotest.test_case "a direct leaf on a native backend" `Quick (fun () ->
          native direct_leaf);
      Alcotest.test_case "a direct leaf in memory" `Quick (fun () ->
          memory direct_leaf);
      Alcotest.test_case "a batch in memory refuses a hard link" `Quick
        test_memory_batch_refuses_a_link;
      Alcotest.test_case "link" `Quick test_link;
      Alcotest.test_case "one hop per batch" `Quick test_one_hop;
      Alcotest.test_case "lstat times match Eio's" `Quick
        test_lstat_times_match_eio;
    ] )
