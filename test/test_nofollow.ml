(* [Eio_fs.Nofollow] over a real POSIX directory, the in-memory backend and
   the [Chaos] wrapper: the final component is never followed, the bound and
   the validation hold before a byte is returned, and a failed read wipes its
   buffer. *)

module Chaos = Eio_fs_chaos

let ( / ) = Eio.Path.( / )

let with_real_dir fn =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let dir = Filename.temp_file "eio-fs-nofollow" "" in
  Sys.remove dir;
  Eio.Path.mkdir ~perm:0o755 Eio.Path.(fs / dir);
  Fun.protect ~finally:(fun () ->
      Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / dir))
  @@ fun () -> fn (Eio.Path.(fs / dir) :> Eio.Fs.dir_ty Eio.Path.t)

let with_in_memory_fs fn =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs_mem.run @@ fun env -> fn env#fs

let regular_file (stat : Eio.File.Stat.t) =
  match stat.kind with
  | `Regular_file -> Ok ()
  | _ -> Error "not a regular file"

exception Cancelled_read

let test_read_nofollow_reads_a_bounded_regular_file () =
  with_real_dir @@ fun dir ->
  Eio.Path.save ~create:(`Exclusive 0o600) (dir / "private") "secret";
  match
    Eio_fs.Nofollow.read_file ~max_bytes:6 ~validate:regular_file
      (dir / "private")
  with
  | Error _ -> Alcotest.fail "a private regular file was refused"
  | Ok bytes ->
      Alcotest.(check string)
        "descriptor bytes" "secret" (Cstruct.to_string bytes);
      Cstruct.memset bytes 0

let test_read_nofollow_refuses_the_final_link () =
  with_real_dir @@ fun dir ->
  Eio.Path.save ~create:(`Exclusive 0o600) (dir / "target") "target";
  Eio.Path.symlink ~link_to:"target" (dir / "private");
  match
    Eio_fs.Nofollow.read_file ~max_bytes:16 ~validate:regular_file
      (dir / "private")
  with
  | Ok bytes ->
      Cstruct.memset bytes 0;
      Alcotest.fail "a final-component symlink was read"
  | Error _ -> Alcotest.fail "a symlink reached validation instead of open"
  | exception Eio.Io _ -> ()

let test_read_nofollow_enforces_the_bound () =
  with_real_dir @@ fun dir ->
  Eio.Path.save ~create:(`Exclusive 0o600) (dir / "private") "three";
  match
    Eio_fs.Nofollow.read_file ~max_bytes:2 ~validate:regular_file
      (dir / "private")
  with
  | Error `Too_large -> ()
  | Error (`Invalid reason) -> Alcotest.fail reason
  | Ok bytes ->
      Cstruct.memset bytes 0;
      Alcotest.fail "an over-bound file was returned"

let test_read_nofollow_returns_descriptor_validation () =
  with_real_dir @@ fun dir ->
  Eio.Path.save ~create:(`Exclusive 0o600) (dir / "private") "secret";
  match
    Eio_fs.Nofollow.read_file ~max_bytes:6
      ~validate:(fun _ -> Error "descriptor policy")
      (dir / "private")
  with
  | Error (`Invalid reason) ->
      Alcotest.(check string) "validation reason" "descriptor policy" reason
  | Error `Too_large -> Alcotest.fail "validation was skipped for the bound"
  | Ok bytes ->
      Cstruct.memset bytes 0;
      Alcotest.fail "descriptor validation was ignored"

let test_read_nofollow_uses_the_in_memory_backend () =
  with_in_memory_fs @@ fun fs ->
  Eio.Path.save ~create:(`Exclusive 0o600) (fs / "target") "target";
  Eio.Path.symlink ~link_to:"target" (fs / "private");
  match
    Eio_fs.Nofollow.read_file ~max_bytes:16 ~validate:regular_file
      (fs / "private")
  with
  | Ok bytes ->
      Cstruct.memset bytes 0;
      Alcotest.fail "the in-memory backend followed a final link"
  | Error _ -> Alcotest.fail "an in-memory link reached validation"
  | exception Eio.Io _ -> ()

let test_read_nofollow_is_one_open_through_its_wrapper () =
  with_real_dir @@ fun dir ->
  Eio.Path.save ~create:(`Exclusive 0o600) (dir / "private") "secret";
  Eio.Path.with_open_dir dir @@ fun base ->
  let observed = ref [] in
  let schedule operation =
    observed := operation :: !observed;
    None
  in
  let wrapped = Chaos.wrap schedule base in
  (match
     Eio_fs.Nofollow.read_file ~max_bytes:6 ~validate:regular_file
       (wrapped / "private")
   with
  | Error _ -> Alcotest.fail "the wrapped private file was refused"
  | Ok bytes -> Cstruct.memset bytes 0);
  let opens =
    List.filter
      (fun (operation : Chaos.Op.t) -> operation.kind = `Open_read)
      !observed
  in
  Alcotest.(check int)
    "the supplied wrapper observes the one open" 1 (List.length opens)

let test_read_nofollow_refuses_the_final_link_beneath_a_capability () =
  with_real_dir @@ fun dir ->
  Eio.Path.save ~create:(`Exclusive 0o600) (dir / "target") "target";
  Eio.Path.symlink ~link_to:"target" (dir / "private");
  Eio.Path.with_open_dir dir @@ fun confined ->
  match
    Eio_fs.Nofollow.read_file ~max_bytes:16 ~validate:regular_file
      (confined / "private")
  with
  | Ok bytes ->
      Cstruct.memset bytes 0;
      Alcotest.fail "a final-component symlink was read beneath a capability"
  | Error _ -> Alcotest.fail "a symlink reached validation instead of open"
  | exception Eio.Io _ -> ()

(* A FIFO with no writer: the open returns and validation refuses it, where a
   blocking open or a read before validation would wait on a writer forever. *)
let test_read_nofollow_validates_a_fifo_before_reading () =
  with_real_dir @@ fun dir ->
  Unix.mkfifo (snd (dir / "private")) 0o600;
  match
    Eio_fs.Nofollow.read_file ~max_bytes:16 ~validate:regular_file
      (dir / "private")
  with
  | Error (`Invalid reason) ->
      Alcotest.(check string) "validation reason" "not a regular file" reason
  | Error `Too_large -> Alcotest.fail "a FIFO was read up to the bound"
  | Ok bytes ->
      Cstruct.memset bytes 0;
      Alcotest.fail "a FIFO was read"

(* [with_regular] opens a regular file of one link and refuses, before any
   open, a symbolic link, a second link and a FIFO. *)
let test_with_regular () =
  with_real_dir @@ fun dir ->
  Eio.Path.save ~create:(`Exclusive 0o600) (dir / "one") "four";
  Unix.symlink (snd (dir / "one")) (snd (dir / "link"));
  Eio.Path.save ~create:(`Exclusive 0o600) (dir / "shared") "";
  Unix.link (snd (dir / "shared")) (snd (dir / "other"));
  Unix.mkfifo (snd (dir / "fifo")) 0o600;
  let size name =
    Eio_fs.Nofollow.with_regular (dir / name) (fun _ stat ->
        Optint.Int63.to_int stat.Eio.File.Stat.size)
    |> Result.map_error (fun (`Invalid why) -> why)
  in
  Alcotest.(check (list (result int string)))
    "the size of the one regular file, the others refused"
    [
      Ok 4;
      Error "a symbolic link, not a regular file";
      Error "a file of 2 links";
      Error "a fifo, not a regular file";
    ]
    (List.map size [ "one"; "link"; "shared"; "fifo" ])

(* A rename over the name between the check and the open's stat leaves the
   opened file with no name: it is read as it was opened, as POSIX keeps an
   open file whose last name went. *)
let test_with_regular_reads_a_replaced_file () =
  with_real_dir @@ fun dir ->
  Eio.Path.save ~create:(`Exclusive 0o600) (dir / "one") "held";
  Eio.Path.save ~create:(`Exclusive 0o600) (dir / "next") "replacement";
  let stats = ref 0 in
  let replace (op : Eio_fs_chaos.Op.t) =
    if op.kind = `Stat && String.ends_with ~suffix:"one" op.path then begin
      incr stats;
      if !stats = 2 then Eio.Path.rename (dir / "next") (dir / "one")
    end;
    None
  in
  let wrapped = Eio_fs_chaos.wrap replace dir in
  let read =
    Eio_fs.Nofollow.with_regular (wrapped / "one") (fun file stat ->
        (Eio.Flow.read_all file, Optint.Int63.to_int stat.Eio.File.Stat.size))
    |> Result.map_error (fun (`Invalid why) -> why)
  in
  Alcotest.(check int) "the name was replaced before the opened stat" 2 !stats;
  Alcotest.(check (result (pair string int) string))
    "the opened file's bytes"
    (Ok ("held", 4))
    read;
  Alcotest.(check string)
    "the name's bytes" "replacement"
    (Eio.Path.load (dir / "one"))

let test_read_nofollow_wipes_a_failed_read () =
  with_real_dir @@ fun dir ->
  Eio.Path.save ~create:(`Exclusive 0o600) (dir / "private") "secret";
  let stat = Eio.Path.stat ~follow:false (dir / "private") in
  let exposed = ref None in
  (match
     Eio_fs_internal.bounded_read ~max_bytes:6 ~validate:regular_file ~stat
       ~read:(fun buffer ->
         Cstruct.blit_from_string "secret" 0 buffer 0 6;
         exposed := Some buffer;
         raise Cancelled_read)
   with
  | exception Cancelled_read -> ()
  | exception failure -> raise failure
  | Ok bytes ->
      Cstruct.memset bytes 0;
      Alcotest.fail "the failed read returned bytes"
  | Error _ -> Alcotest.fail "the fixture did not enter its read");
  match !exposed with
  | None -> Alcotest.fail "the fixture exposed no partial buffer"
  | Some buffer ->
      let wiped = Cstruct.to_string buffer in
      Alcotest.(check string)
        "every byte read before cancellation is wiped"
        (String.make (String.length wiped) '\000')
        wiped

(* The bound is what a file may hold, not what a read costs. A caller bounds an
   untrusted file generously, and a six-byte file read under a one-mebibyte
   bound must not take a mebibyte for it: the buffer offered is the size the
   opened descriptor reports, plus the one byte that tells growth from the end.
   An implementation that allocates the bound up front offers 1048577 here. *)
let test_read_nofollow_costs_the_file_not_the_bound () =
  with_real_dir @@ fun dir ->
  Eio.Path.save ~create:(`Exclusive 0o600) (dir / "small") "secret";
  let stat = Eio.Path.stat ~follow:false (dir / "small") in
  let offered = ref [] in
  let remaining = ref "secret" in
  let read buffer =
    offered := Cstruct.length buffer :: !offered;
    let n = min (Cstruct.length buffer) (String.length !remaining) in
    Cstruct.blit_from_string !remaining 0 buffer 0 n;
    remaining := String.sub !remaining n (String.length !remaining - n);
    n
  in
  (match
     Eio_fs_internal.bounded_read ~max_bytes:(1024 * 1024)
       ~validate:regular_file ~stat ~read
   with
  | Ok bytes ->
      Alcotest.(check string)
        "the small file is read whole" "secret" (Cstruct.to_string bytes)
  | Error _ -> Alcotest.fail "the small file was refused");
  Alcotest.(check int)
    "the largest buffer offered is the file's size plus one" 7
    (List.fold_left max 0 !offered)

(* A file that grows past the size its descriptor reported is still read to the
   bound, so sizing the buffer from the descriptor loses no byte the bound
   admits, and a file growing past the bound is still refused. *)
let test_read_nofollow_follows_growth_to_the_bound () =
  with_real_dir @@ fun dir ->
  Eio.Path.save ~create:(`Exclusive 0o600) (dir / "growing") "ab";
  let stat = Eio.Path.stat ~follow:false (dir / "growing") in
  let feed contents =
    let remaining = ref contents in
    fun buffer ->
      let n = min (Cstruct.length buffer) (String.length !remaining) in
      Cstruct.blit_from_string !remaining 0 buffer 0 n;
      remaining := String.sub !remaining n (String.length !remaining - n);
      n
  in
  let grown = String.make 100 'g' in
  (match
     Eio_fs_internal.bounded_read ~max_bytes:100 ~validate:regular_file ~stat
       ~read:(feed grown)
   with
  | Ok bytes ->
      Alcotest.(check string)
        "growth up to the bound is read" grown (Cstruct.to_string bytes)
  | Error _ -> Alcotest.fail "growth within the bound was refused");
  match
    Eio_fs_internal.bounded_read ~max_bytes:100 ~validate:regular_file ~stat
      ~read:(feed (grown ^ "!"))
  with
  | Ok bytes ->
      Cstruct.memset bytes 0;
      Alcotest.fail "growth past the bound was read"
  | Error `Too_large -> ()
  | Error (`Invalid reason) -> Alcotest.failf "growth was invalid: %s" reason

(* {1 Held parents}

   [with_parent] hands its callback the final leaf of a capability path,
   beneath held non-symlink parents, on a native backend and on one with no
   descriptors. Its leaf works with Eio.Path's own operations, and its result
   is whatever the callback returns. *)

let _ : _ Eio.Path.t -> Eio.Fs.dir_ty Eio.Path.t * string =
 fun path ->
  Eio_fs.Nofollow.with_parent path (fun (leaf : Eio.Fs.dir_ty Eio.Path.t) ->
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
  Eio_fs_mem.run @@ fun env -> fn env#fs

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
    Eio_fs.Nofollow.with_parent (root / "parent/leaf") (fun leaf ->
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
       Eio_fs.Nofollow.with_parent (root / "parent/leaf") (fun leaf ->
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
        (io (fun () ->
             Eio_fs.Nofollow.with_parent (base, raw) (fun _ -> called := true)));
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
       Eio_fs.Nofollow.with_parent (root / "missing/leaf") (fun _ ->
           called := true)
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
             Eio_fs.Nofollow.with_parent
               (root / (name ^ "/leaf"))
               (fun _ -> called := true)));
      require "blocked parent no callback" (not !called))
    [ "blocked"; "link" ];
  Eio.Path.symlink ~link_to:"../outside/leaf" (root / "parent/final-link");
  let link = identity (root / "parent/final-link") in
  Eio_fs.Nofollow.with_parent (root / "parent/final-link") (fun leaf ->
      Alcotest.(check string)
        "final link own lstat identity" link (identity leaf);
      require "final link remains symbolic"
        (match Eio.Path.kind ~follow:false leaf with
        | `Symbolic_link -> true
        | _ -> false));
  Eio_fs.Nofollow.with_parent (root / "parent/leaf") (fun leaf ->
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
  let schedule (op : Chaos.Op.t) =
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
    end;
    None
  in
  let wrapped = Chaos.wrap schedule root in
  called := false;
  require "observed/open identity mismatch denied"
    (denied (fun () ->
         Eio_fs.Nofollow.with_parent (wrapped / "observed/leaf") (fun _ ->
             called := true)));
  require "deterministic parent-open swap executed" !swapped;
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
    Eio_fs.Nofollow.with_parent original_path (fun leaf ->
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
       Eio_fs.Nofollow.with_parent original_path (fun leaf ->
           inspect leaf;
           raise marker)
     with
    | _ -> false
    | exception (Failure _ as ex) -> ex == marker);
  gone "raised exit";
  caller_survives "raised exit"

let suite =
  ( "nofollow",
    [
      Alcotest.test_case "opens only a regular file of one link" `Quick
        test_with_regular;
      Alcotest.test_case "reads a file whose name was replaced" `Quick
        test_with_regular_reads_a_replaced_file;
      Alcotest.test_case "bounded read" `Quick
        test_read_nofollow_reads_a_bounded_regular_file;
      Alcotest.test_case "refuses the final link" `Quick
        test_read_nofollow_refuses_the_final_link;
      Alcotest.test_case "refuses the final link beneath a capability" `Quick
        test_read_nofollow_refuses_the_final_link_beneath_a_capability;
      Alcotest.test_case "enforces the bound" `Quick
        test_read_nofollow_enforces_the_bound;
      Alcotest.test_case "returns descriptor validation" `Quick
        test_read_nofollow_returns_descriptor_validation;
      Alcotest.test_case "uses the in-memory backend" `Quick
        test_read_nofollow_uses_the_in_memory_backend;
      Alcotest.test_case "is one open through a wrapper" `Quick
        test_read_nofollow_is_one_open_through_its_wrapper;
      Alcotest.test_case "validates a FIFO before reading" `Quick
        test_read_nofollow_validates_a_fifo_before_reading;
      Alcotest.test_case "costs the file, not the bound" `Quick
        test_read_nofollow_costs_the_file_not_the_bound;
      Alcotest.test_case "follows growth to the bound" `Quick
        test_read_nofollow_follows_growth_to_the_bound;
      Alcotest.test_case "wipes a failed read" `Quick
        test_read_nofollow_wipes_a_failed_read;
      Alcotest.test_case "a held parent on a native backend" `Quick (fun () ->
          native held_parent);
      Alcotest.test_case "a held parent in memory" `Quick (fun () ->
          memory held_parent);
      Alcotest.test_case "a direct leaf on a native backend" `Quick (fun () ->
          native direct_leaf);
      Alcotest.test_case "a direct leaf in memory" `Quick (fun () ->
          memory direct_leaf);
    ] )
