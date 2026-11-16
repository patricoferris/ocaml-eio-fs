(* The in-memory filesystem against Linux and eio_posix. A stat answers
   times, link counts and block counts as Linux does: the expected answers
   are recorded in stat/traces/linux.csv, as stat/README.md says. *)

let ( / ) = Eio.Path.( / )

(* A clock that moves only when the test moves it. *)
module Manual = struct
  type t = float ref
  type time = float

  let now t = !t
  let sleep_until _ _ = invalid_arg "Manual.sleep_until"
end

let manual () =
  let t = ref 0. in
  (t, Eio.Resource.T (t, Eio.Time.Pi.clock (module Manual)))

let with_fs fn =
  Eio_mock.Backend.run @@ fun () ->
  let t, clock = manual () in
  fn t (Eio_fs.Fs.v ~clock ())

let stat p = Eio.Path.stat ~follow:false p
let trace = "stat/traces/linux.csv"

(* The filesystems the trace names, and its rows: a workload, then its line
   on each filesystem. *)
let linux =
  lazy
    (match
       In_channel.with_open_text trace In_channel.input_lines
       |> List.filter (fun l -> not (String.starts_with ~prefix:"#" l))
       |> List.map (String.split_on_char '|')
     with
    | ("workload" :: fss) :: rows -> (fss, rows)
    | _ -> Fmt.failwith "%s: no header" trace)

(* What the workload [name] answers on the filesystem [fs]. *)
let answer name fs =
  let fss, rows = Lazy.force linux in
  match List.find_index (String.equal fs) fss with
  | None -> Alcotest.failf "%s records nothing on %s: regenerate it" trace fs
  | Some i ->
      List.filter_map
        (function w :: l when w = name -> Some (List.nth l i) | _ -> None)
        rows

(* The workload [name] on the filesystem [root]; each tick is a second. *)
let run name t root =
  (List.assoc name Stat_workload.all)
    ~tick:(fun () -> t := !t +. 1.)
    (root :> Eio.Fs.dir_ty Eio.Path.t)

let check name ~on got =
  List.iter
    (fun fs ->
      Alcotest.(check (list string)) ("as on " ^ fs) (answer name fs) got)
    on

let as_linux name ~on () =
  with_fs @@ fun t root -> check name ~on (run name t root)

(* Directories hold no blocks on tmpfs and one on ext4: the counts follow
   tmpfs. *)
let test_counts () =
  with_fs @@ fun t root ->
  check "counts" ~on:[ "tmpfs" ] (run "counts" t root);
  let in_d =
    List.filter_map
      (fun l ->
        Scanf.sscanf l "%s nlink=%_d blocks=%Ld" (fun n b ->
            if n = "d" || String.starts_with ~prefix:"d/" n then Some b
            else None))
      (answer "counts" "tmpfs")
  in
  Alcotest.(check int64)
    "usage"
    (Int64.mul 512L (List.fold_left Int64.add 0L in_d))
    (Eio_fs.Path.usage (root / "d")).allocated_bytes

let test_trace () =
  Provenance.check ~trace
    ~keys:
      [
        "script";
        "script revision";
        "script digest";
        "recorded";
        "linux os";
        "linux image";
        "linux user";
        "linux eio_posix";
        "tmpfs";
        "ext4";
      ]
    ~code:[ "stat/scripts/record.ml"; "stat/workload/stat_workload.ml" ]
    ~script:"stat/scripts/generate.sh"

(* atime moves once a day even past mtime and ctime; not recorded, as it
   takes a day. *)
let test_relatime_day () =
  with_fs @@ fun t root ->
  let f = root / "f" in
  Eio.Path.save ~create:(`Exclusive 0o644) f "x";
  t := 1.;
  ignore (Eio.Path.load f);
  t := 2.;
  ignore (Eio.Path.load f);
  Alcotest.(check (float 0.)) "not again" 1. (stat f).atime;
  t := 86401.;
  ignore (Eio.Path.load f);
  Alcotest.(check (float 0.)) "a day later" 86401. (stat f).atime

(* Without a clock, time stands at 0. *)
let test_no_clock () =
  Eio_mock.Backend.run @@ fun () ->
  let root = Eio_fs.Fs.v () in
  Eio.Path.save ~create:(`Exclusive 0o644) (root / "f") "x";
  Eio.Path.chmod ~follow:false ~perm:0o600 (root / "f");
  let s = stat (root / "f") in
  Alcotest.(check (list (float 0.)))
    "times" [ 0.; 0.; 0. ]
    [ s.atime; s.mtime; s.ctime ]

(* make(1)'s rule: a target older than its source is rebuilt. *)
let build fs =
  let src = fs / "src" and out = fs / "out" in
  let fresh =
    match stat out with
    | o -> (stat src).mtime <= o.mtime
    | exception Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> false
  in
  if not fresh then
    Eio.Path.save ~create:(`Or_truncate 0o644) out
      (String.uppercase_ascii (Eio.Path.load src))

(* A source edited after its target is built is seen newer, under faults:
   an error ends the run, which then checks nothing. *)
let test_freshness_under_check () =
  Eio_fs.Chaos.check
    ~setup:(fun env ->
      Eio.Path.save ~create:(`Exclusive 0o644) (env#fs / "src") "a")
    (fun env ->
      match
        build env#fs;
        Eio.Time.sleep env#clock 1.;
        Eio.Path.save ~create:(`Or_truncate 0o644) (env#fs / "src") "b";
        build env#fs;
        Eio.Path.load (env#fs / "out")
      with
      | "B" -> ()
      | got -> Fmt.failwith "out holds %S, built from a stale source" got
      | exception Eio.Io _ -> ())

(* A power cut leaves each inode's times as its version on the disk holds
   them: a file's at its last sync, a directory's at the last change to it
   the disk kept. *)
let test_crash_times () =
  with_fs @@ fun t root ->
  let module Crash = Eio_fs.Chaos.Crash in
  t := 1.;
  Eio.Path.save ~create:(`Exclusive 0o644) (root / "f") "a";
  let c = Crash.v Drop_unsynced in
  let d = Crash.wrap c root in
  let times p =
    let s = stat p in
    [ s.mtime; s.ctime ]
  in
  Eio.Path.with_open_out ~create:`Never (d / "f") (fun h ->
      t := 2.;
      Eio.Flow.copy_string "b" h;
      Eio.File.sync h;
      t := 3.;
      Eio.Flow.copy_string "c" h);
  t := 4.;
  Eio.Path.mkdir ~perm:0o755 (d / "sub");
  Alcotest.(check (list (float 0.))) "f, live" [ 3.; 3. ] (times (root / "f"));
  Alcotest.(check (list (float 0.))) "the root, live" [ 4.; 4. ] (times root);
  let disk = Crash.cut c in
  Alcotest.(check string) "f, after" "b" (Eio.Path.load (disk / "f"));
  Alcotest.(check (list (float 0.))) "f, after" [ 2.; 2. ] (times (disk / "f"));
  Alcotest.(check (list (float 0.))) "the root, after" [ 1.; 1. ] (times disk)

(* Absolute symbolic links, in memory and through eio_posix: the root
   [Eio_fs.Fs.v] returns is [Eio.Stdenv.fs], which follows one from the
   root, and a directory opened from it is confined, refusing to follow one
   with [Permission_denied] in every operation that would. *)

let outcome f =
  match f () with
  | v -> "ok " ^ v
  | exception Eio.Io (Eio.Fs.E (Eio.Fs.Permission_denied _), _) ->
      "permission denied"
  | exception Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> "not found"
  | exception Eio.Io _ -> "error"

let kind p =
  Fmt.str "%a" Eio.File.Stat.pp_kind (Eio.Path.stat ~follow:true p).kind

let unit f () =
  f ();
  ""

(* [dir] holds [abs], a link to the file [top ^ "/s/target"], and [absdir],
   one to the directory [top ^ "/s/tdir"]. *)
let ops (dir : _ Eio.Path.t) =
  let p x = dir / x in
  [
    ("load", fun () -> Eio.Path.load (p "abs"));
    ("stat", fun () -> kind (p "abs"));
    ( "stat, not followed",
      fun () ->
        Fmt.str "%a" Eio.File.Stat.pp_kind
          (Eio.Path.stat ~follow:false (p "abs")).kind );
    ("read_link", fun () -> Filename.basename (Eio.Path.read_link (p "abs")));
    ("save", unit (fun () -> Eio.Path.save ~create:`Never (p "abs") "T"));
    ("chmod", unit (fun () -> Eio.Path.chmod ~follow:true ~perm:0o644 (p "abs")));
    ("read_dir", fun () -> String.concat "," (Eio.Path.read_dir (p "absdir")));
    ( "create below",
      unit (fun () ->
          Eio.Path.save ~create:(`Exclusive 0o644) (p "absdir/x") "x") );
    ("stat below", fun () -> kind (p "absdir/x"));
    ("unlink below", unit (fun () -> Eio.Path.unlink (p "absdir/x")));
    ("mkdir below", unit (fun () -> Eio.Path.mkdir ~perm:0o755 (p "absdir/m")));
    ("rmdir below", unit (fun () -> Eio.Path.rmdir (p "absdir/m")));
    ( "symlink below",
      unit (fun () -> Eio.Path.symlink ~link_to:"z" (p "absdir/y")) );
    ( "rename below",
      unit (fun () -> Eio.Path.rename (p "absdir/y") (p "absdir/y2")) );
    ("unlink renamed", unit (fun () -> Eio.Path.unlink (p "absdir/y2")));
    ( "open_dir",
      unit (fun () ->
          Eio.Switch.run @@ fun sw ->
          ignore (Eio.Path.open_dir ~sw (p "absdir"))) );
  ]

let run_ops dir = List.map (fun (n, f) -> n ^ ": " ^ outcome f) (ops dir)

(* What [fs] at [top], and a directory opened there, answer. *)
let answers fs top =
  let s = fs / (top ^ "/s") in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 s;
  Eio.Path.save ~create:(`Exclusive 0o644) (s / "target") "T";
  Eio.Path.mkdir ~perm:0o755 (s / "tdir");
  Eio.Path.symlink ~link_to:(top ^ "/s/target") (s / "abs");
  Eio.Path.symlink ~link_to:(top ^ "/s/tdir") (s / "absdir");
  let unconfined = run_ops s in
  let confined =
    Eio.Switch.run @@ fun sw -> run_ops (Eio.Path.open_dir ~sw s)
  in
  (unconfined, confined)

let test_as_eio_posix () =
  Eio_posix.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let top = Filename.temp_file "eio-fs-absolute-link" "" in
  Sys.remove top;
  Fun.protect ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true (fs / top))
  @@ fun () ->
  let posix = answers fs top in
  let mem = answers (Eio_fs.Fs.v ()) top in
  let check what (p, m) = Alcotest.(check (list string)) what p m in
  check "Eio.Stdenv.fs and the root" (fst posix, fst mem);
  check "an opened directory" (snd posix, snd mem);
  let ok s =
    match String.index_opt s ':' with
    | Some i ->
        String.starts_with ~prefix:": ok" (String.sub s i (String.length s - i))
    | None -> false
  in
  Alcotest.(check (list string))
    "the root follows" []
    (List.filter (fun s -> not (ok s)) (fst mem));
  Alcotest.(check (list string))
    "an opened directory refuses to follow"
    [ "stat, not followed"; "read_link" ]
    (List.filter_map
       (fun s ->
         if ok s then Some (List.hd (String.split_on_char ':' s)) else None)
       (snd mem))

let suite =
  ( "memfs",
    [
      Alcotest.test_case "the stat trace names its provenance" `Quick test_trace;
      Alcotest.test_case "times move as on Linux" `Quick
        (as_linux "times" ~on:[ "tmpfs"; "ext4" ]);
      Alcotest.test_case "times of moved and removed inodes" `Quick
        (as_linux "inode times" ~on:[ "tmpfs"; "ext4" ]);
      Alcotest.test_case "relatime moves atime once a day" `Quick
        test_relatime_day;
      Alcotest.test_case "link and block counts" `Quick test_counts;
      Alcotest.test_case "without a clock, time stands at 0" `Quick
        test_no_clock;
      Alcotest.test_case "a freshness check under Chaos.check" `Quick
        test_freshness_under_check;
      Alcotest.test_case "a power cut keeps the times the disk kept" `Quick
        test_crash_times;
      Alcotest.test_case "absolute links as eio_posix" `Quick test_as_eio_posix;
    ] )
