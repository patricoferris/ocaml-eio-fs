(* A workload's env mirrors Eio_main's: [fs] reaches the whole tree, [cwd] is
   confined to its directory, as eio_posix confines it. *)

module P = Eio.Path

(* An absolute link, and a directory it names. *)
let setup env =
  P.mkdir ~perm:0o755 P.(env#fs / "x");
  P.symlink ~link_to:"/x" P.(env#fs / "abs")

(* Through [cwd], an absolute link, an absolute path and a [..] above it are
   refused; through [fs] they resolve. *)
let confined env =
  let refused p =
    match P.stat ~follow:true p with
    | _ -> Fmt.failwith "%a resolved" P.pp p
    | exception Eio.Io _ -> ()
  in
  refused P.(env#cwd / "abs");
  refused P.(env#cwd / "/x");
  refused P.(env#cwd / "..")

let check () = Eio_fs.Chaos.check ~setup (fun env -> confined env)

let crash () =
  Eio_fs.Chaos.Crash.check ~setup
    (fun ~ack env ->
      match Eio_fs_sync.save P.(env#cwd / "f") "v" with
      | () -> ack ()
      | exception Eio.Io _ -> ())
    ~recover:(fun ~acked:_ env -> confined env)

let alice = { Eio_fs.uid = 1000L; gid = 1000L; groups = []; umask = 0o022 }

(* Root's [etc], and a [home] anyone may write. *)
let setup_dirs env =
  P.mkdir ~perm:0o755 P.(env#fs / "etc");
  P.mkdir ~perm:0o755 P.(env#fs / "home");
  P.chmod ~follow:true ~perm:0o777 P.(env#fs / "home")

(* As alice, through [fs] and [cwd]: a save in [etc] never succeeds. *)
let denied env =
  List.iter
    (fun d ->
      let d = Eio_fs.Fs.as_user alice d in
      match P.save ~create:(`Or_truncate 0o644) P.(d / "etc/conf") "x" with
      | () -> failwith "alice saved etc/conf"
      | exception Eio.Io _ -> ())
    [ env#fs; env#cwd ]

let check_as_user () = Eio_fs.Chaos.check ~setup:setup_dirs denied

(* The faults still reach the operations alice makes. *)
let check_as_user_faults () =
  Eio_fs.Chaos.check ~setup:setup_dirs (fun env ->
      let fs = Eio_fs.Fs.as_user alice env#fs in
      P.save ~create:(`Or_truncate 0o644) P.(fs / "home/f") "x")

let crash_as_user () =
  Eio_fs.Chaos.Crash.check ~setup:setup_dirs
    (fun ~ack env ->
      denied env;
      let fs = Eio_fs.Fs.as_user alice env#fs in
      match Eio_fs_sync.save P.(fs / "home/f") "v" with
      | () -> ack ()
      | exception Eio.Io _ -> ())
    ~recover:(fun ~acked env ->
      let fs = Eio_fs.Fs.as_user alice env#fs in
      match (P.load P.(fs / "home/f"), acked) with
      | "v", _ -> ()
      | exception Eio.Io _ when acked = [] -> ()
      | v, _ -> Fmt.failwith "home/f is %S" v)

let suite =
  ( "env",
    [
      Alcotest.test_case "check: cwd is confined" `Quick (Expect.passes check);
      Alcotest.test_case "crash: cwd is confined" `Quick (Expect.passes crash);
      Alcotest.test_case "check: a workload runs as a user" `Quick
        (Expect.passes check_as_user);
      Alcotest.test_case "check: faults reach a user's operations" `Quick
        (Expect.caught check_as_user_faults);
      Alcotest.test_case "crash: a workload runs as a user" `Quick
        (Expect.passes crash_as_user);
    ] )
