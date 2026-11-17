(* A program's env mirrors Eio_main's: [fs] reaches the whole tree, [cwd] is
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

let check () = Eio_fs_chaos.run ~setup (fun env -> confined env)

let crash () =
  Eio_fs_chaos.Private.check_cuts ~setup
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

let check_as_user () = Eio_fs_chaos.run ~setup:setup_dirs denied

(* The faults still reach the operations alice makes: a save that must not
   fail fails under one. *)
let check_as_user_faults () =
  Eio_fs_chaos.run ~setup:setup_dirs (fun env ->
      let fs = Eio_fs.Fs.as_user alice env#fs in
      try P.save ~create:(`Or_truncate 0o644) P.(fs / "home/f") "x"
      with Eio.Io _ -> failwith "save failed")

let crash_as_user () =
  Eio_fs_chaos.Private.check_cuts ~setup:setup_dirs
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

(* A publication through a temporary file whose name the program draws:
   each run of a check may name it differently. *)
let publish name env =
  let tmp = P.(env#fs / name) in
  P.with_open_out ~create:(`Exclusive 0o644) tmp (fun f ->
      Eio.Flow.copy_string "v" f;
      Eio.File.sync f);
  P.rename tmp P.(env#fs / "target")

let random_name () =
  Eio_fs_chaos.run (fun env ->
      publish (Fmt.str "target.%d" (Random.int 1_000_000_000)) env)

(* A counter that outlives the run, as a module's own sequence does. *)
let counter_name () =
  let n = ref 0 in
  Eio_fs_chaos.run (fun env ->
      incr n;
      publish (Fmt.str "target.%d" !n) env)

(* A program written against Eio.Stdenv's accessors: it names its
   temporary file from secure_random, says so on stdout, and saves through a
   domain manager. *)
let main ?(fail = false) env =
  let hex = Cstruct.create 8 in
  Eio.Flow.read_exact (Eio.Stdenv.secure_random env) hex;
  let name = "target." ^ Cstruct.to_hex_string hex in
  Eio.Flow.copy_string (name ^ "\n") (Eio.Stdenv.stdout env);
  Eio.Domain_manager.run (Eio.Stdenv.domain_mgr env) (fun () ->
      publish name env);
  if fail then failwith "done"

let stdenv () = Eio_fs_chaos.run (main ~fail:false)

(* Two checks print the same: the report shows it. *)
let printed () =
  let output () =
    Expect.field "output"
      (Expect.report (fun () -> Eio_fs_chaos.run (main ~fail:true)))
  in
  let first = output () in
  if not (String.starts_with ~prefix:"target." first) then
    Alcotest.failf "the report shows %S" first;
  Alcotest.(check string) "the same output" first (output ())

(* A check starts no process. *)
let no_process () =
  Expect.refused "Eio_fs_chaos.run: the run with no fault failed: Eio.Io"
    (fun () ->
      Eio_fs_chaos.run (fun env ->
          P.save ~create:(`Or_truncate 0o644) P.(env#fs / "f") "x";
          Eio.Switch.run @@ fun sw ->
          ignore
            (Eio.Process.spawn ~sw (Eio.Stdenv.process_mgr env) [ "true" ]
              : _ Eio.Process.t)))
    ()

let suite =
  ( "env",
    [
      Alcotest.test_case "check: cwd is confined" `Quick (Expect.passes check);
      Alcotest.test_case "crash: cwd is confined" `Quick (Expect.passes crash);
      Alcotest.test_case "check: a program runs as a user" `Quick
        (Expect.passes check_as_user);
      Alcotest.test_case "check: faults reach a user's operations" `Quick
        (Expect.caught check_as_user_faults);
      Alcotest.test_case "crash: a program runs as a user" `Quick
        (Expect.passes crash_as_user);
      Alcotest.test_case "a temporary named by Random" `Quick
        (Expect.passes random_name);
      Alcotest.test_case "a temporary named by a counter" `Quick
        (Expect.passes counter_name);
      Alcotest.test_case "a program written against Eio.Stdenv" `Quick
        (Expect.passes stdenv);
      Alcotest.test_case "two checks print the same" `Quick printed;
      Alcotest.test_case "a check starts no process" `Quick no_process;
    ] )
