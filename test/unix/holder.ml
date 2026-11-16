(* What the cases of this executable share: a fresh directory of the real
   file system or an in-memory one, and another process holding a lock in
   it. *)

let with_real_dir_env fn =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let dir = Filename.temp_file "eio-fs-sync-dir" "" in
  Sys.remove dir;
  Eio.Path.mkdir ~perm:0o755 Eio.Path.(fs / dir);
  Fun.protect ~finally:(fun () ->
      Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / dir))
  @@ fun () -> fn env (Eio.Path.(fs / dir) :> Eio.Fs.dir_ty Eio.Path.t)

let with_real_dir fn = with_real_dir_env (fun _env dir -> fn dir)

let with_in_memory_fs fn =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env -> fn env#fs

let mentions ~needle haystack =
  let n = String.length needle and h = String.length haystack in
  let rec at i =
    i + n <= h && (String.equal (String.sub haystack i n) needle || at (i + 1))
  in
  at 0

(* A process holding [lock] by [kind] ([test.ml], [hold_until_release]), and
   [release], which tells it to let go and returns how it ended: [`Exited 0]
   when it let go at [release], [`Exited 3] when it let go at its own
   five-second bound. *)
let holding_process ~sw ~env ~kind ~lock =
  let manager = Eio.Stdenv.process_mgr env in
  let input, release = Eio.Process.pipe ~sw manager in
  let ready, output = Eio.Process.pipe ~sw manager in
  let self = Sys.executable_name in
  let child =
    Eio.Process.spawn ~sw manager ~stdin:input ~stdout:output ~executable:self
      [ self; "--hold-until-release"; kind; Eio.Path.native_exn lock ]
  in
  Eio.Resource.close input;
  Eio.Resource.close output;
  let announced = Cstruct.create 1 in
  Eio.Flow.read_exact ready announced;
  Alcotest.(check char)
    "the other process holds the lock" 'R'
    (Cstruct.get_char announced 0);
  ( child,
    fun () ->
      (try Eio.Flow.copy_string "X" release with Eio.Io _ -> ());
      Eio.Resource.close release;
      Eio.Process.await child )

let status =
  Alcotest.testable
    (fun ppf -> function
      | `Exited n -> Fmt.pf ppf "exited %d" n
      | `Signaled n -> Fmt.pf ppf "signalled %d" n)
    ( = )

let require what b = Alcotest.(check bool) what true b

let refused f =
  match f () with
  | () -> false
  | exception Eio_fs_unix.Lock_unavailable _ -> true

let rec reap pid =
  match Unix.waitpid [] pid with
  | result -> result
  | exception Unix.Unix_error (Unix.EINTR, _, _) -> reap pid

let exited status code =
  match status with Unix.WEXITED n -> Int.equal n code | _ -> false

(* This executable, run as another process that tries [mode] on [path]: it
   exits 3 when a holder refuses it, 0 when it is granted. *)
let probe mode path expected =
  let self = Sys.executable_name in
  let pid =
    Unix.create_process self
      [| self; mode; Eio.Path.native_exn path |]
      Unix.stdin Unix.stdout Unix.stderr
  in
  let _, status = reap pid in
  require ("another process's probe " ^ mode) (exited status expected)

let ready fd =
  let rec wait () =
    match Unix.select [ fd ] [] [] (-1.) with
    | [], [], [] -> wait ()
    | _ -> true
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> wait ()
  in
  wait ()

let byte fd expected =
  if not (ready fd) then failwith "shared inherited pipe bound";
  let b = Bytes.create 1 in
  if Unix.read fd b 0 1 <> 1 || not (Char.equal (Bytes.get b 0) expected) then
    failwith "shared inherited pipe evidence"

let owner_arg = "--shared-lease-outlives-its-owner"

(* The owner forks a reader that inherits its shared lease, then exits inside
   its callback: the reader's descriptor keeps the hold, against an exclusive
   lease and a POSIX record lock, until it closes it. Each child wait blocks
   until the child writes: a child that never does hangs the test. OCaml
   forks only before any domain is spawned, so this runs in a process of its
   own, run with [owner_arg]. *)
let shared_lease_outlives_its_owner () =
  let lock_name = Filename.temp_file "shared-owner-death-spec" "" in
  let announced, announce = Unix.pipe ~cloexec:true () in
  let gate, release = Unix.pipe ~cloexec:true () in
  let finished, finish = Unix.pipe ~cloexec:true () in
  (* A child exits 0 when [fn] ends there, 3 when it raises: nothing it
     raises may unwind into this process's test. *)
  let in_child fn =
    Fun.protect
      ~finally:(fun () -> Unix._exit 3)
      (fun () ->
        fn ();
        Unix._exit 0)
  in
  let owner = Unix.fork () in
  if owner = 0 then begin
    Unix.close announced;
    Unix.close release;
    Unix.close finished;
    in_child @@ fun () ->
    Eio_main.run @@ fun env ->
    Eio_fs_unix.Lease.with_shared ~block:`No
      Eio.Path.(Eio.Stdenv.fs env / lock_name)
      (fun lease ->
        let raw =
          Eio_unix.Fd.use_exn "shared owner death spec"
            (Eio_fs_unix.Lease.fd lease)
            Fun.id
        in
        let child = Unix.fork () in
        if child = 0 then begin
          Unix.close announce;
          in_child @@ fun () ->
          byte gate 'X';
          Unix.close raw;
          ignore (Unix.write_substring finish "D" 0 1);
          Unix.close finish
        end;
        Unix.close gate;
        Unix.close finish;
        ignore (Unix.write_substring announce "R" 0 1);
        Unix.close announce;
        Unix._exit 0)
  end;
  Unix.close announce;
  Unix.close gate;
  Unix.close finish;
  let released = ref false and reaped = ref false in
  Fun.protect ~finally:(fun () ->
      (if not !released then
         try ignore (Unix.write_substring release "X" 0 1)
         with Unix.Unix_error _ -> ());
      Unix.close release;
      Unix.close announced;
      Unix.close finished;
      if not !reaped then ignore (reap owner);
      Sys.remove lock_name)
  @@ fun () ->
  byte announced 'R';
  let _, status = reap owner in
  reaped := true;
  require "the owner exited inside its callback" (exited status 0);
  Eio_main.run @@ fun env ->
  let lock = Eio.Path.(Eio.Stdenv.fs env / lock_name) in
  require "owner death keeps the inherited shared reader"
    (refused (fun () ->
         Eio_fs_unix.Lease.with_exclusive ~block:`No lock (fun _ -> ())));
  probe "--try-eio-fs-with-lock" lock 3;
  ignore (Unix.write_substring release "X" 0 1);
  released := true;
  byte finished 'D';
  let eof = Bytes.create 1 in
  require "the inherited reader ends at its descriptor's last close"
    (ready finished && Unix.read finished eof 0 1 = 0);
  Eio_fs_unix.Lease.with_exclusive ~block:`No lock (fun _ -> ())
