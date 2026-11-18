(* Power cuts under [~os:`Macos]: fsync(2) stops at the drive's cache, which
   a cut loses, and F_FULLFSYNC goes through it. *)

module P = Eio.Path

type barrier = {
  file : Eio.File.rw_ty Eio.Resource.t -> unit;
  dir : Eio.Fs.dir_ty P.t -> unit;
}

(* fsync(2), as [Eio.File.sync] and the portable barriers issue it. *)
let plain = { file = Eio.File.sync; dir = Eio.Path.sync_dir }

(* F_FULLFSYNC on Darwin. *)
let full = { file = Eio_fs_unix.sync_file; dir = Eio_fs_unix.sync_dir }

(* A value made durable with [b], then acknowledged: recover must find it. *)
let save ?os b () =
  Eio_fs_chaos.Private.check_cuts ?os
    (fun ~ack env ->
      match
        P.with_open_out ~create:(`Exclusive 0o644)
          P.(env#fs / "v")
          (fun f ->
            Eio.Flow.copy_string "v1" f;
            b.file f);
        b.dir env#fs
      with
      | () -> ack ()
      | exception Eio.Io _ -> ())
    ~recover:(fun ~acked env ->
      if acked <> [] && P.load P.(env#fs / "v") <> "v1" then failwith "lost")

(* Two files synced with fsync(2), then a third with F_FULLFSYNC, which
   drains the drive's queue: the first two are durable. *)
let drained () =
  Eio_fs_chaos.Private.check_cuts ~os:`Macos
    (fun ~ack env ->
      let write name sync =
        P.with_open_out ~create:(`Exclusive 0o644)
          P.(env#fs / name)
          (fun f ->
            Eio.Flow.copy_string name f;
            sync f)
      in
      match
        write "a" Eio.File.sync;
        write "b" Eio.File.sync;
        Eio.Path.sync_dir env#fs;
        write "c" Eio_fs_unix.sync_file
      with
      | () -> ack ()
      | exception Eio.Io _ -> ())
    ~recover:(fun ~acked env ->
      if acked <> [] then
        List.iter
          (fun n -> if P.load P.(env#fs / n) <> n then failwith (n ^ " lost"))
          [ "a"; "b" ])

(* A record removed, then a new log written past what is durable: a sector
   of the log may hold the removed record, which a recover that trusts what
   it reads replays. Copy-on-write does not rule it out on APFS. *)
let replay ?os () =
  let record = String.make 512 'R' in
  Eio_fs_chaos.Private.check_cuts ?os
    ~setup:(fun env -> Eio_fs_sync.save P.(env#fs / "old") record)
    (fun ~ack:_ env ->
      try
        P.unlink P.(env#fs / "old");
        Eio.Path.sync_dir env#fs;
        P.save ~create:(`Exclusive 0o644)
          P.(env#fs / "log")
          (String.make 1024 'N')
      with Eio.Io _ -> ())
    ~recover:(fun ~acked:_ env ->
      match P.load P.(env#fs / "log") with
      | s when String.length s >= 512 && String.sub s 0 512 = record ->
          failwith "replayed a removed record"
      | _ | (exception Eio.Io _) -> ())

let replayed os () =
  let m = Expect.report (replay ~os) in
  Alcotest.(check string)
    "error" "Failure(\"replayed a removed record\")" (Expect.field "error" m)

(* A file made, written and renamed under other spellings of its names,
   each step synced: it is one file, as on APFS. *)
let spellings () =
  Eio_fs_chaos.Private.check_cuts ~os:`Macos
    (fun ~ack env ->
      match
        P.mkdir ~perm:0o755 P.(env#fs / "D");
        P.save ~create:(`Exclusive 0o644) P.(env#fs / "d/F") "";
        P.with_open_out ~create:`Never
          P.(env#fs / "D/f")
          (fun f ->
            Eio.Flow.copy_string "v1" f;
            Eio_fs_unix.sync_file f);
        Eio_fs_unix.sync_dir P.(env#fs / "d");
        Eio_fs_unix.sync_dir env#fs;
        P.rename P.(env#fs / "d/f") P.(env#fs / "D/g");
        Eio_fs_unix.sync_dir P.(env#fs / "D")
      with
      | () -> ack ()
      | exception Eio.Io _ -> ())
    ~recover:(fun ~acked env ->
      if acked <> [] && P.load P.(env#fs / "d/G") <> "v1" then failwith "lost")

(* "v" written and synced with fsync(2), then synced again, which may fail,
   then F_FULLFSYNC of another file, which drains the drive's queue: what
   the first fsync flushed is durable, whatever the second did. With
   [more], a write between the two is lost when the second fails. *)
let flushed ?os ~more () =
  Eio_fs_chaos.Private.check_cuts ?os
    ~setup:(fun env ->
      Eio_fs_sync.save P.(env#fs / "v") "";
      Eio_fs_sync.save P.(env#fs / "g") "")
    (fun ~ack env ->
      match
        P.with_open_out ~append:true ~create:`Never
          P.(env#fs / "v")
          (fun f ->
            Eio.Flow.copy_string "one" f;
            Eio.File.sync f;
            if more then Eio.Flow.copy_string "two" f;
            try Eio.File.sync f with Eio.Io _ -> ());
        P.with_open_out ~create:`Never P.(env#fs / "g") Eio_fs_unix.sync_file
      with
      | () -> ack ()
      | exception Eio.Io _ -> ())
    ~recover:(fun ~acked env ->
      if
        acked <> []
        && not (String.starts_with ~prefix:"one" (P.load P.(env#fs / "v")))
      then failwith "lost")

(* A failed fsync, then one that succeeds and F_FULLFSYNC of another file:
   the write the failure lost is not durable, as Linux loses it. *)
let retried ?os () =
  Eio_fs_chaos.Private.check_cuts ?os
    ~setup:(fun env ->
      Eio_fs_sync.save P.(env#fs / "v") "";
      Eio_fs_sync.save P.(env#fs / "g") "")
    (fun ~ack env ->
      match
        P.with_open_out ~append:true ~create:`Never
          P.(env#fs / "v")
          (fun f ->
            Eio.Flow.copy_string "one" f;
            try Eio.File.sync f with Eio.Io _ -> Eio.File.sync f);
        P.with_open_out ~create:`Never P.(env#fs / "g") Eio_fs_unix.sync_file
      with
      | () -> ack ()
      | exception Eio.Io _ -> ())
    ~recover:(fun ~acked env ->
      if acked <> [] && P.load P.(env#fs / "v") <> "one" then failwith "lost")

(* The README's config save, with F_FULLFSYNC on the directory but fsync(2)
   on the file: the new config may be lost after its rename. *)
let config () =
  let save dir v =
    let tmp = P.(dir / "config.tmp") in
    P.with_open_out ~create:(`Or_truncate 0o644) tmp (fun f ->
        Eio.Flow.copy_string v f;
        Eio.File.sync f);
    P.rename tmp P.(dir / "config");
    Eio_fs_unix.sync_dir dir
  in
  Eio_fs_chaos.Private.check_cuts ~os:`Macos
    ~setup:(fun env -> save env#fs "old")
    (fun ~ack env ->
      match save env#fs "new" with () -> ack () | exception Eio.Io _ -> ())
    ~recover:(fun ~acked env ->
      match (P.load P.(env#fs / "config"), acked) with
      | "new", _ | "old", [] -> ()
      | v, _ -> Fmt.failwith "config is %S" v)

(* Each write or truncation the report says a cut lost or may have lost
   names its file as the trace's operation of the same number does. *)
let named_as_traced () =
  let m = Expect.report config in
  let trace =
    String.split_on_char '\n' m
    |> List.map String.trim
    |> List.filter (String.starts_with ~prefix:"#")
  in
  let changes =
    Expect.field "lost" m ^ "; " ^ Expect.field "unsynced" m
    |> String.split_on_char ';' |> List.map String.trim
    |> List.filter (fun c ->
        match String.split_on_char ' ' c with
        | _ :: ("write" | "truncate") :: _ -> true
        | _ -> false)
  in
  if changes = [] then Alcotest.failf "no write in the report: %s" m;
  List.iter
    (fun c ->
      if not (List.mem c trace) then
        Alcotest.failf "%S is not in the trace:\n%s" c
          (String.concat "\n" trace))
    changes

(* A cut that loses what fsync(2) flushed says why; one on Linux does not. *)
let noted () =
  let note =
    "under macOS an fsync stops in the drive's cache; F_FULLFSYNC \
     (Eio_fs_unix.sync_file, Eio_fs_unix.barrier) goes through it"
  in
  Alcotest.(check string)
    "macOS" note
    (Expect.field "note" (Expect.report (save ~os:`Macos plain)));
  let has_note r =
    List.exists
      (String.starts_with ~prefix:"  note:")
      (String.split_on_char '\n' r)
  in
  let r = Expect.report (fun () -> replay ()) in
  if has_note r then Alcotest.failf "a note on Linux: %s" r

let suite =
  ( "macos",
    [
      Alcotest.test_case "fsync is durable on Linux" `Quick
        (Expect.passes (save plain));
      Alcotest.test_case "fsync is not durable on macOS" `Quick
        (Expect.caught (save ~os:`Macos plain));
      Alcotest.test_case "F_FULLFSYNC is durable on macOS" `Quick
        (Expect.passes (save ~os:`Macos full));
      Alcotest.test_case "F_FULLFSYNC drains earlier fsyncs" `Quick
        (Expect.passes drained);
      Alcotest.test_case "a stale record is replayed on Linux" `Quick
        (replayed `Linux);
      Alcotest.test_case "a stale record is replayed on macOS" `Quick
        (replayed `Macos);
      Alcotest.test_case "two spellings of a name are one file" `Quick
        (Expect.passes spellings);
      Alcotest.test_case "a failed fsync keeps what one before flushed" `Quick
        (Expect.passes (flushed ~os:`Macos ~more:false));
      Alcotest.test_case
        "a failed fsync of a later write keeps an earlier flush" `Quick
        (Expect.passes (flushed ~os:`Macos ~more:true));
      Alcotest.test_case "a failed fsync keeps what one before synced" `Quick
        (Expect.passes (flushed ~more:false));
      Alcotest.test_case "a failed fsync of a later write keeps an earlier sync"
        `Quick
        (Expect.passes (flushed ~more:true));
      Alcotest.test_case "a retry after a failed fsync is lost on Linux" `Quick
        (Expect.caught retried);
      Alcotest.test_case "a retry after a failed fsync is lost on macOS" `Quick
        (Expect.caught (retried ~os:`Macos));
      Alcotest.test_case "a report names a write as its trace does" `Quick
        named_as_traced;
      Alcotest.test_case "a cut under macOS says what fsync misses" `Quick noted;
    ] )
