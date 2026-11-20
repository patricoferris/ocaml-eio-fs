(* With 254 records, the four outside the linear tier's structure leave 250
   writes, each tried with four faults: a single byte, a tear, ENOSPC and
   EIO, in that order. The sampled tier has 250 runs for those 1000
   candidates, so taking every fourth one would only ever move a single
   byte, and never tear a write in the middle of the log. *)

let check ?seed w () = Eio_fs_chaos.run ?seed w
let replay_of m = Expect.replay m

(* A replay names its scenario, so one found under another seed runs again
   with none, and with any other. *)
let test_seed_replay () =
  let w = Sampling.log ~bug:true ~n:254 in
  let found = Expect.report (check ~seed:7 w) in
  let replay = replay_of found in
  List.iter
    (fun seed ->
      let again = Expect.report (fun () -> Eio_fs_chaos.run ?seed ~replay w) in
      Alcotest.(check string)
        "faults"
        (Expect.field "faults" found)
        (Expect.field "faults" again))
    [ None; Some 7; Some 8 ]

(* The same for a power cut: the scenario of a failure the sampled tier
   found under seed 7 runs again under any seed. *)
let test_crash_seed_replay () =
  let config ?seed ?replay () =
    let save dir v =
      let tmp = Eio.Path.(dir / "config.tmp") in
      Eio.Path.with_open_out ~create:(`Or_truncate 0o644) tmp (fun f ->
          Eio.Flow.copy_string v f;
          Eio.File.sync f);
      Eio.Path.rename tmp Eio.Path.(dir / "config")
    in
    Eio_fs_chaos.Private.check_cuts ?seed ?replay
      ~setup:(fun env -> save env#fs "old")
      (fun ~ack env ->
        match save env#fs "new" with () -> ack () | exception Eio.Io _ -> ())
      ~recover:(fun ~acked env ->
        match (Eio.Path.load Eio.Path.(env#fs / "config"), acked) with
        | "new", _ | "old", [] -> ()
        | v, _ -> Fmt.failwith "config is %S" v)
  in
  let found = Expect.report (config ~seed:7) in
  let replay = replay_of found in
  List.iter
    (fun seed ->
      let again = Expect.report (config ?seed ~replay) in
      List.iter
        (fun f ->
          Alcotest.(check string)
            f (Expect.field f found) (Expect.field f again))
        [ "cut"; "faults"; "error" ])
    [ None; Some 7; Some 8 ]

(* The linear tier grows with the program's structure, not its square: the
   cuts during a restart are drawn in the sampled tier. *)
let saves n env =
  for i = 1 to n do
    Eio.Path.save ~create:(`Or_truncate 0o644)
      Eio.Path.(env#fs / Fmt.str "f%d" i)
      "x"
  done

let publishes n env =
  for i = 1 to n do
    Eio_fs.Sync.publish ~max_bytes:16L
      Eio.Path.(env#fs / Fmt.str "f%d" i)
      (Eio.Flow.copy_string "x")
  done

let suite =
  ( "sampling",
    [
      Alcotest.test_case "log: correct" `Quick
        (Expect.passes (check (Sampling.log ~n:254)));
      Alcotest.test_case
        "log: no rollback in the middle, by the sampled tier, whatever the \
         period of its candidates"
        `Quick
        (Expect.caught (check (Sampling.log ~bug:true ~n:254)));
      Alcotest.test_case "a failure found under seed 7 replays under any seed"
        `Quick test_seed_replay;
      Alcotest.test_case "a power cut found under seed 7 replays under any seed"
        `Quick test_crash_seed_replay;
      Alcotest.test_case "64 saves fit the default budget" `Slow
        (Expect.passes (check (saves 64)));
      Alcotest.test_case "16 publications fit the default budget" `Slow
        (Expect.passes (check (publishes 16)));
    ] )
