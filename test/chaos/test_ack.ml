(* A file the program acknowledged must hold what it held then, after each
   fault and before the restart; inspect judges the rest. *)

module Chaos = Eio_fs_chaos
module P = Eio.Path

let config env = P.(env#fs / "config")

(* Saves [config] and acknowledges it. *)
let app ?bug env =
  App.save_config ?bug env#fs "new";
  Chaos.ack env (config env)

let title report = List.hd (String.split_on_char '\n' report)
let saved () = Chaos.run (app ~bug:`None)

(* A cut loses the rename that no directory sync made durable. *)
let unsynced_dir () =
  let report = Expect.report (fun () -> Chaos.run (app ~bug:`No_dir_sync)) in
  let prefix = "Eio_fs_chaos.run: an acknowledged file lost what it held" in
  if not (String.starts_with ~prefix (title report)) then
    Alcotest.failf "not a lost file: %s" report;
  Alcotest.(check string)
    "error"
    "/config held \"new\" when acknowledged after #4, and now is missing"
    (Expect.field "error" report);
  let replay = Expect.replay report in
  Alcotest.(check string)
    "the replay fails the same way" report
    (Expect.report (fun () -> Chaos.run ~replay (app ~bug:`No_dir_sync)))

(* A file written again after its ack is the program's to judge. *)
let changed_since () =
  Chaos.run (fun env ->
      App.save_config env#fs "new";
      Chaos.ack env (config env);
      P.save ~create:(`Or_truncate 0o644) (config env) "newer")

(* inspect runs after each fault with the paths acknowledged before it. *)
let inspected () =
  let seen = ref [] in
  Chaos.run (app ~bug:`None) ~inspect:(fun env ->
      let acked = Chaos.acked env in
      if not (List.mem acked !seen) then seen := acked :: !seen);
  Alcotest.(check (list (list string)))
    "acked" [ [ "/config" ]; [] ]
    (List.sort compare !seen |> List.rev)

let rejected () =
  let report =
    Expect.report (fun () ->
        Chaos.run (app ~bug:`None) ~inspect:(fun _ -> failwith "no"))
  in
  Alcotest.(check string)
    "title" "Eio_fs_chaos.run: inspect rejected what the fault left"
    (title report)

(* An env no check made. *)
let stranger () : Chaos.env =
  let fs = (Eio_fs.Fs.v () :> Eio.Fs.dir_ty P.t) in
  object
    method fs = fs
    method cwd = fs

    method clock =
      (Eio_mock.Clock.make () :> float Eio.Time.clock_ty Eio.Resource.t)

    method mono_clock =
      (Eio_mock.Clock.Mono.make () :> Eio.Time.Mono.ty Eio.Resource.t)
  end

let refused () =
  ( Eio_mock.Backend.run @@ fun () ->
    let env = stranger () in
    P.save ~create:(`Exclusive 0o644) P.(env#fs / "f") "x";
    match Chaos.ack env P.(env#fs / "f") with
    | () -> Alcotest.fail "an env of no run was accepted"
    | exception Invalid_argument _ -> () );
  Chaos.run (fun env ->
      (try P.mkdir ~perm:0o755 P.(env#fs / "d") with Eio.Io _ -> ());
      match Chaos.ack env P.(env#fs / "d") with
      | () -> failwith "a directory was acknowledged"
      | exception Invalid_argument _ -> ())

let suite =
  ( "ack",
    [
      Alcotest.test_case "a synced save passes" `Quick (Expect.passes saved);
      Alcotest.test_case "a save with no directory sync loses its file" `Quick
        unsynced_dir;
      Alcotest.test_case "a file changed since its ack is not compared" `Quick
        (Expect.passes changed_since);
      Alcotest.test_case "inspect sees what was acknowledged" `Quick inspected;
      Alcotest.test_case "inspect rejects" `Quick rejected;
      Alcotest.test_case "refused arguments" `Quick refused;
    ] )
