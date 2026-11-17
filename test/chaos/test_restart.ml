(* An app runs under a check as is: a faulty run may end with Eio.Io, and
   the app, started again on what it left, must then return. *)

module P = Eio.Path

let complete = "complete"

(* The app's start-up: a file it finds must hold what it saved. *)
let start f =
  match P.load f with
  | s when s = complete -> ()
  | s -> Fmt.failwith "corrupt file: %S" s
  | exception Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> ()

(* Saves through a temporary file and a rename, so a fault leaves the old
   file or the new one; in place, a fault leaves part of it. *)
let app ~in_place env =
  let f = P.(env#fs / "f") in
  start f;
  if in_place then P.save ~create:(`Or_truncate 0o644) f complete
  else begin
    let tmp = P.(env#fs / "f.tmp") in
    P.save ~create:(`Or_truncate 0o644) tmp complete;
    P.rename tmp f
  end

let atomic () = Eio_fs.Chaos.check (app ~in_place:false)
let in_place ?replay () = Eio_fs.Chaos.check ?replay (app ~in_place:true)

let restart_failed () =
  let report = Expect.report (fun () -> in_place ()) in
  let title = List.hd (String.split_on_char '\n' report) in
  if not (String.starts_with ~prefix:"Eio_fs.Chaos.check: the restart" title)
  then Alcotest.failf "not a failed restart: %s" report;
  let replay =
    Scanf.sscanf (Expect.field "replay" report) "~replay:%S" Fun.id
  in
  Alcotest.(check string)
    "the replay fails the same way" report
    (Expect.report (fun () -> in_place ~replay ()))

(* Two fibers that both meet a fault end with Eio.Exn.Multiple. *)
let both () =
  Eio_fs.Chaos.check (fun env ->
      Eio.Fiber.both
        (fun () -> P.save ~create:(`Or_truncate 0o644) P.(env#fs / "a") "a")
        (fun () -> P.save ~create:(`Or_truncate 0o644) P.(env#fs / "b") "b"))

(* An exception other than Eio.Io still fails the check. *)
let other () =
  Eio_fs.Chaos.check (fun env ->
      try P.save ~create:(`Or_truncate 0o644) P.(env#fs / "f") complete
      with Eio.Io _ -> failwith "save failed")

let suite =
  ( "restart",
    [
      Alcotest.test_case "an Eio.Io that leaves a clean state passes" `Quick
        (Expect.passes atomic);
      Alcotest.test_case "a state the restart rejects fails" `Quick
        restart_failed;
      Alcotest.test_case "Eio.Io from two fibers passes" `Quick
        (Expect.passes both);
      Alcotest.test_case "another exception fails" `Quick (Expect.caught other);
    ] )
