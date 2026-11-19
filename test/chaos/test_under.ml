(* A program on the disk and the network at once, and a filesystem built
   over the network. *)

let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8080)

let contains s sub =
  let n = String.length sub and m = String.length s in
  let rec go i = i + n <= m && (String.sub s i n = sub || go (i + 1)) in
  go 0

(* Saves a log and asks a server for a greeting, each of which may fail.
   With [bug], it fails once both did in one run, which only a pair of
   failures across the two devices makes. *)
let both ?(bug = false) () =
  Eio_chaos.run [ Eio_fs_chaos.disk (); Eio_net_chaos.network () ] @@ fun env ->
  let saved =
    match
      Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(env#fs / "log") "up"
    with
    | () -> true
    | exception Eio.Io _ -> false
  in
  Eio.Switch.run @@ fun sw ->
  let l = Eio.Net.listen ~reuse_addr:true ~backlog:1 ~sw env#net addr in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio.Net.run_server l ~on_error:ignore (fun flow _ ->
          Eio.Flow.copy_string "hello" flow));
  let fetched =
    match
      Eio.Time.with_timeout env#clock 5.0 (fun () ->
          Ok (Eio.Flow.read_all (Eio.Net.connect ~sw env#net addr)))
    with
    | Ok "hello" -> true
    | Ok _ | Error `Timeout -> false
    | exception (Eio.Io _ | End_of_file) -> false
  in
  if bug && (not saved) && not fetched then failwith "lost both"

let pair () =
  let r = Expect.report (both ~bug:true) in
  let key = Expect.replay r in
  if
    not
      (String.starts_with ~prefix:"fs+net/" key
      && contains key "/fs:" && contains key "+net:")
  then Alcotest.failf "not a pair across the disk and the network: %s" r

let remote ?leak ~sw env = Remote.v ?leak ~sw env

let over_network ?leak () =
  Eio_fs_chaos.conformance ~budget:500
    ~under:[ Eio_net_chaos.network () ]
    (remote ?leak)

let leaks () =
  let r = Expect.report (over_network ~leak:true) in
  if
    not
      (String.starts_with
         ~prefix:"Eio_fs_chaos.conformance: the program failed under a fault" r
      && contains r "Connection_reset"
      && contains r "replay:   Eio_fs_chaos.conformance ~replay:\"net+fs/")
  then Alcotest.failf "the leak is not reported: %s" r

let suite =
  ( "under",
    [
      Alcotest.test_case "a program on a disk and a network" `Quick
        (Expect.passes (both ?bug:None));
      Alcotest.test_case "a pair across the disk and the network" `Quick pair;
      Alcotest.test_case "a remote filesystem over a faulty network" `Quick
        (Expect.passes (over_network ?leak:None));
      Alcotest.test_case "a remote filesystem that leaks network errors" `Quick
        leaks;
    ] )
