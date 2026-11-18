(* Chaos.device against the kernel traces: the in-memory filesystem passes
   for Linux and for macOS, as a user and as root, a directory with a fault
   fails with a report, and eio_posix on this machine fails exactly where its
   recorded answers differ from the kernel's. README.md says where the
   traces come from. *)

module Chaos = Eio_fs_chaos

type os = [ `Linux | `Macos | `Windows ]

let user =
  { Eio_fs_mem.uid = 1000L; gid = 1000L; groups = [ 4242L ]; umask = 0o022 }

let memfs (os : os) () =
  Eio_mock.Backend.run @@ fun () ->
  Chaos.device ~os ~user (fun ~sw:_ _ -> Eio_fs_mem.v ~os ~user ())

(* Without [~user], the scenarios that need one are left out, and root
   passes the others. *)
let memfs_root (os : os) () =
  Eio_mock.Backend.run @@ fun () ->
  Chaos.device ~os (fun ~sw:_ _ -> Eio_fs_mem.v ~os ())

(* A device built on another capability takes it from the env: here a
   wrapper over the env's own filesystem. *)
let overlay (os : os) () =
  Chaos.device ~os (fun ~sw:_ env -> Chaos.wrap (fun _ -> None) env#fs)

let failure f =
  match f () with
  | () -> Alcotest.fail "the check passed"
  | exception Failure report -> report

(* The scenarios a report says differ. *)
let differing report =
  String.split_on_char '\n' report
  |> List.filter_map (fun l ->
      if String.length l > 2 && String.sub l 0 2 = "  " && l.[2] <> ' ' then
        match String.index_opt l ':' with
        | Some i -> Some (String.sub l 2 (i - 2))
        | None -> None
      else None)

(* "stat A" after "write a x" finds the file on macOS: a stat of [A] that
   answers ENOENT differs there. *)
let no_stat_of_a (op : Chaos.Op.t) =
  if op.kind = `Stat && Filename.basename op.path = "A" then
    Some (Chaos.Fault.Fail `Enoent)
  else None

let test_stat_fault () =
  Eio_mock.Backend.run @@ fun () ->
  let report =
    failure @@ fun () ->
    Chaos.device ~os:`Macos ~user (fun ~sw:_ _ ->
        Chaos.wrap no_stat_of_a (Eio_fs_mem.v ~os:`Macos ~user ()))
  in
  Alcotest.(check string)
    "report"
    {|Eio_fs_chaos.device: 1 of 112 scenarios differ from macOS
  case lookup: write a x; stat A: expected file 1, got ENOENT
  111 passed, 1 differed, 3 left out; 3 that need a native path|}
    report

(* An error that prints on several lines is reported on one. *)
type Eio.Exn.Backend.t += Odd

let () =
  Eio.Exn.Backend.register_pp (fun ppf -> function
    | Odd ->
        Fmt.string ppf "odd";
        true
    | _ -> false)

let test_one_line () =
  let odd (op : Chaos.Op.t) =
    if op.kind = `Stat && Filename.basename op.path = "A" then
      raise
        (Eio.Exn.add_context (Eio.Exn.create (Eio.Exn.X Odd)) "stat %s" op.path)
    else None
  in
  let report =
    failure @@ fun () ->
    Chaos.device ~os:`Macos ~user (fun ~sw:_ _ ->
        Chaos.wrap odd (Eio_fs_mem.v ~os:`Macos ~user ()))
  in
  Alcotest.(check int)
    "lines" 3
    (List.length (String.split_on_char '\n' report))

(* Where eio_posix differs from the kernel too, the report says so. *)
let test_eio_posix_too () =
  let report =
    failure @@ fun () ->
    Chaos.device ~os:`Linux ~user (fun ~sw:_ _ ->
        Chaos.wrap
          (fun op ->
            if
              op.kind = `Change
              && List.length (String.split_on_char '/' op.path) > 2
            then Some (Fail `Eio)
            else None)
          (Eio_fs_mem.v ~user ()))
  in
  let differs = Chaos.Private.eio_posix_differs `Linux in
  List.iter
    (fun line ->
      match differing line with
      | [ s ] ->
          let marked =
            String.ends_with ~suffix:"(eio_posix differs here too)" line
          in
          if marked <> List.mem s differs then
            Alcotest.failf "%s: marked %b" s marked
      | _ -> ())
    (String.split_on_char '\n' report)

(* Linux does not find [A] either: the same directory passes there. *)
let test_stat_fault_linux () =
  Eio_mock.Backend.run @@ fun () ->
  Chaos.device ~os:`Linux ~user (fun ~sw:_ _ ->
      Chaos.wrap no_stat_of_a (Eio_fs_mem.v ~user ()))

(* Under [`Linux] the filesystem keeps [a] and [A] apart, as macOS does
   not. *)
let test_wrong_os () =
  Eio_mock.Backend.run @@ fun () ->
  let report =
    failure @@ fun () ->
    Chaos.device ~os:`Macos ~user (fun ~sw:_ _ ->
        Eio_fs_mem.v ~os:`Linux ~user ())
  in
  Alcotest.(check bool)
    "names the first scenario" true
    (List.mem "case lookup" (differing report))

(* A report without [~user] counts the scenarios it left out. *)
let test_left_out () =
  Eio_mock.Backend.run @@ fun () ->
  let report =
    failure @@ fun () ->
    Chaos.device ~os:`Macos (fun ~sw:_ _ -> Eio_fs_mem.v ~os:`Linux ())
  in
  let last = List.hd (List.rev (String.split_on_char '\n' report)) in
  Alcotest.(check bool)
    "left out" true
    (String.ends_with
       ~suffix:"14 left out; 11 that need ~user; 3 that need a native path" last)

(* The trace with its columns in reverse order but the operations', and one
   added: the device finds each column by its name. *)
let test_columns_by_name () =
  let lines =
    In_channel.with_open_bin
      (Filename.concat "traces" "fs.csv")
      In_channel.input_lines
  in
  let shuffle line extra =
    match String.split_on_char ',' line with
    | ops :: cols -> String.concat "," ((ops :: List.rev cols) @ [ extra ])
    | [] -> line
  in
  let trace =
    String.concat "\n"
      (match lines with
      | header :: rows ->
          shuffle header "a column added later"
          :: List.map (fun r -> shuffle r "?") rows
      | [] -> [])
  in
  List.iter
    (fun (os, basic) ->
      List.iter
        (fun name ->
          Alcotest.(check (option (list string)))
            name
            (Chaos.Private.expected ~basic os name)
            (Chaos.Private.expected ~trace ~basic os name))
        Chaos.Private.scenarios)
    [ (`Linux, false); (`Macos, false); (`Windows, false); (`Windows, true) ]

let invalid name f =
  match f () with
  | () -> Alcotest.failf "%s: the check passed" name
  | exception Invalid_argument _ -> ()

let test_refused () =
  Eio_mock.Backend.run @@ fun () ->
  let fs ~sw:_ _ = Eio_fs_mem.v ~user () in
  invalid "root" (fun () -> Chaos.device ~user:Eio_fs_mem.root fs);
  invalid "umask" (fun () -> Chaos.device ~user:{ user with umask = 0o077 } fs);
  invalid "no supplementary group" (fun () ->
      Chaos.device ~user:{ user with groups = [ 1000L ] } fs)

(* eio_posix on this machine, in a fresh directory of $TMPDIR. *)

let host_os () =
  let ic = Unix.open_process_args_in "uname" [| "uname"; "-s" |] in
  let s = In_channel.input_all ic in
  ignore (Unix.close_process_in ic);
  match String.trim s with
  | "Darwin" -> Some `Macos
  | "Linux" -> Some `Linux
  | _ -> None

let host_user () =
  let gid = Unix.getgid () in
  match List.filter (( <> ) gid) (Array.to_list (Unix.getgroups ())) with
  | other :: _ when Unix.getuid () <> 0 ->
      Some
        {
          Eio_fs_mem.uid = Int64.of_int (Unix.getuid ());
          gid = Int64.of_int gid;
          groups = [ Int64.of_int other ];
          umask = 0o022;
        }
  | _ -> None

let remove dir =
  ignore (Sys.command (Filename.quote_command "chmod" [ "-R"; "u+rwx"; dir ]));
  ignore (Sys.command (Filename.quote_command "rm" [ "-rf"; dir ]))

(* eio_posix, which Windows lacks: there the test is skipped. *)
let eio_posix f =
  match Posix.run with Some run -> run f | None -> Alcotest.skip ()

let test_host () =
  eio_posix @@ fun env ->
  match (host_os (), host_user ()) with
  | None, _ | _, None ->
      print_endline "skipped: not macOS or Linux, or root, or no second group";
      Alcotest.skip ()
  | Some os, Some user ->
      ignore (Unix.umask 0o022);
      let tmp = Filename.temp_dir "conformance" "" in
      Fun.protect ~finally:(fun () -> remove tmp) @@ fun () ->
      let n = ref 0 in
      (* Opened, a directory is confined, as the recorder's cwd was, and takes
         the user's group, as the recorder's did. *)
      let fresh ~sw _ =
        incr n;
        let d = Filename.concat tmp (string_of_int !n) in
        Unix.mkdir d 0o755;
        Unix.chown d (-1) (Int64.to_int user.gid);
        Eio.Path.open_dir ~sw Eio.Path.(Eio.Stdenv.fs env / d)
      in
      let differs = Chaos.Private.eio_posix_differs os in
      let failed =
        match Chaos.device ~os ~user fresh with
        | () -> []
        | exception Failure r -> differing r
      in
      let only l l' = List.filter (fun x -> not (List.mem x l')) l in
      let wrong =
        List.map
          (fun n -> n ^ ": passes, the trace says it differs")
          (only differs failed)
        @ List.map
            (fun n -> n ^ ": differs, the trace says it passes")
            (only failed differs)
      in
      if wrong <> [] then
        Alcotest.failf "eio_posix on %s:@\n%s"
          (match os with `Linux -> "Linux" | `Macos -> "macOS")
          (String.concat "\n" wrong)

let () =
  Alcotest.run "eio-fs.kernel"
    [
      ( "memfs",
        [
          Alcotest.test_case "Linux" `Quick (memfs `Linux);
          Alcotest.test_case "macOS" `Quick (memfs `Macos);
          Alcotest.test_case "Linux, as root" `Quick (memfs_root `Linux);
          Alcotest.test_case "macOS, as root" `Quick (memfs_root `Macos);
          Alcotest.test_case "Windows, as a basic user" `Quick (memfs `Windows);
          Alcotest.test_case "Windows, as an administrator" `Quick
            (memfs_root `Windows);
          Alcotest.test_case "an overlay over env#fs, Linux" `Quick
            (overlay `Linux);
          Alcotest.test_case "an overlay over env#fs, macOS" `Quick
            (overlay `Macos);
          Alcotest.test_case "an overlay over env#fs, Windows" `Quick
            (overlay `Windows);
        ] );
      ( "device",
        [
          Alcotest.test_case "a stat fault" `Quick test_stat_fault;
          Alcotest.test_case "the fault agrees with Linux" `Quick
            test_stat_fault_linux;
          Alcotest.test_case "an error on one line" `Quick test_one_line;
          Alcotest.test_case "where eio_posix differs too" `Quick
            test_eio_posix_too;
          Alcotest.test_case "the other os" `Quick test_wrong_os;
          Alcotest.test_case "left out" `Quick test_left_out;
          Alcotest.test_case "refused arguments" `Quick test_refused;
          Alcotest.test_case "columns found by name" `Quick test_columns_by_name;
        ] );
      ("host", [ Alcotest.test_case "eio_posix" `Quick test_host ]);
    ]
