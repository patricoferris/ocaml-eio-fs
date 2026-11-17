(* Conformance.check against the kernel traces: the in-memory filesystem
   passes for Linux and for macOS, a directory with a fault fails with a
   report, and eio_posix on this machine fails exactly where its recorded
   answers differ from the kernel's. README.md says where the traces come
   from. *)

module Conformance = Eio_fs.Conformance

type os = [ `Linux | `Macos ]

let os_name : os -> string = function `Linux -> "Linux" | `Macos -> "macOS"

let user =
  { Eio_fs.uid = 1000L; gid = 1000L; groups = [ 4242L ]; umask = 0o022 }

let memfs (os : os) name () =
  Eio_mock.Backend.run @@ fun () ->
  Conformance.check ~os ~only:[ name ] ~user (fun () ->
      Eio_fs.Fs.v ~os ~user ())

let failure f =
  match f () with
  | () -> Alcotest.fail "the check passed"
  | exception Failure report -> report

(* "stat A" after "write a x" finds the file on macOS: a stat of [A] that
   answers ENOENT differs there. *)
let no_stat_of_a (op : Eio_fs.Chaos.Op.t) =
  if op.kind = `Stat && Filename.basename op.path = "A" then
    Some (Eio_fs.Chaos.Fault.Fail `Enoent)
  else None

let test_stat_fault () =
  Eio_mock.Backend.run @@ fun () ->
  let report =
    failure @@ fun () ->
    Conformance.check ~os:`Macos ~user (fun () ->
        Eio_fs.Chaos.wrap no_stat_of_a (Eio_fs.Fs.v ~os:`Macos ~user ()))
  in
  Alcotest.(check string)
    "report"
    {|Eio_fs.Conformance.check: "case lookup" differs from macOS at operation 2
  scenario:  case lookup
  operation: 2, stat A
  macOS:     file 1
  directory: ENOENT
  trace:
    1 write a x  ok
    2 stat A     ENOENT
  replay:    ~only:["case lookup"]|}
    report

(* Linux does not find [A] either: the same directory passes there. *)
let test_stat_fault_linux () =
  Eio_mock.Backend.run @@ fun () ->
  Conformance.check ~os:`Linux ~only:[ "case lookup" ] ~user (fun () ->
      Eio_fs.Chaos.wrap no_stat_of_a (Eio_fs.Fs.v ~user ()))

(* Under [`Linux] the filesystem keeps [a] and [A] apart, as macOS does
   not. *)
let test_wrong_os () =
  Eio_mock.Backend.run @@ fun () ->
  let report =
    failure @@ fun () ->
    Conformance.check ~os:`Macos ~user (fun () ->
        Eio_fs.Fs.v ~os:`Linux ~user ())
  in
  Alcotest.(check bool)
    "names the first scenario" true
    (String.starts_with
       ~prefix:
         {|Eio_fs.Conformance.check: "case lookup" differs from macOS at operation 2|}
       report)

let invalid name f =
  match f () with
  | () -> Alcotest.failf "%s: the check passed" name
  | exception Invalid_argument _ -> ()

let test_refused () =
  Eio_mock.Backend.run @@ fun () ->
  let fs () = Eio_fs.Fs.v ~user () in
  invalid "root" (fun () -> Conformance.check ~user:Eio_fs.root fs);
  invalid "umask" (fun () ->
      Conformance.check ~user:{ user with umask = 0o077 } fs);
  invalid "no supplementary group" (fun () ->
      Conformance.check ~user:{ user with groups = [ 1000L ] } fs);
  invalid "unknown scenario" (fun () ->
      Conformance.check ~only:[ "no such scenario" ] ~user fs)

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
          Eio_fs.uid = Int64.of_int (Unix.getuid ());
          gid = Int64.of_int gid;
          groups = [ Int64.of_int other ];
          umask = 0o022;
        }
  | _ -> None

let remove dir =
  ignore (Sys.command (Filename.quote_command "chmod" [ "-R"; "u+rwx"; dir ]));
  ignore (Sys.command (Filename.quote_command "rm" [ "-rf"; dir ]))

let test_host () =
  match (host_os (), host_user ()) with
  | None, _ | _, None ->
      print_endline "skipped: not macOS or Linux, or root, or no second group";
      Alcotest.skip ()
  | Some os, Some user ->
      ignore (Unix.umask 0o022);
      let tmp = Filename.temp_dir "conformance" "" in
      Fun.protect ~finally:(fun () -> remove tmp) @@ fun () ->
      Eio_posix.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      let n = ref 0 in
      (* Opened, a directory is confined, as the recorder's cwd was, and takes
         the user's group, as the recorder's did. *)
      let fresh () =
        incr n;
        let d = Filename.concat tmp (string_of_int !n) in
        Unix.mkdir d 0o755;
        Unix.chown d (-1) (Int64.to_int user.gid);
        Eio.Path.open_dir ~sw Eio.Path.(Eio.Stdenv.fs env / d)
      in
      let differs = Conformance.eio_posix_differs os in
      let wrong =
        List.filter_map
          (fun name ->
            let failed =
              match Conformance.check ~os ~only:[ name ] ~user fresh with
              | () -> None
              | exception Failure r -> Some r
            in
            match (failed, List.mem name differs) with
            | None, false | Some _, true -> None
            | None, true ->
                Fmt.kstr Option.some "%s: passes, the trace says it differs"
                  name
            | Some r, false -> Some r)
          Conformance.scenarios
      in
      if wrong <> [] then
        Alcotest.failf "eio_posix on %s:@\n%s" (os_name os)
          (String.concat "\n" wrong)

(* record.ml and eio_fs_kernel.ml decide every outcome; generate.sh decides
   only where they run, which the other lines record. *)
let test_provenance () =
  Provenance.check ~trace:"../../lib/kernel/fs.csv"
    ~keys:
      [
        "script";
        "script revision";
        "script digest";
        "recorded";
        "macos os";
        "macos filesystem";
        "macos mount";
        "macos user";
        "macos eio_posix";
        "linux os";
        "linux image";
        "linux filesystem";
        "linux mount";
        "linux user";
        "linux eio_posix";
      ]
    ~code:[ "scripts/record.ml"; "../../lib/kernel/eio_fs_kernel.ml" ]
    ~script:"scripts/generate.sh"

let suite (os : os) =
  ( os_name os,
    List.map
      (fun name -> Alcotest.test_case name `Quick (memfs os name))
      Conformance.scenarios )

let () =
  Alcotest.run "os"
    [
      ( "trace",
        [ Alcotest.test_case "names its provenance" `Quick test_provenance ] );
      suite `Linux;
      suite `Macos;
      ( "check",
        [
          Alcotest.test_case "a stat fault" `Quick test_stat_fault;
          Alcotest.test_case "the fault agrees with Linux" `Quick
            test_stat_fault_linux;
          Alcotest.test_case "the other os" `Quick test_wrong_os;
          Alcotest.test_case "refused arguments" `Quick test_refused;
        ] );
      ("host", [ Alcotest.test_case "eio_posix" `Quick test_host ]);
    ]
