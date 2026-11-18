(* A stat answers times, link counts and block counts as Linux does: the
   expected answers are recorded in traces/linux.csv, as README.md says. *)

let ( / ) = Eio.Path.( / )

(* A clock that moves only when the test moves it. *)
module Manual = struct
  type t = float ref
  type time = float

  let now t = !t
  let sleep_until _ _ = invalid_arg "Manual.sleep_until"
end

let manual () =
  let t = ref 0. in
  (t, Eio.Resource.T (t, Eio.Time.Pi.clock (module Manual)))

let with_fs fn =
  Eio_mock.Backend.run @@ fun () ->
  let t, clock = manual () in
  fn t (Eio_fs_mem.v ~clock ())

let trace = "traces/linux.csv"

(* The filesystems the trace names, and its rows: a workload, then its line
   on each filesystem. *)
let linux =
  lazy
    (match
       In_channel.with_open_text trace In_channel.input_lines
       |> List.filter (fun l -> not (String.starts_with ~prefix:"#" l))
       |> List.map (String.split_on_char '|')
     with
    | ("workload" :: fss) :: rows -> (fss, rows)
    | _ -> Fmt.failwith "%s: no header" trace)

(* What the workload [name] answers on the filesystem [fs]. *)
let answer name fs =
  let fss, rows = Lazy.force linux in
  match List.find_index (String.equal fs) fss with
  | None -> Alcotest.failf "%s records nothing on %s: regenerate it" trace fs
  | Some i ->
      List.filter_map
        (function w :: l when w = name -> Some (List.nth l i) | _ -> None)
        rows

(* The workload [name] on the filesystem [root]; each tick is a second. *)
let run name t root =
  (List.assoc name Stat_workload.all)
    ~tick:(fun () -> t := !t +. 1.)
    (root :> Eio.Fs.dir_ty Eio.Path.t)

let check name ~on got =
  List.iter
    (fun fs ->
      Alcotest.(check (list string)) ("as on " ^ fs) (answer name fs) got)
    on

let as_linux name ~on () =
  with_fs @@ fun t root -> check name ~on (run name t root)

(* Directories hold no blocks on tmpfs and one on ext4: the counts follow
   tmpfs. *)
let test_counts () =
  with_fs @@ fun t root ->
  check "counts" ~on:[ "tmpfs" ] (run "counts" t root);
  let in_d =
    List.filter_map
      (fun l ->
        Scanf.sscanf l "%s nlink=%_d blocks=%Ld" (fun n b ->
            if n = "d" || String.starts_with ~prefix:"d/" n then Some b
            else None))
      (answer "counts" "tmpfs")
  in
  Alcotest.(check int64)
    "usage"
    (Int64.mul 512L (List.fold_left Int64.add 0L in_d))
    (Eio_fs.Tree.usage (root / "d")).allocated_bytes

let test_trace () =
  Provenance.check ~trace
    ~keys:
      [
        "script";
        "script revision";
        "script digest";
        "recorded";
        "linux os";
        "linux image";
        "linux user";
        "linux eio_posix";
        "tmpfs";
        "ext4";
      ]
    ~code:[ "scripts/record.ml"; "workload/stat_workload.ml" ]
    ~script:"scripts/generate.sh"

let suite =
  ( "stat",
    [
      Alcotest.test_case "the trace names its provenance" `Quick test_trace;
      Alcotest.test_case "times move as on Linux" `Quick
        (as_linux "times" ~on:[ "tmpfs"; "ext4" ]);
      Alcotest.test_case "times of moved and removed inodes" `Quick
        (as_linux "inode times" ~on:[ "tmpfs"; "ext4" ]);
      Alcotest.test_case "link and block counts" `Quick test_counts;
    ] )
