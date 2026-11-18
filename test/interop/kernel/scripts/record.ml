(* Records each scenario's outcomes, one CSV row per scenario: its
   operations, separated by ";", then their outcomes straight from the kernel
   through [Unix], and through Eio's backend, each separated by "|". On
   Windows, [Unix] goes through the C runtime, not straight to the kernel, so
   only Eio's backend, eio_windows, is recorded.

   record LABEL...          records in the current directory, which must be
                            empty, as the user running it, in columns
                            named LABEL and LABEL eio_posix, or LABEL
                            eio_windows on Windows
   record merge TRACE R...  is the trace TRACE with the columns the
                            recordings R hold replaced by theirs, a row
                            per scenario: an answer neither they nor TRACE
                            hold is not recorded *)

module K = Eio_fs_kernel

(* The operations of a scenario through [Unix], straight to the kernel. [-]
   marks one [Unix] cannot make. *)

let unix_errno (e : Unix.error) =
  match e with
  | ENOENT -> "ENOENT"
  | EEXIST -> "EEXIST"
  | EACCES | EPERM -> "denied"
  | ELOOP -> "ELOOP"
  | EFBIG -> "EFBIG"
  | EOPNOTSUPP -> "ENOTSUP"
  | e -> (
      let s = Unix.error_message e in
      match K.errno_of_message s with Some n -> n | None -> "unix " ^ s)

let unix_kind (st : Unix.LargeFile.stats) =
  match st.st_kind with
  | S_REG -> Fmt.str "file %Ld" st.st_size
  | S_DIR -> "dir"
  | S_LNK -> "link"
  | _ -> "other"

let unix_names p =
  let d = Unix.opendir p in
  let rec go acc =
    match Unix.readdir d with
    | "." | ".." -> go acc
    | n -> go (n :: acc)
    | exception End_of_file -> acc
  in
  let l = Fun.protect ~finally:(fun () -> Unix.closedir d) (fun () -> go []) in
  K.names (List.sort String.compare l)

let unix_open p flags =
  let has c = String.contains flags c in
  let flags =
    (if has 'r' then [ Unix.O_RDONLY ] else [ Unix.O_RDWR ])
    @ (if has 'x' then [ Unix.O_CREAT; O_EXCL ]
       else if has 't' then [ Unix.O_CREAT; O_TRUNC ]
       else if has 'c' then [ Unix.O_CREAT ]
       else [])
    @ if has 'a' then [ Unix.O_APPEND ] else []
  in
  Unix.close (Unix.openfile p (Unix.O_CLOEXEC :: flags) 0o644)

(* [held] closes what an operation holds open; [base] is where a path
   starts. *)
let unix_op ~root ~held ~base (ids : K.ids) line =
  let ( / ) d p =
    if p = "" then d ^ "/"
    else if Filename.is_relative p then d ^ "/" ^ p
    else p
  in
  let dir = !base in
  let ok () = "ok" in
  let with_fd p flags f =
    let fd = Unix.openfile p flags 0o644 in
    Fun.protect ~finally:(fun () -> Unix.close fd) (fun () -> f fd)
  in
  match K.words ~root line with
  | [ "write"; p; data ] ->
      with_fd (dir / p) [ O_WRONLY; O_CREAT; O_TRUNC ] (fun fd ->
          ignore (Unix.write_substring fd data 0 (String.length data)));
      ok ()
  | [ "read"; p ] ->
      with_fd (dir / p) [ O_RDONLY ] (fun fd ->
          let buf = Buffer.create 64 and b = Bytes.create 4096 in
          let rec go () =
            match Unix.read fd b 0 4096 with
            | 0 -> String.escaped (Buffer.contents buf)
            | n ->
                Buffer.add_subbytes buf b 0 n;
                go ()
          in
          go ())
  | [ "readd"; p ] ->
      with_fd (dir / p) [ O_RDONLY ] (fun fd ->
          string_of_int (Unix.read fd (Bytes.create 16) 0 16))
  (* [Unix] binds no pwrite, lchmod, O_NOFOLLOW or mkdirs, and no directory
     that confines the paths beneath it: [cd] only moves where they start. *)
  | [ "pwa"; _; _; _ ] | [ "lchmod"; _; _ ] | [ "mkdirs"; _ ] -> "-"
  | [ "cd"; p ] ->
      base := dir / p;
      "-"
  | [ "hold"; p ] ->
      let fd = Unix.openfile (dir / p) [ O_RDONLY; O_CLOEXEC ] 0 in
      held := (p, fun () -> Unix.close fd) :: !held;
      ok ()
  | [ "holddir"; p ] ->
      let d = Unix.opendir (dir / p) in
      held := (p, fun () -> Unix.closedir d) :: !held;
      ok ()
  | [ "close"; p ] -> (
      match List.assoc_opt p !held with
      | None -> raise (K.Bad_op line)
      | Some close ->
          held := List.remove_assoc p !held;
          close ();
          ok ())
  | [ "open"; _; flags ] when String.contains flags 'n' -> "-"
  | [ "open"; p; flags ] ->
      unix_open (dir / p) flags;
      ok ()
  | [ "opendir"; p ] ->
      Unix.closedir (Unix.opendir (dir / p));
      ok ()
  | [ "lsd"; p ] | [ "ls"; p ] -> unix_names (dir / p)
  | [ "mkdir"; p ] ->
      Unix.mkdir (dir / p) 0o755;
      ok ()
  | [ "mkdir"; p; m ] ->
      Unix.mkdir (dir / p) (K.perm m);
      ok ()
  | [ "unlink"; p ] ->
      Unix.unlink (dir / p);
      ok ()
  | [ "rmdir"; p ] ->
      Unix.rmdir (dir / p);
      ok ()
  | [ "rename"; a; b ] ->
      Unix.rename (dir / a) (dir / b);
      ok ()
  | [ "symlink"; target; p ] ->
      Unix.symlink target (dir / p);
      ok ()
  | [ "readlink"; p ] -> String.escaped (Unix.readlink (dir / p))
  | [ "stat"; p ] -> unix_kind (Unix.LargeFile.stat (dir / p))
  | [ "lstat"; p ] -> unix_kind (Unix.LargeFile.lstat (dir / p))
  | [ "mode"; p ] -> Fmt.str "%o" (Unix.LargeFile.lstat (dir / p)).st_perm
  | [ "group"; p ] ->
      let g = (Unix.LargeFile.lstat (dir / p)).st_gid in
      if g = ids.gid then "primary"
      else if g = ids.other_gid then "other"
      else "foreign"
  | [ "chmod"; p; m ] ->
      Unix.chmod (dir / p) (K.perm m);
      ok ()
  | [ "chgrp"; p ] ->
      Unix.chown (dir / p) (-1) ids.other_gid;
      ok ()
  | [ "chain"; n ] ->
      if not (Sys.file_exists (dir / "f")) then
        with_fd (dir / "f") [ O_WRONLY; O_CREAT; O_EXCL ] (fun fd ->
            ignore (Unix.write_substring fd "x" 0 1));
      for i = 1 to int_of_string n do
        let target = if i = 1 then "f" else Fmt.str "l%d" (i - 1) in
        Unix.symlink target (dir / Fmt.str "l%d" i)
      done;
      ok ()
  | _ -> raise (K.Bad_op line)

let run_unix ~root ids dir ops =
  let held = ref [] and base = ref dir in
  Fun.protect ~finally:(fun () -> List.iter (fun (_, close) -> close ()) !held)
  @@ fun () ->
  List.map
    (fun line ->
      try unix_op ~root ~held ~base ids line with
      | K.Bad_op _ as e -> raise e
      | Unix.Unix_error (e, _, _) -> unix_errno e
      | End_of_file -> "eof")
    ops

let clean s = String.map (function ',' | '\n' | '\r' -> ' ' | c -> c) s
let row l = clean (String.concat "|" l)
let here = Sys.getcwd ()

let record_eio ids ~cwd i ops =
  let dir = Eio.Path.(cwd / string_of_int i) in
  Eio.Path.mkdir ~perm:0o755 dir;
  K.run ~root:(Filename.concat here (string_of_int i)) ids dir ops

let record_posix label =
  ignore (Unix.umask 0o022);
  let gid = Unix.getgid () in
  (* The directory takes the user's group: macOS gives a new entry its
     directory's. *)
  Unix.chown "." (-1) gid;
  let other_gid =
    match List.filter (( <> ) gid) (Array.to_list (Unix.getgroups ())) with
    | g :: _ -> g
    | [] -> failwith "record: the user needs a supplementary group"
  in
  let ids = { K.gid; other_gid } in
  Backend.run @@ fun cwd ->
  Fmt.pr "ops,%s,%s eio_posix@." label label;
  List.iteri
    (fun i (_, ops) ->
      let k = Fmt.str "k%d" i in
      Unix.mkdir k 0o755;
      let kernel = run_unix ~root:(Filename.concat here k) ids k ops in
      let eio = record_eio ids ~cwd i ops in
      Fmt.pr "%s,%s,%s@." (K.key ops) (row kernel) (row eio))
    K.scenarios

(* Windows has no groups an outcome could name. *)
let record_windows label =
  let ids = { K.gid = -1; other_gid = -2 } in
  Backend.run @@ fun cwd ->
  Fmt.pr "ops,%s eio_windows@." label;
  List.iteri
    (fun i (_, ops) ->
      Fmt.pr "%s,%s@." (K.key ops) (row (record_eio ids ~cwd i ops)))
    K.scenarios

(* A trace or a recording: its column names, and each row's outcomes by
   column, keyed by its operations. *)
let read file =
  match In_channel.with_open_bin file In_channel.input_lines with
  | [] -> failwith ("record merge: empty " ^ file)
  | header :: lines ->
      let split l = String.split_on_char ',' (String.trim l) in
      let columns = List.tl (split header) in
      let rows =
        List.filter_map
          (fun line ->
            match split line with
            | ops :: cols when List.length cols = List.length columns ->
                Some (ops, List.combine columns cols)
            | [ "" ] -> None
            | _ -> failwith ("record merge: " ^ line))
          lines
      in
      (columns, rows)

(* The trace's columns, in order: a host records some of them, and the
   others are kept from the trace before. *)
let columns =
  [
    "linux";
    "macos";
    "linux eio_posix";
    "macos eio_posix";
    "windows eio_windows";
    "windows basic user eio_windows";
  ]

let merge trace recordings =
  let recordings = List.map read recordings in
  let before = if Sys.file_exists trace then snd (read trace) else [] in
  let answer ops column =
    let from rows =
      Option.bind (List.assoc_opt ops rows) (List.assoc_opt column)
    in
    match List.find_opt (fun (cols, _) -> List.mem column cols) recordings with
    | Some (_, rows) -> Option.value ~default:"not recorded" (from rows)
    | None -> Option.value ~default:"not recorded" (from before)
  in
  print_endline (String.concat "," ("ops" :: columns));
  List.iter
    (fun (_, ops) ->
      let ops = K.key ops in
      print_endline (String.concat "," (ops :: List.map (answer ops) columns)))
    K.scenarios

let () =
  set_binary_mode_out stdout true;
  match Array.to_list Sys.argv with
  | _ :: "merge" :: trace :: (_ :: _ as recordings) -> merge trace recordings
  | _ :: (_ :: _ as label) when Sys.win32 ->
      record_windows (String.concat " " label)
  | _ :: (_ :: _ as label) -> record_posix (String.concat " " label)
  | _ ->
      prerr_endline
        "usage: record LABEL | record merge TRACE RECORDING RECORDING";
      exit 2
