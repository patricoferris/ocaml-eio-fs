(* Records each scenario's outcomes, one CSV row per scenario: its
   operations, separated by ";", then their outcomes straight from the kernel
   through [Unix], and through eio_posix, each separated by "|".

   record                   records in the current directory, which must be
                            empty, as the user running it
   record merge LINUX MACOS joins two recordings of the same scenarios *)

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

let unix_op (ids : K.ids) dir line =
  let ( / ) d p = if p = "" then d ^ "/" else d ^ "/" ^ p in
  let ok () = "ok" in
  let with_fd p flags f =
    let fd = Unix.openfile p flags 0o644 in
    Fun.protect ~finally:(fun () -> Unix.close fd) (fun () -> f fd)
  in
  match K.words line with
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
  (* [Unix] binds no pwrite, lchmod or O_NOFOLLOW. *)
  | [ "pwa"; _; _; _ ] | [ "lchmod"; _; _ ] -> "-"
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

let run_unix ids dir ops =
  List.map
    (fun line ->
      try unix_op ids dir line with
      | K.Bad_op _ as e -> raise e
      | Unix.Unix_error (e, _, _) -> unix_errno e
      | End_of_file -> "eof")
    ops

let clean s = String.map (function ',' | '\n' -> ' ' | c -> c) s

let record () =
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
  Eio_posix.run @@ fun env ->
  let cwd = Eio.Stdenv.cwd env in
  print_endline "ops,kernel,eio";
  let row l = clean (String.concat "|" l) in
  List.iteri
    (fun i (_, ops) ->
      let k = Fmt.str "k%d" i in
      Unix.mkdir k 0o755;
      let kernel = run_unix ids k ops in
      let dir = Eio.Path.(cwd / string_of_int i) in
      Eio.Path.mkdir ~perm:0o755 dir;
      let eio = K.run ids dir ops in
      Fmt.pr "%s,%s,%s@." (K.key ops) (row kernel) (row eio))
    K.scenarios

let rows file =
  In_channel.with_open_text file In_channel.input_lines
  |> List.tl
  |> List.map (fun line ->
      match String.split_on_char ',' line with
      | [ ops; kernel; eio ] -> (ops, (kernel, eio))
      | _ -> failwith ("record merge: " ^ line))

let merge linux macos =
  let macos = rows macos in
  print_endline "ops,linux,macos,linux eio_posix,macos eio_posix";
  List.iter
    (fun (ops, (lk, le)) ->
      let mk, me =
        Option.value
          ~default:("not recorded", "not recorded")
          (List.assoc_opt ops macos)
      in
      Fmt.pr "%s,%s,%s,%s,%s@." ops lk mk le me)
    (rows linux)

let () =
  match Array.to_list Sys.argv with
  | [ _ ] -> record ()
  | [ _; "merge"; linux; macos ] -> merge linux macos
  | _ ->
      prerr_endline "usage: record | record merge LINUX MACOS";
      exit 2
