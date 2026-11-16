type t = tick:(unit -> unit) -> Eio.Fs.dir_ty Eio.Path.t -> string list

let ( / ) = Eio.Path.( / )
let stat p = Eio.Path.stat ~follow:false p
let int = Optint.Int63.of_int

(* Which of atime, mtime and ctime moved, and how the link count did. *)
let moved (b : Eio.File.Stat.t) (a : Eio.File.Stat.t) =
  (if a.atime <> b.atime then "a" else "")
  ^ (if a.mtime <> b.mtime then "m" else "")
  ^ (if a.ctime <> b.ctime then "c" else "")
  ^ if a.nlink <> b.nlink then Fmt.str " nlink %Ld->%Ld" b.nlink a.nlink else ""

(* Each operation runs after a tick; its line is its label and what moved on
   each watched path that exists before and after it. *)
let script ~tick watched ops =
  let snap () =
    List.map
      (fun (n, p) -> (n, try Some (stat p) with Eio.Io _ -> None))
      watched
  in
  List.map
    (fun (label, op) ->
      tick ();
      let before = snap () in
      (try op () with End_of_file -> ());
      let changes =
        List.filter_map
          (fun ((n, b), (_, a)) ->
            match (b, a) with
            | Some b, Some a when moved b a <> "" -> Some (n ^ ":" ^ moved b a)
            | _ -> None)
          (List.combine before (snap ()))
      in
      String.concat " " (label :: changes))
    ops

let times ~tick root =
  let d = root / "d" and e = root / "e" in
  Eio.Path.mkdir ~perm:0o755 d;
  Eio.Path.mkdir ~perm:0o755 e;
  let f = d / "f" and g = d / "g" and l = d / "l" and sub = d / "sub" in
  Eio.Switch.run @@ fun sw ->
  let wf = ref None and rf = ref None in
  let w () = Option.get !wf and r () = Option.get !rf in
  let open_w () = wf := Some (Eio.Path.open_out ~sw ~create:`Never f) in
  let open_r () = rf := Some (Eio.Path.open_in ~sw f) in
  script ~tick
    [ ("d", d); ("e", e); ("f", f); ("l", l) ]
    [
      ("create f", fun () -> Eio.Path.save ~create:(`Exclusive 0o644) f "");
      ( "write 3",
        fun () ->
          open_w ();
          Eio.Flow.copy_string "abc" (w ()) );
      ("write 0", fun () -> ignore (Eio.Flow.single_write (w ()) []));
      ( "pwrite 1",
        fun () ->
          Eio.File.pwrite_all (w ()) ~file_offset:(int 1)
            [ Cstruct.of_string "x" ] );
      ("truncate to the same size", fun () -> Eio.File.truncate (w ()) (int 3));
      ("truncate 10", fun () -> Eio.File.truncate (w ()) (int 10));
      ("fsync", fun () -> Eio.File.sync (w ()));
      ("fstat", fun () -> ignore (Eio.File.stat (w ())));
      ( "read 1",
        fun () ->
          open_r ();
          ignore (Eio.Flow.single_read (r ()) (Cstruct.create 1)) );
      ( "read 1 again",
        fun () -> ignore (Eio.Flow.single_read (r ()) (Cstruct.create 1)) );
      ( "pread at the end",
        fun () ->
          ignore
            (Eio.File.pread (r ()) ~file_offset:(int 100) [ Cstruct.create 1 ])
      );
      ( "open to truncate",
        fun () ->
          Eio.Switch.run @@ fun sw ->
          ignore (Eio.Path.open_out ~sw ~create:(`Or_truncate 0o644) f) );
      ( "open to truncate, empty",
        fun () ->
          Eio.Switch.run @@ fun sw ->
          ignore (Eio.Path.open_out ~sw ~create:(`Or_truncate 0o644) f) );
      ( "open to read",
        fun () -> Eio.Switch.run @@ fun sw -> ignore (Eio.Path.open_in ~sw f) );
      ( "open to write",
        fun () ->
          Eio.Switch.run @@ fun sw ->
          ignore (Eio.Path.open_out ~sw ~create:`Never f) );
      ( "chmod to the same mode",
        fun () -> Eio.Path.chmod ~follow:false ~perm:0o644 f );
      ("chmod 600", fun () -> Eio.Path.chmod ~follow:false ~perm:0o600 f);
      ( "chown to the same owner",
        fun () ->
          let s = stat f in
          Eio.Path.chown ~follow:false ~uid:s.uid ~gid:s.gid f );
      ("chown nothing", fun () -> Eio.Path.chown ~follow:false f);
      ("stat", fun () -> ignore (Eio.Path.stat ~follow:true f));
      ("mkdir sub", fun () -> Eio.Path.mkdir ~perm:0o755 sub);
      ("symlink l", fun () -> Eio.Path.symlink ~link_to:"f" l);
      ("readlink l", fun () -> ignore (Eio.Path.read_link l));
      ("readlink l again", fun () -> ignore (Eio.Path.read_link l));
      ("read_dir d", fun () -> ignore (Eio.Path.read_dir d));
      ("read_dir d again", fun () -> ignore (Eio.Path.read_dir d));
      ("rename f g", fun () -> Eio.Path.rename f g);
      ("rename g f", fun () -> Eio.Path.rename g f);
      ("rename d/sub e/sub", fun () -> Eio.Path.rename sub (e / "sub"));
      ("rename e/sub d/sub", fun () -> Eio.Path.rename (e / "sub") sub);
      ("rename d/sub d/sub", fun () -> Eio.Path.rename sub sub);
      ("rmdir sub", fun () -> Eio.Path.rmdir sub);
      ("unlink l", fun () -> Eio.Path.unlink l);
    ]

(* The entry a rename moves, the one it replaces and the one an unlink
   removes, each stat'ed through a descriptor open on it. *)
let inode_times ~tick root =
  let f = root / "f" and g = root / "g" and h = root / "h" in
  List.iter
    (fun p -> Eio.Path.save ~create:(`Exclusive 0o644) p "x")
    [ f; g; h ];
  Eio.Switch.run @@ fun sw ->
  let fds = List.map (fun p -> Eio.Path.open_in ~sw p) [ f; g; h ] in
  let before = List.map Eio.File.stat fds in
  tick ();
  Eio.Path.rename f g;
  Eio.Path.unlink h;
  let after = List.map Eio.File.stat fds in
  List.map2
    (fun n m -> n ^ " " ^ m)
    [ "moved"; "replaced"; "unlinked" ]
    (List.map2 moved before after)

let counts ~tick:_ root =
  let d = root / "d" in
  Eio.Path.mkdir ~perm:0o755 d;
  let p = ( / ) d in
  Eio.Path.mkdir ~perm:0o755 (p "s1");
  Eio.Path.mkdir ~perm:0o755 (p "s2");
  Eio.Path.save ~create:(`Exclusive 0o644) (p "one") "x";
  Eio.Path.save ~create:(`Exclusive 0o644) (p "empty") "";
  Eio.Path.save ~create:(`Exclusive 0o644) (p "big") (String.make 5000 'x');
  ( Eio.Switch.run @@ fun sw ->
    let file n = Eio.Path.open_out ~sw ~create:(`Exclusive 0o644) (p n) in
    Eio.File.pwrite_all (file "sparse") ~file_offset:(int 1_000_000)
      [ Cstruct.of_string "y" ];
    Eio.File.truncate (file "trunc") (int 100_000);
    let cut = file "cut" in
    Eio.File.pwrite_all cut ~file_offset:Optint.Int63.zero
      [ Cstruct.of_string (String.make 9000 'z') ];
    Eio.File.truncate cut (int 4097) );
  Eio.Path.symlink ~link_to:"one" (p "ln");
  List.map
    (fun n ->
      let s = stat (if n = "." then root else root / n) in
      Fmt.str "%s nlink=%Ld blocks=%Ld" n s.nlink s.blocks)
    [
      ".";
      "d";
      "d/one";
      "d/empty";
      "d/big";
      "d/sparse";
      "d/trunc";
      "d/cut";
      "d/ln";
      "d/s1";
      "d/s2";
    ]

let all = [ ("times", times); ("inode times", inode_times); ("counts", counts) ]
