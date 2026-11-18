(* Each operation is a line of words. A word [p{s}n] is [p] followed by [s]
   repeated [n] times; [-] is the empty string; [\NNN] is the byte of
   decimal code [NNN], a space included; [@p] is [p] under the scenario's
   absolute path. *)

type ids = { gid : int; other_gid : int }

(* NFC and NFD spellings of e-acute, and other names a fold changes. *)
let e_nfc = "\xc3\xa9"
let e_upper = "\xc3\x89"
let e_nfd = "e\xcc\x81"
let sharp_s = "\xc3\x9f"
let kelvin = "\xe2\x84\xaa"
let e_upper_nfd = "E\xcc\x81"
let smiley = "\xf0\x9f\x98\x80"

let scenarios =
  [
    (* Names: case and Unicode normalisation. *)
    ("case lookup", [ "write a x"; "stat A"; "read A"; "ls ." ]);
    ("case exclusive create", [ "write a x"; "open A x"; "ls ." ]);
    ("case create", [ "write a x"; "write A y"; "read a"; "ls ." ]);
    ("case mkdir", [ "mkdir d"; "mkdir D"; "write d/f x"; "ls D"; "ls ." ]);
    ("case rename", [ "write a x"; "rename a A"; "ls ."; "read a" ]);
    ("case rename dir", [ "mkdir d"; "rename d D"; "ls ." ]);
    ( "case rename over",
      [ "write a x"; "write b y"; "rename b A"; "ls ."; "read a" ] );
    ("case rename dir over", [ "mkdir a"; "mkdir B"; "rename a b"; "ls ." ]);
    ("case unlink", [ "write a x"; "unlink A"; "ls ." ]);
    ("case rmdir", [ "mkdir d"; "rmdir D"; "ls ." ]);
    ("case symlink", [ "symlink a l"; "symlink b L"; "readlink L"; "ls ." ]);
    ("case component", [ "mkdir d"; "write D/f x"; "ls d" ]);
    ("case non-ascii", [ "write " ^ e_nfc ^ " x"; "stat " ^ e_upper; "ls ." ]);
    ("case sharp s", [ "write " ^ sharp_s ^ " x"; "stat ss"; "stat SS" ]);
    ("case kelvin", [ "write k x"; "stat " ^ kelvin; "write " ^ kelvin ^ " y" ]);
    ("nfd lookup", [ "write " ^ e_nfc ^ " x"; "stat " ^ e_nfd; "ls ." ]);
    ( "nfd create",
      [
        "write " ^ e_nfd ^ " x";
        "write " ^ e_nfc ^ " y";
        "ls .";
        "read " ^ e_nfd;
      ] );
    ( "nfd rename",
      [ "write " ^ e_nfc ^ " x"; "rename " ^ e_nfc ^ " " ^ e_nfd; "ls ." ] );
    ("nfd case", [ "write " ^ e_upper_nfd ^ " x"; "stat " ^ e_nfc; "ls ." ]);
    ("invalid utf-8", [ "write \xff x"; "mkdir \xfe"; "ls ." ]);
    ("invalid utf-8 lookup", [ "stat \xff"; "unlink \xff"; "rename \xff a" ]);
    (* Names: limits. *)
    ("name max", [ "write {a}255 x"; "stat {a}256"; "write {a}256 x" ]);
    ( "name max multibyte",
      [ "write {" ^ e_nfc ^ "}128 x"; "write {" ^ e_nfc ^ "}255 x"; "ls ." ] );
    ("name max 256 characters", [ "write {" ^ e_nfc ^ "}256 x"; "ls ." ]);
    ( "name max decomposed",
      [ "write {" ^ e_nfd ^ "}128 x"; "write {" ^ e_nfc ^ "}128 y"; "ls ." ] );
    ( "name max astral",
      [ "write {" ^ smiley ^ "}128 x"; "write {" ^ smiley ^ "}255 x"; "ls ." ]
    );
    ("path max", [ "stat {a/}520"; "stat {a/}2100"; "mkdir {a/}520" ]);
    ("nul byte", [ "stat a\000b"; "write a\000b x" ]);
    (* unlink, rmdir, mkdir. *)
    ("unlink dir", [ "mkdir d"; "unlink d"; "unlink d/"; "ls ." ]);
    ("unlink file slash", [ "write f x"; "unlink f/"; "ls ." ]);
    ("unlink dots", [ "mkdir d"; "unlink d/."; "unlink d/.."; "unlink ." ]);
    ("rmdir dot", [ "mkdir d"; "rmdir d/."; "ls ." ]);
    ("rmdir dot-dot", [ "mkdir d"; "mkdir d/e"; "rmdir d/e/.."; "ls ." ]);
    ("rmdir non-empty", [ "mkdir d"; "write d/f x"; "rmdir d" ]);
    ("rmdir file", [ "write f x"; "rmdir f"; "rmdir f/" ]);
    ("rmdir link", [ "mkdir d"; "symlink d l"; "rmdir l"; "ls ." ]);
    ("rmdir link slash", [ "mkdir d"; "symlink d l"; "rmdir l/"; "ls ." ]);
    ("unlink link slash", [ "mkdir d"; "symlink d l"; "unlink l/"; "ls ." ]);
    ("rename link slash", [ "mkdir d"; "symlink d l"; "rename l/ m"; "ls ." ]);
    ( "mkdir exists",
      [ "mkdir d"; "mkdir d"; "mkdir d/"; "write f x"; "mkdir f"; "mkdir f/" ]
    );
    ( "mkdir dangling link",
      [ "symlink nowhere l"; "mkdir l"; "mkdir l/"; "ls ." ] );
    ("mkdir slash", [ "mkdir n/"; "ls ." ]);
    ("mkdir under file", [ "write f x"; "mkdir f/x" ]);
    ("mkdir dots", [ "mkdir ."; "mkdir d"; "mkdir d/."; "mkdir d/.." ]);
    (* open. *)
    ( "open file",
      [
        "write f abc";
        "open f r";
        "open f w";
        "open f c";
        "open f x";
        "open f t";
        "read f";
      ] );
    ("open missing", [ "open m r"; "open m w"; "open m c"; "ls ." ]);
    ( "open dir",
      [ "mkdir d"; "open d r"; "open d w"; "open d c"; "open d x"; "open d t" ]
    );
    ( "open slash",
      [
        "write f x";
        "open f/ r";
        "open f/ c";
        "open n/ c";
        "mkdir d";
        "open d/ r";
        "open d/ c";
        "ls .";
      ] );
    ( "open nofollow",
      [
        "write f x";
        "symlink f l";
        "open l rn";
        "open l wn";
        "open l cn";
        "open l xn";
        "open l c";
      ] );
    ( "open dangling link",
      [ "symlink t l"; "open l c"; "ls ."; "symlink u m"; "open m x" ] );
    ("open dangling slash link", [ "symlink t/ l"; "open l c"; "ls ." ]);
    ( "open dir handle",
      [ "mkdir d"; "opendir d"; "write f x"; "opendir f"; "opendir m" ] );
    ("read dir", [ "mkdir d"; "readd d" ]);
    ( "pwrite append",
      [ "write f abc"; "pwa f XY 0"; "read f"; "pwa f Z 10"; "read f" ] );
    (* Symbolic links. *)
    ("symlink empty", [ "symlink - l"; "readlink l"; "stat l"; "ls ." ]);
    ( "symlink empty follow",
      [ "symlink - l"; "open l r"; "ls l"; "open l c"; "ls ." ] );
    ( "symlink exists",
      [ "write f x"; "symlink a f"; "symlink a f/"; "symlink a n/"; "ls ." ] );
    ("readlink not link", [ "write f x"; "readlink f"; "mkdir d"; "readlink d" ]);
    ("symlink chain 32", [ "chain 32"; "stat l32"; "open l32 r"; "open l32 c" ]);
    ("symlink chain 33", [ "chain 33"; "stat l33"; "open l33 r"; "open l33 c" ]);
    ("symlink chain 40", [ "chain 40"; "stat l40"; "open l40 r" ]);
    ("symlink chain 41", [ "chain 41"; "stat l41" ]);
    ("symlink loop", [ "symlink a b"; "symlink b a"; "stat a"; "open a c" ]);
    ( "trailing slash",
      [
        "write f x";
        "stat f/";
        "lstat f/";
        "symlink f l";
        "stat l/";
        "mkdir d";
        "symlink d m";
        "lstat m/";
        "stat f/.";
        "stat f/..";
      ] );
    (* rename. *)
    ( "rename into itself",
      [ "mkdir d"; "mkdir d/e"; "rename d d/e/x"; "rename d/e d/e"; "ls d" ] );
    ( "rename over non-empty",
      [ "mkdir a"; "mkdir b"; "write b/f x"; "rename a b"; "rename a b/" ] );
    ("rename kinds", [ "mkdir d"; "write f x"; "rename d f"; "rename f d" ]);
    ("rename dot", [ "mkdir d"; "rename d/. e"; "ls ." ]);
    ("rename dot-dot", [ "mkdir d"; "rename d/.. e"; "ls ." ]);
    ( "rename to dot",
      [
        "mkdir d";
        "write f x";
        "rename f d/.";
        "rename f d/..";
        "rename f .";
        "ls .";
      ] );
    ( "rename slash",
      [
        "write f x";
        "rename f/ g";
        "rename f g/";
        "mkdir d";
        "rename d/ e/";
        "ls .";
      ] );
    ("rename missing", [ "rename m n"; "write f x"; "rename f f"; "ls ." ]);
    ("rename above", [ "mkdir a"; "mkdir a/b"; "rename a/b a"; "ls a" ]);
    (* Modes and owners, as a user. *)
    ( "chmod link",
      [
        "write f x";
        "symlink f l";
        "lchmod l 700";
        "mode f";
        "chmod l 600";
        "mode f";
      ] );
    ( "dir no write",
      [ "mkdir d 500"; "write d/f x"; "mkdir d/e"; "ls d"; "chmod d 700" ] );
    ( "dir write and search",
      [
        "mkdir d";
        "chmod d 300";
        "write d/f x";
        "mkdir d/e";
        "stat d/f";
        "chmod d 700";
      ] );
    ( "dir no read",
      [
        "mkdir d";
        "write d/f x";
        "chmod d 300";
        "ls d";
        "stat d/f";
        "chmod d 700";
      ] );
    ( "dir no search",
      [
        "mkdir d";
        "write d/f x";
        "chmod d 600";
        "stat d/f";
        "ls d";
        "chmod d 700";
      ] );
    ( "dir search only",
      [
        "mkdir d";
        "write d/f x";
        "chmod d 100";
        "opendir d";
        "lsd d";
        "stat d/f";
        "chmod d 700";
      ] );
    ( "file no read",
      [ "write f x"; "chmod f 200"; "open f r"; "open f w"; "chmod f 600" ] );
    ( "file no write",
      [
        "write f x";
        "chmod f 400";
        "open f w";
        "open f t";
        "read f";
        "chmod f 600";
      ] );
    ( "group inherited",
      [
        "mkdir d";
        "chgrp d";
        "write d/f x";
        "group d/f";
        "mkdir d/e";
        "group d/e";
        "mode d/e";
      ] );
    ( "setgid dir",
      [
        "mkdir d";
        "chgrp d";
        "chmod d 2775";
        "write d/f x";
        "group d/f";
        "mkdir d/e";
        "group d/e";
        "mode d/e";
      ] );
    ( "write clears setuid",
      [ "write f x"; "chmod f 4755"; "mode f"; "write f y"; "mode f" ] );
    ("chgrp", [ "write f x"; "chgrp f"; "group f"; "chmod f 2755"; "mode f" ]);
    ( "mkdir modes",
      [ "mkdir d 7777"; "mode d"; "write f x"; "chmod f 7777"; "mode f" ] );
    (* Windows' traps. Reserved names, trailing dots and spaces. *)
    ( "reserved names",
      [
        "write NUL x";
        "write CON y";
        "write com1.txt z";
        "mkdir aux";
        "ls .";
        "read NUL";
      ] );
    ("trailing dot", [ "write a. x"; "stat a"; "read a."; "ls ." ]);
    ("trailing space", [ "write a\\032 x"; "stat a"; "read a\\032"; "ls ." ]);
    ("dots name", [ "write ... x"; "read ..."; "ls ." ]);
    ("link to dots", [ "write ... x"; "symlink ... l"; "read l"; "ls ." ]);
    (* What an open handle keeps from being moved or removed. *)
    ( "held file rename",
      [ "write f x"; "hold f"; "rename f g"; "read g"; "ls ." ] );
    ( "delete pending",
      [
        "write f x";
        "hold f";
        "unlink f";
        "ls .";
        "write f y";
        "close f";
        "write f z";
        "read f";
        "ls .";
      ] );
    ( "held file replaced",
      [ "write f x"; "write g y"; "hold f"; "rename g f"; "read f"; "ls ." ] );
    ( "held file dir rename",
      [ "mkdir d"; "write d/f x"; "hold d/f"; "rename d e"; "ls ." ] );
    ( "held file dir rmdir",
      [ "mkdir d"; "write d/f x"; "hold d/f"; "unlink d/f"; "rmdir d"; "ls ." ]
    );
    ("held dir rename", [ "mkdir d"; "holddir d"; "rename d e"; "ls ." ]);
    ("held dir rmdir", [ "mkdir d"; "holddir d"; "rmdir d"; "ls ." ]);
    (* [..] and links, in a directory opened as a sandbox. *)
    ( "link dot-dot",
      [
        "mkdir d";
        "mkdir d/e";
        "write x 1";
        "write d/x 2";
        "symlink d/e l";
        "read l/../x";
      ] );
    ( "dot-dot re-enter",
      [
        "mkdir d";
        "write d/f x";
        "stat d/../d/f";
        "cd d";
        "stat f";
        "stat ../d/f";
        "ls ..";
      ] );
    ( "absolute link",
      [ "write f x"; "symlink @f l"; "read l"; "lstat l"; "read @f" ] );
    ( "absolute link in sandbox",
      [ "mkdir d"; "write d/f x"; "symlink @d/f d/l"; "cd d"; "read l" ] );
    ( "absolute link out of sandbox",
      [ "write f x"; "mkdir d"; "symlink @f d/l"; "cd d"; "read l"; "stat l" ]
    );
    ( "link out of sandbox",
      [ "write f x"; "mkdir d"; "symlink ../f d/l"; "cd d"; "read l"; "stat l" ]
    );
    (* A link to a directory, which Windows makes a directory. *)
    ( "rmdir dir link",
      [ "mkdir d"; "write d/f x"; "symlink d l"; "rmdir l"; "ls ."; "ls d" ] );
    ("unlink dir link", [ "mkdir d"; "symlink d l"; "unlink l"; "ls ." ]);
    ( "link before dir",
      [ "symlink t l"; "mkdir t"; "write t/f x"; "stat l"; "ls l"; "read l/f" ]
    );
    ("link before file", [ "symlink t l"; "write t x"; "read l"; "stat l" ]);
    (* Renames over what exists, as eio#934 makes them on Windows. *)
    ( "rename onto file",
      [ "write a x"; "write b y"; "rename a b"; "read b"; "ls ." ] );
    ("rename onto empty dir", [ "mkdir d"; "mkdir e"; "rename d e"; "ls ." ]);
    ( "rename file onto empty dir",
      [ "write f x"; "mkdir e"; "rename f e"; "ls ." ] );
    ( "rename onto non-empty dir",
      [ "mkdir d"; "mkdir e"; "write e/f x"; "rename d e"; "ls ."; "ls e" ] );
    ("rename dir onto file", [ "mkdir d"; "write f x"; "rename d f"; "ls ." ]);
    (* What eio#932 fixed: kinds, and a read that must not create. *)
    ( "stat kinds",
      [
        "write f x";
        "mkdir d";
        "symlink f l";
        "symlink d m";
        "lstat l";
        "lstat m";
        "stat l";
        "stat m";
        "stat d";
        "open n r";
        "ls .";
      ] );
    (* eio#819: mkdirs under a parent that exists. *)
    ("mkdirs", [ "mkdir d"; "mkdirs d/e"; "mkdirs x/y"; "ls d"; "ls x" ]);
  ]

let repeat word =
  match (String.index_opt word '{', String.index_opt word '}') with
  | Some i, Some j when i < j ->
      let s = String.sub word (i + 1) (j - i - 1) in
      let n =
        int_of_string (String.sub word (j + 1) (String.length word - j - 1))
      in
      String.sub word 0 i ^ String.concat "" (List.init n (fun _ -> s))
  | _ -> word

exception Bad_op of string

let absolute ?root word =
  if String.length word = 0 || word.[0] <> '@' then word
  else
    match root with
    | Some root ->
        String.concat Filename.dir_sep
          (root
          :: String.split_on_char '/'
               (String.sub word 1 (String.length word - 1)))
    | None -> raise (Bad_op word)

let unescape word =
  if String.contains word '\\' then Scanf.unescaped word else word

let expand ?root word =
  if word = "-" then "" else absolute ?root (unescape (repeat word))

let words ?root op = List.map (expand ?root) (String.split_on_char ' ' op)
let key ops = String.concat ";" (List.map String.escaped ops)

(* An error, by its errno: by its Eio class, else by the strerror text both
   POSIX backends print, or by the Win32 error eio_windows prints. *)
let errnos =
  [
    ("not a directory", "ENOTDIR");
    ("is a directory", "EISDIR");
    ("directory not empty", "ENOTEMPTY");
    ("invalid argument", "EINVAL");
    ("busy", "EBUSY");
    ("name too long", "ENAMETOOLONG");
    ("illegal byte sequence", "EILSEQ");
    ("not supported", "ENOTSUP");
    (* Windows' messages, by the Win32 error that names them. *)
    ("volume label syntax is incorrect", "ERROR_INVALID_NAME");
    ("the directory name is invalid", "ERROR_DIRECTORY");
    ("reparse point buffer is invalid", "ERROR_INVALID_REPARSE_DATA");
  ]

let contains ~sub s =
  let n = String.length sub and m = String.length s in
  let rec go i = i + n <= m && (String.sub s i n = sub || go (i + 1)) in
  go 0

let errno_of_message s =
  let s = String.lowercase_ascii s in
  List.find_map
    (fun (sub, e) -> if contains ~sub s then Some e else None)
    errnos

let errno exn =
  match exn with
  | Eio.Io (Eio.Fs.E (Not_found _), _) -> "ENOENT"
  | Eio.Io (Eio.Fs.E (Already_exists _), _) -> "EEXIST"
  | Eio.Io (Eio.Fs.E (Permission_denied _), _) -> "denied"
  | Eio.Io (Eio.Fs.E Symlink, _) -> "ELOOP"
  | Eio.Io (Eio.Fs.E File_too_large, _) -> "EFBIG"
  | Eio.Io (Eio.Exn.Not_available _, _) -> "ENOTSUP"
  | Invalid_argument _ -> "invalid_argument"
  | End_of_file -> "eof"
  | exn -> (
      let s = Printexc.to_string exn in
      match errno_of_message s with Some e -> e | None -> "exn " ^ s)

let perm s = int_of_string ("0o" ^ s)

let kind (st : Eio.File.Stat.t) =
  match st.kind with
  | `Regular_file -> Fmt.str "file %Ld" (Optint.Int63.to_int64 st.size)
  | `Directory -> "dir"
  | `Symbolic_link -> "link"
  | _ -> "other"

let names l = "[" ^ String.concat "/" (List.map String.escaped l) ^ "]"

let open_ dir p flags =
  let has c = String.contains flags c in
  let follow = not (has 'n') in
  Eio.Switch.run @@ fun sw ->
  if has 'r' then ignore (Eio.Path.open_in ~sw ~follow Eio.Path.(dir / p))
  else
    let create =
      if has 'x' then `Exclusive 0o644
      else if has 't' then `Or_truncate 0o644
      else if has 'c' then `If_missing 0o644
      else `Never
    in
    ignore
      (Eio.Path.open_out ~sw ~follow ~append:(has 'a') ~create
         Eio.Path.(dir / p))

let chain dir n =
  if not (Eio.Path.is_file Eio.Path.(dir / "f")) then
    Eio.Path.save ~create:(`Exclusive 0o644) Eio.Path.(dir / "f") "x";
  for i = 1 to n do
    let target = if i = 1 then "f" else Fmt.str "l%d" (i - 1) in
    Eio.Path.symlink ~link_to:target Eio.Path.(dir / Fmt.str "l%d" i)
  done

let op ?root ~sw ids ~base ~held line =
  let hold p close = held := (p, close) :: !held in
  let ( / ) = Eio.Path.( / ) in
  let dir = !base in
  let ok () = "ok" in
  match words ?root line with
  | [ "write"; p; data ] ->
      Eio.Path.save ~create:(`Or_truncate 0o644) (dir / p) data;
      ok ()
  | [ "read"; p ] -> String.escaped (Eio.Path.load (dir / p))
  | [ "readd"; p ] ->
      Eio.Switch.run @@ fun sw ->
      let f = Eio.Path.open_in ~sw (dir / p) in
      let buf = Cstruct.create 16 in
      string_of_int (Eio.Flow.single_read f buf)
  | [ "pwa"; p; data; off ] ->
      Eio.Switch.run @@ fun sw ->
      let f = Eio.Path.open_out ~sw ~append:true ~create:`Never (dir / p) in
      Eio.File.pwrite_all f
        ~file_offset:(Optint.Int63.of_string off)
        [ Cstruct.of_string data ];
      ok ()
  | [ "open"; p; flags ] ->
      open_ dir p flags;
      ok ()
  | [ "opendir"; p ] ->
      Eio.Path.with_open_dir (dir / p) ignore;
      ok ()
  | [ "lsd"; p ] ->
      Eio.Path.with_open_dir (dir / p) (fun d -> names (Eio.Path.read_dir d))
  | [ "mkdir"; p ] ->
      Eio.Path.mkdir ~perm:0o755 (dir / p);
      ok ()
  | [ "mkdir"; p; m ] ->
      Eio.Path.mkdir ~perm:(perm m) (dir / p);
      ok ()
  | [ "unlink"; p ] ->
      Eio.Path.unlink (dir / p);
      ok ()
  | [ "rmdir"; p ] ->
      Eio.Path.rmdir (dir / p);
      ok ()
  | [ "rename"; a; b ] ->
      Eio.Path.rename (dir / a) (dir / b);
      ok ()
  | [ "symlink"; target; p ] ->
      Eio.Path.symlink ~link_to:target (dir / p);
      ok ()
  | [ "readlink"; p ] -> String.escaped (Eio.Path.read_link (dir / p))
  | [ "stat"; p ] -> kind (Eio.Path.stat ~follow:true (dir / p))
  | [ "lstat"; p ] -> kind (Eio.Path.stat ~follow:false (dir / p))
  | [ "mode"; p ] -> Fmt.str "%o" (Eio.Path.stat ~follow:false (dir / p)).perm
  | [ "group"; p ] ->
      let g = Int64.to_int (Eio.Path.stat ~follow:false (dir / p)).gid in
      if g = ids.gid then "primary"
      else if g = ids.other_gid then "other"
      else "foreign"
  | [ "chmod"; p; m ] ->
      Eio.Path.chmod ~follow:true ~perm:(perm m) (dir / p);
      ok ()
  | [ "lchmod"; p; m ] ->
      Eio.Path.chmod ~follow:false ~perm:(perm m) (dir / p);
      ok ()
  | [ "chgrp"; p ] ->
      Eio.Path.chown ~follow:false ~gid:(Int64.of_int ids.other_gid) (dir / p);
      ok ()
  | [ "ls"; p ] -> names (Eio.Path.read_dir (dir / p))
  | [ "chain"; n ] ->
      chain dir (int_of_string n);
      ok ()
  | [ "hold"; p ] ->
      let f = Eio.Path.open_in ~sw (dir / p) in
      hold p (fun () -> Eio.Resource.close f);
      ok ()
  | [ "holddir"; p ] ->
      let d = Eio.Path.open_dir ~sw (dir / p) in
      hold p (fun () -> Eio.Resource.close (fst d));
      ok ()
  | [ "close"; p ] -> (
      match List.assoc_opt p !held with
      | None -> raise (Bad_op line)
      | Some close ->
          held := List.remove_assoc p !held;
          close ();
          ok ())
  | [ "mkdirs"; p ] ->
      Eio.Path.mkdirs ~perm:0o755 (dir / p);
      ok ()
  | [ "cd"; p ] ->
      base := (Eio.Path.open_dir ~sw (dir / p) :> Eio.Fs.dir_ty Eio.Path.t);
      ok ()
  | _ -> raise (Bad_op line)

let run ?root ids dir ops =
  Eio.Switch.run @@ fun sw ->
  let base = ref (dir :> Eio.Fs.dir_ty Eio.Path.t) and held = ref [] in
  List.map
    (fun line ->
      try op ?root ~sw ids ~base ~held line with
      | (Bad_op _ | Eio.Cancel.Cancelled _) as e -> raise e
      | exn -> errno exn)
    ops
