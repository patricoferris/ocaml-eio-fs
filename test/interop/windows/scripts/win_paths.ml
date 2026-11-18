(* The Win32 paths paths.exe records through the unrestricted fs, and how:
   for each, its native spelling, the directory and name Eio.Path.split cuts
   it into, and the outcome of each operation, then what [win32] answers to
   a read of it. A path, and every answer, spells the current directory as
   <work>, its parent as <parent>, its drive as <drive>, the rest as <root>,
   and the directory on another drive as <other> and its name as
   <other-name>. Without one, a case that names it is not recorded. *)

type op = Write of string | Read | Mkdir | Rename of string | Ls

let cases =
  [
    ("absolute", {|<work>\a|}, [ Write "x"; Read ]);
    ("absolute with slashes", "<work>/a", [ Read ]);
    ("rooted", {|<root>\b|}, [ Write "x"; Read ]);
    ("drive-relative", "<drive>c", [ Write "x"; Read ]);
    ("verbatim trailing dot", {|\\?\<work>\d.|}, [ Write "x"; Read ]);
    ("trailing dot", {|<work>\d.|}, [ Read ]);
    ("trailing dot trimmed", {|<work>\d|}, [ Read ]);
    ("verbatim reserved", {|\\?\<work>\NUL|}, [ Write "y"; Read ]);
    ("reserved", {|<work>\NUL|}, [ Read ]);
    ("reserved with an extension", {|<work>\com1.txt|}, [ Read ]);
    ("listing", "<work>", [ Ls ]);
    ( "rename a directory across drives",
      {|<work>\e|},
      [ Mkdir; Rename {|<other>\e|} ] );
    ( "rename a file across drives",
      {|<work>\f|},
      [ Write "z"; Rename {|<other>\f|} ] );
    ("other drive", "<other>", [ Ls ]);
  ]

let replace ~sub ~by s =
  let n = String.length sub in
  let b = Buffer.create (String.length s) in
  let rec go i =
    if i >= String.length s then ()
    else if n > 0 && i + n <= String.length s && String.sub s i n = sub then (
      Buffer.add_string b by;
      go (i + n))
    else (
      Buffer.add_char b s.[i];
      go (i + 1))
  in
  go 0;
  Buffer.contents b

let error exn =
  let cls =
    match exn with
    | Eio.Io (Eio.Fs.E (Not_found _), _) -> "ENOENT"
    | Eio.Io (Eio.Fs.E (Already_exists _), _) -> "EEXIST"
    | Eio.Io (Eio.Fs.E (Permission_denied _), _) -> "denied"
    | _ -> "exn"
  in
  cls ^ ": " ^ Printexc.to_string exn

let header =
  [ "case"; "path"; "native"; "split dir"; "split name"; "outcomes"; "stdlib" ]

let rows ~fs ~work ?other ?(win32 = fun _ -> "-") () =
  let drive = String.sub work 0 2 in
  let root = String.sub work 2 (String.length work - 2) in
  let spell s =
    replace ~sub:"<work>" ~by:work s
    |> replace ~sub:"<other>" ~by:(Option.value ~default:"<other>" other)
    |> replace ~sub:"<root>" ~by:root
    |> replace ~sub:"<drive>" ~by:drive
  and unspell s =
    (* A message prints a path escaped, as an OCaml string. *)
    let un s (sub, by) =
      replace ~sub ~by s |> replace ~sub:(String.escaped sub) ~by
    in
    let other =
      match other with
      | Some o ->
          [ (o, "<other>"); (Eio_utils.Nt_path.basename o, "<other-name>") ]
      | None -> []
    in
    List.fold_left un s
      (((work, "<work>") :: other)
      @ [ (Eio_utils.Nt_path.dirname work, "<parent>"); (root, "<root>") ])
    |> String.map (function '\n' | '\r' | '|' -> ' ' | c -> c)
  in
  let ( / ) = Eio.Path.( / ) in
  let names_other (_, path, ops) =
    let has s = replace ~sub:"<other>" ~by:"" s <> s in
    has path || List.exists (function Rename q -> has q | _ -> false) ops
  in
  List.map
    (fun ((case, path, ops) as c) ->
      if other = None && names_other c then
        case :: path :: List.init 5 (fun _ -> "not recorded")
      else
        let p = fs / spell path in
        let native = Option.value ~default:"none" (Eio.Path.native p) in
        let dir, name =
          match Eio.Path.split p with
          | Some ((_, d), n) -> (d, n)
          | None -> ("none", "none")
        in
        let outcome op =
          try
            match op with
            | Write data ->
                Eio.Path.save ~create:(`Or_truncate 0o644) p data;
                "ok"
            | Read -> String.escaped (Eio.Path.load p)
            | Mkdir ->
                Eio.Path.mkdir ~perm:0o755 p;
                "ok"
            | Rename q ->
                Eio.Path.rename p (fs / spell q);
                "ok"
            | Ls ->
                "["
                ^ String.concat "/"
                    (List.sort String.compare (Eio.Path.read_dir p))
                ^ "]"
          with Eio.Io _ as exn -> error exn
        in
        let win32 = function Read -> win32 (spell path) | _ -> "-" in
        let outcomes = List.map (fun op -> unspell (outcome op)) ops in
        let win32 = List.map (fun op -> unspell (win32 op)) ops in
        [
          case;
          path;
          unspell native;
          unspell dir;
          unspell name;
          String.concat "|" outcomes;
          String.concat "|" win32;
        ])
    cases
