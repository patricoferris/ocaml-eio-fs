(*
 * Copyright (C) 2023 Thomas Leonard
 * Copyright (C) 2026 Memfs developers
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)
open! Hermetic

type user = Low_level.User.t = {
  uid : int64;
  gid : int64;
  groups : int64 list;
  umask : int;
}

let root = Low_level.User.root

module Nofollow = struct
  type error = [ `Invalid of string | `Too_large ]

  let read_file ~max_bytes ~validate path =
    if max_bytes < 0 || max_bytes = max_int then
      Fmt.invalid_arg
        "Eio_fs.Nofollow.read_file: max_bytes is %d, must be between 0 and \
         max_int - 1"
        max_bytes;
    Eio.Switch.run @@ fun sw ->
    let file = Eio.Path.open_in ~sw ~follow:false path in
    Eio_fs_internal.bounded_read ~max_bytes ~validate ~stat:(Eio.File.stat file)
      ~read:(Eio.Flow.single_read file)

  (* Once opened, a file may have lost its last name to a rename over it,
     which leaves the descriptor reading the file it opened. *)
  let refused ~opened (s : Eio.File.Stat.t) =
    match s.kind with
    | `Regular_file when Int64.equal s.nlink 1L -> None
    | `Regular_file when opened && Int64.equal s.nlink 0L -> None
    | `Regular_file -> Fmt.kstr Option.some "a file of %Ld links" s.nlink
    | kind ->
        Fmt.kstr Option.some "a %a, not a regular file" Eio.File.Stat.pp_kind
          kind

  let with_regular path f =
    match refused ~opened:false (Eio.Path.stat ~follow:false path) with
    | Some why -> Error (`Invalid why)
    | None -> (
        Eio.Path.with_open_in ~follow:false path @@ fun file ->
        let stat = Eio.File.stat file in
        match refused ~opened:true stat with
        | Some why -> Error (`Invalid why)
        | None -> Ok (f file stat))
end

module Tree = struct
  type usage = {
    allocated_bytes : int64;
    apparent_bytes : int64;
    files : int;
    latest_mtime : float;
  }

  let add_bytes a b =
    if Int64.sub Int64.max_int a < b then Int64.max_int else Int64.add a b

  let bytes_of_blocks blocks =
    if blocks <= 0L then 0L
    else if blocks > Int64.div Int64.max_int 512L then Int64.max_int
    else Int64.mul blocks 512L

  (* Only a regular file has a length a reader can act on: a directory's [size]
     is the space its entries take and a symlink's is its target. *)
  let own stat =
    let regular = stat.Eio.File.Stat.kind = `Regular_file in
    {
      allocated_bytes = bytes_of_blocks stat.Eio.File.Stat.blocks;
      apparent_bytes =
        (if regular then Optint.Int63.to_int64 stat.Eio.File.Stat.size else 0L);
      files = (if regular then 1 else 0);
      latest_mtime = stat.mtime;
    }

  let add a b =
    {
      allocated_bytes = add_bytes a.allocated_bytes b.allocated_bytes;
      apparent_bytes = add_bytes a.apparent_bytes b.apparent_bytes;
      files = a.files + b.files;
      latest_mtime = Float.max a.latest_mtime b.latest_mtime;
    }

  let empty =
    { allocated_bytes = 0L; apparent_bytes = 0L; files = 0; latest_mtime = 0. }

  (* [f] on [path] and its stat, then the entries below it when it is a
     directory. *)
  let visit f path =
    let stat = Eio.Path.stat ~follow:false path in
    f path stat;
    if stat.kind <> `Directory then []
    else
      Eio.Path.read_dir path
      |> List.rev_map (fun name -> Eio.Path.(path / name))

  let batch f paths =
    List.fold_left
      (fun next path -> List.rev_append (visit f path) next)
      [] paths

  (* Partition the level first so the number of fibers is bounded structurally;
     each worker then performs its share sequentially. *)
  let level ~max_fibers f paths =
    let worker_count = Int.min max_fibers (List.length paths) in
    let work = Array.make worker_count [] in
    List.iteri
      (fun i path ->
        let worker = i mod worker_count in
        work.(worker) <- path :: work.(worker))
      paths;
    match Array.to_list work with
    | [ paths ] -> batch f paths
    | batches -> List.concat (Eio.Fiber.List.map (batch f) batches)

  let walk ~fn ~max_fibers f path =
    if max_fibers <= 0 then
      Fmt.invalid_arg "%s: max_fibers is %d, must be positive" fn max_fibers;
    let rec loop = function
      | [] -> ()
      | paths -> loop (level ~max_fibers f paths)
    in
    loop [ path ]

  let iter ?(max_fibers = 32) f path =
    walk ~fn:"Eio_fs.Tree.iter" ~max_fibers f path

  let rec grant_owner p =
    match Eio.Path.stat ~follow:false p with
    | { kind = `Directory; perm; _ } -> (
        (if perm land 0o700 <> 0o700 then
           try Eio.Path.chmod ~follow:false ~perm:(perm lor 0o700) p
           with Eio.Io _ -> ());
        match Eio.Path.read_dir p with
        | names -> List.iter (fun n -> grant_owner Eio.Path.(p / n)) names
        | exception Eio.Io _ -> ())
    | _ | (exception Eio.Io _) -> ()

  let rec revoke_writes p =
    let stat = Eio.Path.stat ~follow:false p in
    if stat.kind = `Directory then
      List.iter (fun n -> revoke_writes Eio.Path.(p / n)) (Eio.Path.read_dir p);
    if stat.kind <> `Symbolic_link && stat.perm land 0o222 <> 0 then
      Eio.Path.chmod ~follow:false ~perm:(stat.perm land lnot 0o222) p

  let usage ?(max_fibers = 32) path =
    let total = ref empty in
    walk ~fn:"Eio_fs.Tree.usage" ~max_fibers
      (fun _ stat -> total := add !total (own stat))
      path;
    !total

  (* The native spelling names a host file; [Eio.Path.pp] renders through the
     capability label, which is all an in-memory path has. *)
  let name path =
    match Eio.Path.native path with
    | Some native -> native
    | None -> Fmt.to_to_string Eio.Path.pp path

  let same_file a b =
    let stat path =
      match Eio.Path.stat ~follow:true path with
      | stat -> Some stat
      | exception Eio.Io (Eio.Fs.E (Not_found _), _) -> None
    in
    match (stat a, stat b) with
    | Some a, Some b -> Int64.equal a.dev b.dev && Int64.equal a.ino b.ino
    | _ -> false

  let entries dir =
    match Eio.Path.read_dir dir with
    | names -> names
    | exception Eio.Io (Eio.Fs.E (Not_found _), _) -> []

  (* A failed rmdir(2) is read again rather than by its errno, which POSIX
     lets be EEXIST or ENOTEMPTY for a directory that holds anything. *)
  let rec remove_empty = function
    | [] -> Ok ()
    | d :: rest -> (
        match Eio.Path.rmdir d with
        | () -> remove_empty rest
        | exception (Eio.Io _ as e) -> (
            match Eio.Path.read_dir d with
            | _ :: _ -> Ok ()
            | exception Eio.Io (Eio.Fs.E (Not_found _), _) -> Ok ()
            | [] | (exception Eio.Io _) -> Error (d, e)))

  let rec links dir rel =
    let at = Eio.Path.(dir / rel) in
    match Eio.Path.kind ~follow:false at with
    | `Symbolic_link -> Some [ (rel, Eio.Path.read_link at) ]
    | `Directory -> (
        let stem =
          if String.ends_with ~suffix:"/" rel then
            String.sub rel 0 (String.length rel - 1)
          else rel
        in
        match Eio.Path.read_dir at with
        | [] -> None
        | names ->
            List.fold_left
              (fun acc n ->
                Option.bind acc (fun acc ->
                    Option.map (List.append acc) (links dir (stem ^ "/" ^ n))))
              (Some []) names)
    | _ -> None

  exception Shared of string

  let fail_shared fmt = Fmt.kstr (fun why -> raise (Shared why)) fmt

  (* Each copy is made at a name of its own beside [spare], as the walk copies
     the files of a level in several fibers. *)
  let unshare ~spare path =
    let copies = Atomic.make 0 in
    let copy_apart file (stat : Eio.File.Stat.t) =
      let dir, stem = spare in
      let n = Atomic.fetch_and_add copies 1 + 1 in
      let spare = (dir, Fmt.str "%s.%d" stem n) in
      try
        Eio.Path.with_open_in file (fun src ->
            Eio.Path.with_open_out
              ~create:(`Exclusive (stat.perm land 0o777))
              spare
              (fun dst -> Eio.Flow.copy src dst));
        Eio.Path.rename spare file
      with Eio.Io _ as e ->
        fail_shared "%s, linked %Ld times, cannot be copied apart: %a"
          (name file) stat.nlink Eio.Exn.pp e
    in
    let shared (stat : Eio.File.Stat.t) =
      stat.kind = `Regular_file && Int64.compare stat.nlink 1L > 0
    in
    match iter (fun f stat -> if shared stat then copy_apart f stat) path with
    | () -> Ok ()
    | exception Shared why -> Error why

  type node = Node of string * node list

  (* Each child is drawn behind the guides of its ancestors: a branch, and
     below it a rule while siblings follow, as tree(1) draws them. *)
  let rec pp_children ppf guides = function
    | [] -> ()
    | [ child ] -> pp_child ppf guides "\u{2514}\u{2500}\u{2500} " "    " child
    | child :: rest ->
        pp_child ppf guides "\u{251C}\u{2500}\u{2500} " "\u{2502}   " child;
        pp_children ppf guides rest

  and pp_child ppf guides branch rule (Node (label, children)) =
    Fmt.pf ppf "@,%s%s%s" guides branch label;
    pp_children ppf (guides ^ rule) children

  let pp_tree ~depth =
    let name (_, p) = Low_level.last_component p in
    let rec node depth path =
      match Eio.Path.kind ~follow:false path with
      | `Regular_file -> Some (Node (name path, []))
      | `Directory ->
          let children =
            if depth <= 1 then []
            else
              Eio.Path.read_dir path
              |> List.filter_map (fun child ->
                  node (depth - 1) Eio.Path.(path / child))
          in
          Some (Node (name path, children))
      | `Symbolic_link ->
          Some (Node (name path ^ " -> " ^ Eio.Path.read_link path, []))
      | _ -> None
    in
    fun ppf path ->
      if depth > 0 then
        Option.iter
          (fun (Node (label, children)) ->
            Fmt.pf ppf "@[<v>%s%a@]" label
              (fun ppf -> pp_children ppf "")
              children)
          (node depth path)
end

module Fs = Memfs

module Crunch = struct
  type entry =
    | Dir of { path : string; perm : int }
    | File of { path : string; perm : int; contents : string }
    | Symlink of { path : string; target : string }

  type t = entry list

  let child parent name =
    if parent = "/" then "/" ^ name else parent ^ "/" ^ name

  let scan root =
    let entries = ref [] in
    let push entry = entries := entry :: !entries in
    let rec walk ~follow dir path =
      let stat = Eio.Path.stat ~follow dir in
      match stat.kind with
      | `Regular_file ->
          push (File { path; perm = stat.perm; contents = Eio.Path.load dir })
      | `Directory ->
          push (Dir { path; perm = stat.perm });
          Eio.Path.read_dir dir
          |> List.iter (fun name ->
              walk ~follow:false Eio.Path.(dir / name) (child path name))
      | `Symbolic_link ->
          push (Symlink { path; target = Eio.Path.read_link dir })
      | _ -> Eio.traceln "Skipping %a" Eio.Path.pp dir
    in
    (match Eio.Path.kind ~follow:true root with
    | `Directory -> ()
    | _ -> Low_level.Error.err ("scan: not a directory: " ^ snd root));
    walk ~follow:true root "/";
    List.rev !entries

  let find path entries =
    List.find_map
      (function
        | File f when String.equal f.path path -> Some f.contents | _ -> None)
      entries

  let load ?clock ?os entries =
    let t = Low_level.v ?clock ?os () in
    let fs = Fs.fs t in
    let mem path = (fs, path) in
    List.iter
      (function
        | Dir { path; perm } ->
            if path <> "/" then Eio.Path.mkdirs ~exists_ok:true ~perm (mem path);
            Eio.Path.chmod ~follow:false ~perm (mem path)
        | File { path; perm; contents } ->
            Low_level.embed
              { at = Low_level.Dir.Fs t; user = Low_level.User.root }
              ~perm path ~contents
        | Symlink { path; target } ->
            Eio.Path.symlink ~link_to:target (mem path))
      entries;
    (fs, "/")

  let source ppf entries =
    Fmt.pf ppf "(* Generated by eio-fs-crunch. Do not edit. *)@.@.";
    Fmt.pf ppf "let entries : Eio_fs.Crunch.t =@.";
    Fmt.pf ppf "  Eio_fs.Crunch.@.";
    Fmt.pf ppf "    [@.";
    List.iter
      (function
        | Dir { path; perm } ->
            Fmt.pf ppf "      Dir { path = %S; perm = 0o%03o };@." path perm
        | File { path; perm; contents } ->
            Fmt.pf ppf
              "      File { path = %S; perm = 0o%03o; contents = %S };@." path
              perm contents
        | Symlink { path; target } ->
            Fmt.pf ppf "      Symlink { path = %S; target = %S };@." path target)
      entries;
    Fmt.pf ppf "    ]@.@.";
    Fmt.pf ppf
      "let load ?clock ?os () = Eio_fs.Crunch.load ?clock ?os entries@."

  let interface =
    "(* Generated by eio-fs-crunch. Do not edit. *)\n\n\
     val entries : Eio_fs.Crunch.t\n\
     (** The embedded file tree. *)\n\n\
     val load :\n\
    \  ?clock:_ Eio.Time.clock ->\n\
    \  ?os:[< `Linux | `Macos ] ->\n\
    \  unit ->\n\
    \  [ `Close | Eio.Fs.dir_ty ] Eio.Path.t\n\
     (** [load ~clock ~os ()] is the root of a new in-memory filesystem holding\n\
    \    {!entries}, as {!Eio_fs.Crunch.load} makes it. *)\n"
end

type stdenv = < fs : Eio.Fs.dir_ty Eio.Path.t ; cwd : Eio.Fs.dir_ty Eio.Path.t >

(* Root owns the tree and makes the current directory, which it gives to
   [user]. *)
let run ?clock ?(cwd = "/") ?(user = root) ?os fn =
  if not (Eio_fs_path.is_abs cwd) then
    Fmt.invalid_arg "Eio_fs.run: cwd is %S, must be an absolute path" cwd;
  let t = Low_level.v ?clock ~cwd ?os () in
  let admin = (Eio.Path.of_dir (Fs.fs t) :> Eio.Fs.dir_ty Eio.Path.t) in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(admin / cwd);
  if not (Int64.equal user.uid 0L) then
    Eio.Path.chown ~follow:true ~uid:user.uid ~gid:user.gid
      Eio.Path.(admin / cwd);
  let fs = (Eio.Path.of_dir (Fs.fs ~user t) :> Eio.Fs.dir_ty Eio.Path.t) in
  let cwd = (Eio.Path.of_dir (Fs.cwd ~user t) :> Eio.Fs.dir_ty Eio.Path.t) in
  (* Resolve the current directory now: it then follows its directory. *)
  ignore (Eio.Path.stat ~follow:true cwd : Eio.File.Stat.t);
  fn
    object
      method fs = fs
      method cwd = cwd
    end
