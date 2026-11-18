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
open Eio.Std

type dir_fd = Low_level.Dir.fd

(* The [dir_fd] beneath an in-memory directory: a rename's target and
   [as_user] ask for it. *)
type (_, _, _) Eio.Resource.pi +=
  | Memfs_dir : ('t, 't -> dir_fd, [> `Memfs_dir ]) Eio.Resource.pi

(* A rename target that wraps an in-memory directory, as a Chaos directory
   does, is unwrapped here, inside the package, to the directory it wraps:
   the entry moves within the one tree, and the wrapper itself still hands
   its base to no caller. *)
let rec as_memfs_dir : 'a. 'a Eio.Resource.t -> dir_fd option =
 fun (Eio.Resource.T (t, ops) as dir) ->
  match Eio.Resource.get_opt ops Memfs_dir with
  | Some fn -> Some (fn t)
  | None -> Option.bind (Eio_fs_internal.base_dir dir) as_memfs_dir

module rec Dir : sig
  include Eio.Fs.Pi.DIR

  val v : label:string -> path:string -> dir_fd -> t
  val fd : t -> dir_fd
  val as_root : t -> t
  val close : t -> unit

  val rename_exclusive :
    t ->
    Eio_fs_sync.Rename.t ->
    source:string ->
    target:Eio_fs_sync.Rename.path ->
    unit
end = struct
  type t = {
    fd : dir_fd;
    dir_path : string;
    label : string;
    mutable root_closed : bool;
  }

  (* Descriptor state belongs to the descriptor, including switch release.
     Only descriptor-free root and cwd capabilities need their own flag. *)
  let closed t =
    match t.fd.at with
    | Fs _ | Cwd _ -> t.root_closed
    | Dir_fd fd -> Low_level.Fd.closed fd

  let fd t =
    if closed t then Low_level.Error.badf ();
    t.fd

  let close t =
    match t.fd.at with
    | Fs _ | Cwd _ -> t.root_closed <- true
    | Dir_fd fd -> Low_level.close fd

  let v ~label ~path:dir_path fd = { fd; dir_path; label; root_closed = false }
  let as_root t = { t with fd = { t.fd with user = Low_level.User.root } }

  (* [~follow:false] refuses a symlink at [path] itself, as O_NOFOLLOW. *)
  let nofollow ~follow flags =
    if follow then flags else Low_level.Open_flags.(flags + o_nofollow)

  let open_in t ~sw ~follow path =
    let flags = nofollow ~follow Low_level.Open_flags.o_rdonly in
    let fd = Low_level.openat ~mode:0 ~sw (fd t) path flags in
    (Low_level.Flow.of_fd fd :> Eio.File.ro_ty Eio.Resource.t)

  let open_out t ~sw ~follow ~append ~create path =
    let mode, flags =
      match create with
      | `Never -> (0, Low_level.Open_flags.empty)
      | `If_missing perm -> (perm, Low_level.Open_flags.o_creat)
      | `Or_truncate perm -> (perm, Low_level.Open_flags.(o_creat + o_trunc))
      | `Exclusive perm -> (perm, Low_level.Open_flags.(o_creat + o_excl))
    in
    let flags =
      if append then Low_level.Open_flags.(flags + o_append) else flags
    in
    let flags = nofollow ~follow Low_level.Open_flags.(flags + o_rdwr) in
    let fd = Low_level.openat ~sw ~mode (fd t) path flags in
    (Low_level.Flow.of_fd fd :> Eio.File.rw_ty r)

  let mkdir t ~perm path = Low_level.mkdirat ~perm (fd t) path
  let unlink t path = Low_level.unlink (fd t) path
  let rmdir t path = Low_level.rmdir (fd t) path
  let stat t ~follow path = Low_level.statat ~follow (fd t) path
  let read_dir t path = Low_level.read_dir (fd t) path
  let read_link t path = Low_level.read_link (fd t) path
  let chmod t ~follow ~perm path = Low_level.chmod ~follow (fd t) ~perm path

  let chown ~follow ?uid ?gid t path =
    Low_level.chown ~follow ?uid ?gid (fd t) path

  let with_dir_entries t path fn =
    fn (Low_level.read_dir_entries (fd t) path |> List.to_seq)

  (* Volatile storage: the strongest barrier it has is nothing, once the
     directory opens read-only, as fsync(2) on Linux needs it to. *)
  let sync_dir t path =
    Eio.Switch.run @@ fun sw ->
    let fd =
      Low_level.openat ~sw ~mode:0 (fd t) path Low_level.Open_flags.o_rdonly
    in
    Low_level.close fd

  (* A directory of another backend is on another device: EXDEV, as
     [Low_level.rename] raises it between two in-memory filesystems. *)
  let target dir new_path =
    match as_memfs_dir dir with
    | None ->
        Low_level.Error.err ("rename: invalid cross-device link: " ^ new_path)
    | Some dir -> dir

  let rename t old_path (new_dir : [> Eio.Fs.dir_ty ] Eio.Std.r) new_path =
    let new_dir = target new_dir new_path in
    Low_level.rename (fd t) old_path new_dir new_path

  let rename_exclusive t _backend ~source ~target:(new_dir, new_path) =
    let new_dir = target new_dir new_path in
    Low_level.rename ~replace:false (fd t) source new_dir new_path

  let symlink ~link_to t path = Low_level.symlinkat ~link_to (fd t) path

  let open_dir t ~sw path =
    let flags = Low_level.Open_flags.(o_rdonly + o_directory) in
    let fd = Low_level.openat ~sw ~mode:0 (fd t) path flags in
    let label = Low_level.last_component path in
    let full_path = Eio_fs_path.append t.dir_path path in
    let d =
      v ~label ~path:full_path
        { at = Low_level.Dir.Dir_fd fd; user = t.fd.user }
    in
    (* The upcast drops the barrier tag from the static row to fit
       [Pi.DIR]; the operation itself stays probeable on the resource. *)
    (Eio.Resource.T (d, Handler.v) :> [ `Close | `Dir ] Eio.Resource.t)

  let open_subtree t ~sw path = open_dir t ~sw path
  let pp f t = Fmt.string f (String.escaped t.label)

  (* An in-memory tree has no native path: one would point a barrier or a
     lock at the host filesystem, where "./x" resolves against the process's
     cwd. *)
  let native t _ =
    ignore (fd t);
    None

  (* The in-memory tree names its entries the way the POSIX backend does,
     so it splits and joins paths by the same syntax. *)
  include Eio_utils.Posix_path
end

and Handler : sig
  val v :
    (Dir.t, [ `Dir | `Close | `Dir_sync | `Process_local ]) Eio.Resource.handler
end = struct
  let rec self =
    lazy
      (Eio.Resource.handler
         [
           H (Eio.Fs.Pi.Dir, (module Dir));
           H (Eio.Resource.Close, Dir.close);
           H (Memfs_dir, Dir.fd);
           (* The crash model reads and rebuilds a tree as root. *)
           H
             ( Eio_fs_internal.Admin,
               fun t ->
                 (Eio.Resource.T (Dir.as_root t, Lazy.force self)
                   :> Eio.Fs.dir_ty Eio.Resource.t) );
           H (Eio_fs_sync.Rename.Exclusive, Dir.rename_exclusive);
           H (Eio_fs_sync.Dir_sync, fun t ~path ~full:_ -> Dir.sync_dir t path);
           (* The tree lives in this process's heap. It answers no native path
           and hands out no descriptor, so nothing outside this process can
           reach a file of it: an exclusion between fibers here is the whole
           exclusion these files admit. *)
           H
             ( Eio_fs_sync.Process_local,
               fun t ->
                 ignore (Dir.fd t);
                 true );
         ])

  let v = Lazy.force self
end

let dir ~label ~path fd =
  (Eio.Resource.T (Dir.v ~label ~path fd, Handler.v)
    :> [ `Close | Eio.Fs.dir_ty ] Eio.Resource.t)

(* Full access to the file system [t]. *)
let fs ?(user = Low_level.User.root) t =
  dir ~label:"fs" ~path:"." { at = Fs t; user }

(* Access confined to the current directory of [t]. *)
let cwd ?(user = Low_level.User.root) t =
  dir ~label:"cwd" ~path:"." { at = Cwd t; user }

let v ?os ?(user = Low_level.User.root) ?clock () =
  (fs ~user (Low_level.v ?clock ~owner:user ?os ()), "/")

(* A wrapper is rebuilt over its base as [user]. *)
let as_user user ((d, path) : _ Eio.Path.t) =
  let label = Fmt.to_to_string Eio.Path.pp (d, "") in
  let rec go (Eio.Resource.T (t, ops) as r : Eio.Fs.dir_ty Eio.Resource.t) =
    match Eio.Resource.get_opt ops Memfs_dir with
    | Some fd -> dir ~label ~path:"." { (fd t) with user }
    | None -> (
        match Eio_fs_internal.rebase r with
        | Some rebase -> rebase go
        | None ->
            Fmt.invalid_arg
              "Eio_fs_mem.as_user: path is %a, not a directory of an in-memory \
               filesystem"
              Eio.Path.pp (d, path))
  in
  (go (d :> Eio.Fs.dir_ty Eio.Resource.t), path)

(* Looked up as root, as [set_times] is: the model asks about a file the
   wrapped user may reach only through a directory opened before. *)
let held ((d, path) : _ Eio.Path.t) =
  match as_memfs_dir d with
  | None -> None
  | Some fd -> (
      try Low_level.held { fd with user = Low_level.User.root } path
      with Eio.Io (Eio.Fs.E (Not_found _), _) -> None)

let descriptors ((d, _) : _ Eio.Path.t) =
  Option.map Low_level.descriptors (as_memfs_dir d)

let set_times ((d, path) : _ Eio.Path.t) ~atime ~mtime ~ctime =
  match as_memfs_dir d with
  | None -> ()
  | Some fd ->
      Low_level.set_times
        { fd with user = Low_level.User.root }
        path ~atime ~mtime ~ctime

(* The copy keeps each mode, which the umask narrowed. *)
let keep_mode fs (stat : Eio.File.Stat.t) (_, path) =
  Eio.Path.chmod ~follow:false ~perm:stat.perm (fs, path)

(* Opened without following and checked again once open, so what is copied
   is the regular file the stat saw, not whatever took its name in
   between. *)
let copy_regular fs (stat : Eio.File.Stat.t) v =
  Eio.Path.with_open_in ~follow:false v @@ fun i ->
  let opened = Eio.File.stat i in
  if opened.kind <> `Regular_file || opened.ino <> stat.ino then
    Fmt.kstr Low_level.Error.err "%a changed while it was copied" Eio.Path.pp v;
  Eio.Path.with_open_out ~create:(`If_missing stat.perm)
    (fs, snd v)
    (fun o -> Eio.Flow.copy i o);
  keep_mode fs stat v

(* The root is followed and must be a directory; the filter is asked about
   it before anything is made, and every entry below it is copied as it is,
   a link kept a link. *)
let load ?os ?clock ?(filter = fun _ -> true) path =
  let stat = Eio.Path.stat ~follow:true path in
  if stat.kind <> `Directory then
    Low_level.Error.err ("load: not a directory: " ^ snd path);
  let t = Low_level.v ?clock ?os () in
  let fs = fs t in
  let switch_filesystem (_, path) = (fs, path) in
  let keep_mode = keep_mode fs in
  let rec copy_dir (stat : Eio.File.Stat.t) v =
    Eio.Path.mkdirs ~exists_ok:true ~perm:stat.perm (switch_filesystem v);
    List.iter (fun name -> make_fs Eio.Path.(v / name)) (Eio.Path.read_dir v);
    keep_mode stat v
  and make_fs v =
    let stat = Eio.Path.stat ~follow:false v in
    match stat.kind with
    | `Regular_file -> if filter v then copy_regular fs stat v
    | `Directory -> if filter v then copy_dir stat v
    | `Symbolic_link ->
        if filter v then
          let link_to = Eio.Path.read_link v in
          Eio.Path.symlink ~link_to (switch_filesystem v)
    | _ -> Eio.traceln "Skipping %a" Eio.Path.pp v
  in
  if filter path then copy_dir stat path;
  (fs, "/")
