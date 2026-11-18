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

(* How a filesystem spells its paths: as POSIX does, or as Windows does,
   through Eio_utils.Nt_path, which eio_windows registers too, so that
   [Eio.Path.( / )], [split] and [native] answer as they do there. *)
module type SYNTAX = sig
  include Eio.Fs.Pi.PATH

  val native : dir_path:string -> string -> string option
  (** [native ~dir_path path] is the native spelling of [path] below the
      directory whose own is [dir_path]. *)

  val dir_path : parent:string -> Low_level.Fd.t -> string -> string
  (** [dir_path ~parent fd path] is the native spelling of the directory [fd],
      opened at [path] below [parent]. *)
end

module Posix_syntax = struct
  include Eio_utils.Posix_path

  (* An in-memory tree has no native path: one would point a barrier or a
     lock at the host filesystem, where "./x" resolves against the process's
     cwd. *)
  let native ~dir_path:_ _ = None
  let dir_path ~parent _ path = join parent path
end

module Nt_syntax = struct
  include Eio_utils.Nt_path

  (* Win32's [Filename.is_implicit]: neither absolute nor from [.] or
     [..]. *)
  let implicit p =
    is_relative p
    && not
         (List.exists
            (fun prefix -> String.starts_with ~prefix p)
            [ ".\\"; "./"; "..\\"; "../" ])

  (* As eio_windows spells it: a path below a directory from that
     directory's absolute path, the current directory's from [.]. *)
  let native ~dir_path path =
    if not (is_relative path) then Some path
    else
      let p = if String.equal dir_path "." then path else join dir_path path in
      Some
        (if String.equal p "" then "."
         else if String.equal p "." || not (implicit p) then p
         else ".\\" ^ p)

  let dir_path ~parent:_ fd _ = Low_level.win_path fd
end

module Make (P : SYNTAX) = struct
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

    let v ~label ~path:dir_path fd =
      { fd; dir_path; label; root_closed = false }

    let as_root t =
      { t with fd = { t.fd with user = Low_level.User.root; admin = true } }

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
     directory opens read-only, as fsync(2) on Linux needs it to. eio_windows
     syncs no directory, as Windows does not need it to. *)
    let sync_dir t path =
      if Low_level.windows (fd t) then ()
      else
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
      let label = Eio_fs_internal.last_component path in
      let full_path = P.dir_path ~parent:t.dir_path fd path in
      let d =
        v ~label ~path:full_path
          { at = Low_level.Dir.Dir_fd fd; user = t.fd.user; admin = t.fd.admin }
      in
      (* The upcast drops the barrier tag from the static row to fit
       [Pi.DIR]; the operation itself stays probeable on the resource. *)
      (Eio.Resource.T (d, Handler.v) :> [ `Close | `Dir ] Eio.Resource.t)

    let open_subtree t ~sw path = open_dir t ~sw path
    let pp f t = Fmt.string f (String.escaped t.label)

    let native t path =
      ignore (fd t);
      P.native ~dir_path:t.dir_path path

    let split = P.split
    let join = P.join
  end

  and Handler : sig
    val v :
      ( Dir.t,
        [ `Dir | `Close | `Dir_sync | `Process_local | `Modeless ] )
      Eio.Resource.handler
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
             H (Eio_fs_sync.Modeless, fun t -> Low_level.windows (Dir.fd t));
           ])

    let v = Lazy.force self
  end

  let dir ~label ~path fd =
    (Eio.Resource.T (Dir.v ~label ~path fd, Handler.v)
      :> [ `Close | Eio.Fs.dir_ty ] Eio.Resource.t)
end

module Posix = Make (Posix_syntax)
module Windows = Make (Nt_syntax)

let dir ~label ~path (fd : dir_fd) =
  if Low_level.windows fd then Windows.dir ~label ~path fd
  else Posix.dir ~label ~path fd

(* Full access to the file system [t]. *)
let fs ?(user = Low_level.User.root) t =
  dir ~label:"fs" ~path:"." { at = Fs t; user; admin = false }

(* Access confined to the current directory of [t]. *)
let cwd ?(user = Low_level.User.root) t =
  dir ~label:"cwd" ~path:"." { at = Cwd t; user; admin = false }

let v ?os ?(user = Low_level.User.root) ?clock ?drives () =
  let t = Low_level.v ?clock ~owner:user ?os ?drives () in
  (fs ~user t, Low_level.root_path t)

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
