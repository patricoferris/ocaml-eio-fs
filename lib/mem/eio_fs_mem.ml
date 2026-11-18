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
open Eio_fs_mem_core

type user = Low_level.User.t = {
  uid : int64;
  gid : int64;
  groups : int64 list;
  umask : int;
}

let root = Low_level.User.root
let v = Memfs.v
let as_user = Memfs.as_user
let load = Memfs.load

type stdenv = < fs : Eio.Fs.dir_ty Eio.Path.t ; cwd : Eio.Fs.dir_ty Eio.Path.t >

(* Root owns the tree and makes the current directory, which it gives to
   [user]. *)
let run ?clock ?(cwd = "/") ?(user = root) ?os fn =
  if not (Eio_fs_path.is_abs cwd) then
    Fmt.invalid_arg "Eio_fs_mem.run: cwd is %S, must be an absolute path" cwd;
  let t = Low_level.v ?clock ~cwd ?os () in
  let admin = (Eio.Path.of_dir (Memfs.fs t) :> Eio.Fs.dir_ty Eio.Path.t) in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(admin / cwd);
  if not (Int64.equal user.uid 0L) then
    Eio.Path.chown ~follow:true ~uid:user.uid ~gid:user.gid
      Eio.Path.(admin / cwd);
  let fs = (Eio.Path.of_dir (Memfs.fs ~user t) :> Eio.Fs.dir_ty Eio.Path.t) in
  let cwd = (Eio.Path.of_dir (Memfs.cwd ~user t) :> Eio.Fs.dir_ty Eio.Path.t) in
  (* Resolve the current directory now: it then follows its directory. *)
  ignore (Eio.Path.stat ~follow:true cwd : Eio.File.Stat.t);
  fn
    object
      method fs = fs
      method cwd = cwd
    end
