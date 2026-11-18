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

type entry =
  | Dir of { path : string; perm : int }
  | File of { path : string; perm : int; contents : string }
  | Symlink of { path : string; target : string }

let child parent name =
  if String.ends_with ~suffix:"/" parent then parent ^ name
  else parent ^ "/" ^ name

(* Opened without following and checked again once open, so what is read is
   the regular file the stat saw, not whatever took its name in between. *)
let contents (stat : Eio.File.Stat.t) path =
  Eio.Path.with_open_in ~follow:false path @@ fun file ->
  let opened = Eio.File.stat file in
  if opened.kind <> `Regular_file || not (Int64.equal opened.ino stat.ino) then
    Fmt.kstr Low_level.Error.err "scan: changed while it was read: %a"
      Eio.Path.pp path;
  Eio.Flow.read_all file

let scan ?filter ~at root =
  let at = if String.starts_with ~prefix:"/" at then at else "/" ^ at in
  (match Eio.Path.kind ~follow:true root with
  | `Directory -> ()
  | _ -> Low_level.Error.err ("scan: not a directory: " ^ snd root));
  let entries = ref [] in
  let record names (stat : Eio.File.Stat.t) p =
    let path = List.fold_left child at names in
    let entry =
      match stat.kind with
      | `Directory -> Some (Dir { path; perm = stat.perm })
      | `Regular_file ->
          Some (File { path; perm = stat.perm; contents = contents stat p })
      | `Symbolic_link -> Some (Symlink { path; target = Eio.Path.read_link p })
      | _ ->
          Eio.traceln "Skipping %a" Eio.Path.pp p;
          None
    in
    Option.iter (fun e -> entries := (names, e) :: !entries) entry
  in
  Eio_fs_internal.walk ~follow:true ?filter
    (fun names p stat -> record names stat p)
    root;
  (* Names compared one by one, a shorter list first, is depth-first order
     with sorted names. *)
  List.sort (fun (a, _) (b, _) -> List.compare String.compare a b) !entries
  |> List.map snd

(* Each directory takes its mode once everything is made in it. *)
let load ?os ?clock entries =
  let t = Low_level.v ?os ?clock () in
  let fs = Memfs.fs t in
  let mem path = (fs, path) in
  let modes =
    List.fold_left
      (fun modes -> function
        | Dir { path; perm } ->
            if path <> "/" then Eio.Path.mkdirs ~exists_ok:true ~perm (mem path);
            (path, perm) :: modes
        | File { path; perm; contents } ->
            Low_level.embed
              { at = Low_level.Dir.Fs t; user = Low_level.User.root }
              ~perm path ~contents;
            modes
        | Symlink { path; target } ->
            Eio.Path.symlink ~link_to:target (mem path);
            modes)
      [] entries
  in
  List.iter
    (fun (path, perm) -> Eio.Path.chmod ~follow:false ~perm (mem path))
    modes;
  (fs, "/")
